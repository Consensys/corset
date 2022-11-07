use cached::SizedCache;
use color_eyre::owo_colors::OwoColorize;
use indicatif::{ProgressBar, ProgressStyle};
use itertools::Itertools;
use rayon::prelude::*;
use std::collections::HashSet;
use tabled::{builder::Builder, object::Columns, ModifyObject, Style};

use eyre::*;
use log::*;
use pairing_ce::{bn256::Fr, ff::Field};

use crate::{
    column::ColumnSet,
    compiler::{Constraint, ConstraintSet, EvalSettings, Expression, Handle},
    pretty::*,
};

fn fail(expr: &Expression, i: isize, l: Option<usize>, columns: &ColumnSet<Fr>) -> Result<()> {
    let trace_span: isize = crate::SETTINGS.get().unwrap().trace_span;

    let mut builder = Builder::default();
    let module = expr.dependencies().iter().next().unwrap().module.clone();
    let handles = if crate::SETTINGS.get().unwrap().full_trace {
        columns
            .cols
            .get(&module)
            .unwrap()
            .keys()
            .map(|name| Handle::new(&module, &name))
            .sorted_by_key(|h| h.name.clone())
            .collect::<Vec<_>>()
    } else {
        expr.dependencies().iter().cloned().collect::<Vec<_>>()
    };
    for handle in handles.into_iter() {
        builder.add_record(
            vec![handle.to_string()]
                .into_iter()
                .chain(((i - trace_span).max(0)..=i + trace_span).map(|i| {
                    columns
                        .get(&handle)
                        .unwrap()
                        .get(i, false)
                        .map(|x| x.pretty())
                        .unwrap_or_else(|| "nil".into())
                }))
                .collect::<Vec<_>>(),
        );
    }

    builder.set_columns(
        vec![String::new()]
            .into_iter()
            .chain(((i - trace_span).max(0)..=i + trace_span).map(|i| i.to_string()))
            .collect::<Vec<_>>(),
    );
    let mut table = builder.build();
    table
        .with(
            Columns::single(trace_span as usize + 1)
                .modify()
                .with(|s: &str| s.red().to_string()),
        )
        .with(Style::blank());
    eprintln!("\n\n{}\n", table);

    let r = expr.eval(
        i,
        &mut |handle, i, wrap| {
            columns
                .get(handle)
                .ok()
                .and_then(|c| c.get(i, wrap))
                .cloned()
        },
        &mut None,
        &EvalSettings::new().trace(true),
    );

    Err(eyre!(
        "{}|{}{}\n -> {}",
        expr.pretty(),
        i,
        l.map(|l| format!("/{}", l)).unwrap_or_default(),
        r.as_ref()
            .map(Pretty::pretty)
            .unwrap_or_else(|| "nil".to_owned()),
    ))
}

fn check_constraint_at(
    expr: &Expression,
    i: isize,
    l: Option<usize>,
    columns: &ColumnSet<Fr>,
    fail_on_oob: bool,
    cache: &mut Option<SizedCache<Fr, Fr>>,
) -> Result<()> {
    let r = expr.eval(
        i,
        &mut |handle, i, wrap| columns._cols[handle.id.unwrap()].get(i, wrap).cloned(),
        cache,
        &Default::default(),
    );
    if let Some(r) = r {
        if !r.is_zero() {
            return fail(expr, i, l, columns);
        }
    } else if fail_on_oob {
        return fail(expr, i, l, columns);
    }
    Ok(())
}

fn check_constraint(
    expr: &Expression,
    domain: &Option<Vec<isize>>,
    columns: &ColumnSet<Fr>,
    name: &str,
) -> Result<()> {
    let cols_lens = expr
        .dependencies()
        .into_iter()
        .map(|handle| {
            columns
                .get(&handle)
                .with_context(|| eyre!("can not find column `{}`", handle))
                .map(|c| c.len())
        })
        .collect::<Result<Vec<_>>>()?;
    // Early exit if all the columns are empty: the module is not triggered
    // Ideally, this should be an `all` rather than an `any`, but the IC
    // pushes columns that will always be filled.
    if cols_lens.iter().any(|l| l.is_none()) {
        debug!("Skipping constraint `{}` with empty columns", name);
        return Ok(());
    }
    if !cols_lens
        .iter()
        .all(|&l| l.unwrap_or_default() == cols_lens[0].unwrap_or_default())
    {
        error!(
            "all columns are not of the same length:\n{}",
            expr.dependencies()
                .iter()
                .map(|handle| format!(
                    "\t{}: {}",
                    handle,
                    columns
                        .get(handle)
                        .unwrap()
                        .len()
                        .map(|x| x.to_string())
                        .unwrap_or_else(|| "nil".into())
                ))
                .collect::<Vec<_>>()
                .join("\n")
        );
    }
    let l = cols_lens[0].unwrap_or(0);
    if l == 0 {
        return Err(eyre!("empty trace, aborting"));
    }

    let mut cache = Some(cached::SizedCache::with_size(200000)); // ~1.60MB cache
    match domain {
        Some(is) => {
            for i in is {
                check_constraint_at(expr, *i, None, columns, true, &mut cache)?;
            }
        }
        None => {
            for i in 0..l as isize {
                check_constraint_at(expr, i, Some(l), columns, false, &mut cache)?;
            }
        }
    };
    Ok(())
}

pub fn check(cs: &ConstraintSet, only: &Option<Vec<String>>, with_bar: bool) -> Result<()> {
    if cs.modules.is_empty() {
        return Ok(());
    }

    let bar = if with_bar {
        Some(
            ProgressBar::new(cs.constraints.len() as u64).with_style(
                ProgressStyle::default_bar()
                    .template("Validating {msg} {bar:40} {pos}/{len}")
                    .unwrap()
                    .progress_chars("##-"),
            ),
        )
    } else {
        None
    };
    let failed = cs
        .constraints
        .par_iter()
        .with_max_len(1)
        .filter(|c| {
            only.as_ref()
                .map(|o| o.contains(&c.name().to_string()))
                .unwrap_or(true)
        })
        .inspect(|_| {
            if let Some(b) = &bar {
                b.inc(1)
            }
        })
        .filter_map(|c| {
            match c {
                Constraint::Vanishes { name, domain, expr } => {
                    if name == "INV_CONSTRAINTS" {
                        return None;
                    }
                    if matches!(**expr, Expression::Void) {
                        // warn!("Ignoring Void expression {}", name);
                        return None;
                    }

                    match expr.as_ref() {
                        Expression::List(es) => {
                            for e in es {
                                if let Err(err) = check_constraint(e, domain, &cs.modules, name) {
                                    error!("{}", err);
                                    return Some(name.to_owned());
                                }
                            }
                            None
                        }
                        _ => {
                            if let Err(err) = check_constraint(expr, domain, &cs.modules, name) {
                                error!("{}", err);
                                Some(name.to_owned())
                            } else {
                                None
                            }
                        }
                    }
                }
                Constraint::Plookup(_, _, _) => {
                    // warn!("Plookup validation not yet implemented");
                    None
                }
                Constraint::Permutation(_name, _from, _to) => {
                    // warn!("Permutation validation not yet implemented");
                    None
                }
                Constraint::InRange(_, _e, _range) => {
                    // warn!("Range validation not yet implemented")
                    None
                }
            }
        })
        .collect::<HashSet<_>>();
    if failed.is_empty() {
        info!("Validation successful");
        Ok(())
    } else {
        Err(eyre!(
            "Constraints failed: {}",
            failed.into_iter().collect::<Vec<_>>().join(", ")
        ))
    }
}
