use color_eyre::owo_colors::OwoColorize;
use indicatif::{ProgressBar, ProgressStyle};
use std::collections::HashSet;
use tabled::{builder::Builder, object::Columns, ModifyObject, Style};

use eyre::*;
use log::*;
use pairing_ce::{bn256::Fr, ff::Field};

use crate::{
    column::ColumnSet,
    compiler::{Constraint, ConstraintSet, Expression},
    utils::*,
};

fn fail(expr: &Expression, i: isize, l: Option<usize>, columns: &ColumnSet<Fr>) -> Result<()> {
    let mut builder = Builder::default();
    for handle in expr.dependencies() {
        builder.add_record(
            vec![handle.to_string()]
                .into_iter()
                .chain(((i - 5).max(0)..i + 5).map(|i| {
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
            .chain(((i - 5).max(0)..i + 5).map(|i| i.to_string()))
            .collect::<Vec<_>>(),
    );
    let mut table = builder.build();
    table
        .with(
            Columns::single(6)
                .modify()
                .with(|s: &str| s.red().to_string()),
        )
        .with(Style::blank());
    println!("\n\n{}\n", table);

    let r = expr.eval(
        i,
        &mut |handle, i, wrap| {
            columns
                .get(handle)
                .ok()
                .and_then(|c| c.get(i, wrap))
                .cloned()
        },
        true,
        0,
        true,
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
) -> Result<()> {
    let r = expr.eval(
        i,
        &mut |handle, i, wrap| columns.get(handle).unwrap().get(i, wrap).cloned(),
        false,
        0,
        true,
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
) -> Result<()> {
    // Early exit if all the columns are empty: the module is not triggered
    if expr.dependencies().iter().all(|handle| {
        columns
            .get(handle)
            .and_then(|col| col.len().ok_or(eyre!("")))
            .is_err()
    }) {
        return Ok(());
    }

    match domain {
        Some(is) => {
            for i in is {
                check_constraint_at(expr, *i, None, columns, true)?;
            }
            Ok(())
        }
        None => {
            let cols_lens = expr
                .dependencies()
                .into_iter()
                .map(|handle| {
                    columns
                        .get(&handle)
                        .and_then(|col| col.len().ok_or_else(|| eyre!("{} is void", handle)))
                })
                .collect::<Result<Vec<_>>>()?;
            if !cols_lens.iter().all(|&l| l == cols_lens[0]) {
                error!(
                    "all columns in `{}` are not of the same length: `{:?}`",
                    &expr,
                    expr.dependencies()
                        .iter()
                        .map(|handle| format!(
                            "{}: {}",
                            handle,
                            columns.get(handle).unwrap().len().unwrap()
                        ))
                        .collect::<Vec<_>>()
                        .join(", ")
                );
            }
            let l = cols_lens[0];
            if l == 0 {
                return Err(eyre!("empty trace, aborting"));
            }
            for i in 0..l as isize {
                check_constraint_at(expr, i, Some(l), columns, false)?;
            }
            Ok(())
        }
    }
}

pub fn check(cs: &ConstraintSet, with_bar: bool) -> Result<()> {
    if cs.columns.is_empty() {
        return Ok(());
    }
    let mut failed = HashSet::new();

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

    for c in cs.constraints.iter() {
        if let Some(bar) = bar.as_ref() {
            bar.inc(1);
        }
        match c {
            Constraint::Vanishes { name, domain, expr } => {
                if name == "INV_CONSTRAINTS" {
                    continue;
                }
                if matches!(**expr, Expression::Void) {
                    // warn!("Ignoring Void expression {}", name);
                    continue;
                }
                if let Some(bar) = bar.as_ref() {
                    bar.set_message(name.to_owned());
                }

                match expr.as_ref() {
                    Expression::List(es) => {
                        for e in es {
                            if let Err(err) = check_constraint(e, domain, &cs.columns) {
                                error!("{}", err);
                                failed.insert(name.to_owned());
                            };
                        }
                    }
                    _ => {
                        if let Err(err) = check_constraint(expr, domain, &cs.columns) {
                            error!("{}", err);
                            failed.insert(name.to_owned());
                        }
                    }
                }
            }
            Constraint::Plookup(_, _, _) => {
                // warn!("Plookup validation not yet implemented");
            }
            Constraint::Permutation(_name, _from, _to) => {
                // warn!("Permutation validation not yet implemented");
            }
            Constraint::InRange(_, _e, _range) => {
                // warn!("Range validation not yet implemented")
            }
        }
    }
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
