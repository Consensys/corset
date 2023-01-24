use cached::SizedCache;
use colored::Colorize;
#[cfg(feature = "interactive")]
use indicatif::{ProgressBar, ProgressStyle};
use itertools::Itertools;
use rayon::prelude::*;
use std::collections::HashSet;

use anyhow::*;
use log::*;
use pairing_ce::{
    bn256::Fr,
    ff::{Field, PrimeField},
};

use crate::{
    column::ColumnSet,
    compiler::{Constraint, ConstraintSet, Expression, Handle, Node},
    pretty::*,
};

#[derive(Clone, Copy, Debug)]
pub struct DebugSettings {
    unclutter: bool,
    /// whether to skip reporting s-exps reducing to 0
    dim: bool,
    /// whether to dim s-exps reducing to 0
    continue_on_error: bool,
    /// whether to stop reporting a constraint on the first failure
    report: bool,
    /// whether to report computation details on failing constraints
    context_span: isize,
    /// whether to report all the module columns in a failing constraint or only the involved ones
    full_trace: bool,
}
impl DebugSettings {
    pub fn new() -> Self {
        DebugSettings {
            unclutter: false,
            dim: false,
            continue_on_error: false,
            report: false,
            context_span: 2,
            full_trace: false,
        }
    }
    pub fn dim(self, x: bool) -> Self {
        Self { dim: x, ..self }
    }
    pub fn unclutter(self, x: bool) -> Self {
        Self {
            unclutter: x,
            ..self
        }
    }
    pub fn continue_on_error(self, x: bool) -> Self {
        Self {
            continue_on_error: x,
            ..self
        }
    }
    pub fn report(self, x: bool) -> Self {
        Self { report: x, ..self }
    }
    pub fn context_span(self, x: isize) -> Self {
        Self {
            context_span: x,
            ..self
        }
    }
    pub fn full_trace(self, x: bool) -> Self {
        Self {
            full_trace: x,
            ..self
        }
    }
}

fn fail(expr: &Node, i: isize, columns: &ColumnSet, settings: DebugSettings) -> Result<()> {
    let module = expr.dependencies().iter().next().unwrap().module.clone();
    let handles = if settings.full_trace {
        columns
            .cols
            .get(&module)
            .unwrap()
            .keys()
            .map(|name| Handle::new(&module, &name))
            .sorted_by_key(|h| h.name.clone())
            .collect::<Vec<_>>()
    } else {
        expr.dependencies()
            .iter()
            .cloned()
            .sorted_by_cached_key(Handle::to_string)
            .collect::<Vec<_>>()
    };

    let mut m_columns = vec![vec![String::new()]
        .into_iter()
        .chain(handles.iter().map(|h| h.name.to_string()))
        .collect::<Vec<_>>()];
    for j in (i - settings.context_span).max(0)..=i + settings.context_span {
        m_columns.push(
            vec![j.to_string()]
                .into_iter()
                .chain(handles.iter().map(|handle| {
                    columns
                        .get(handle)
                        .unwrap()
                        .get(j, false)
                        .map(|x| x.pretty())
                        .unwrap_or_else(|| "nil".into())
                }))
                .collect(),
        )
    }

    let mut trace = String::new();
    for ii in 0..m_columns[0].len() {
        for (j, col) in m_columns.iter().enumerate() {
            let padding = col.iter().map(String::len).max().unwrap() + 2;
            // - 1 to account for the first column
            if j as isize + (i - settings.context_span).max(0) - 1 == i {
                trace.push_str(&format!(
                    "{:width$}",
                    m_columns[j][ii].red().bold(),
                    width = padding
                ));
            } else {
                let s = format!("{:width$}", m_columns[j][ii], width = padding);
                trace.push_str(
                    &(if j == 0 {
                        s.bright_white().bold().to_string()
                    } else {
                        s
                    }),
                )
            }
        }
        trace.push('\n');
    }
    trace.push('\n');

    Err(anyhow!(
        trace
            + &expr.debug(
                &|n| n.eval(
                    i,
                    &mut |handle, i, wrap| columns._cols[handle.id.unwrap()].get(i, wrap).cloned(),
                    &mut None,
                    &Default::default(),
                ),
                settings.unclutter,
                settings.dim
            )
    ))
}

fn check_constraint_at(
    expr: &Node,
    i: isize,
    columns: &ColumnSet,
    fail_on_oob: bool,
    cache: &mut Option<SizedCache<Fr, Fr>>,
    settings: DebugSettings,
) -> Result<()> {
    let r = expr.eval(
        i,
        &mut |handle, i, wrap| columns._cols[handle.id.unwrap()].get_raw(i, wrap).cloned(),
        cache,
        &Default::default(),
    );
    if let Some(r) = r {
        if !r.is_zero() {
            return fail(expr, i, columns, settings);
        }
    } else if fail_on_oob {
        return fail(expr, i, columns, settings);
    }
    Ok(())
}

fn check_constraint(
    expr: &Node,
    domain: &Option<Vec<isize>>,
    columns: &ColumnSet,
    name: &Handle,
    settings: DebugSettings,
) -> Result<()> {
    let cols_lens = expr
        .dependencies()
        .into_iter()
        .map(|handle| {
            columns
                .get(&handle)
                .with_context(|| anyhow!("can not find column `{}`", handle))
                .map(|c| c.padded_len())
        })
        .collect::<Result<Vec<_>>>()?;
    if cols_lens.is_empty() {
        return Ok(());
    }
    // Early exit if all the columns are empty: the module is not triggered
    // Ideally, this should be an `all` rather than an `any`, but the ID
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
                    "\t{}: {}/{:?}",
                    handle,
                    columns
                        .get(handle)
                        .unwrap()
                        .padded_len()
                        .map(|x| x.to_string())
                        .unwrap_or_else(|| "nil".into()),
                    columns.get(handle).unwrap().spilling
                ))
                .collect::<Vec<_>>()
                .join("\n")
        );
    }
    let l = cols_lens[0].unwrap_or(0);
    if l == 0 {
        return Err(anyhow!("empty trace, aborting"));
    }

    let mut cache = Some(cached::SizedCache::with_size(200000)); // ~1.60MB cache
    match domain {
        Some(is) => {
            for i in is {
                check_constraint_at(expr, *i, columns, true, &mut cache, settings)?;
            }
        }
        None => {
            for i in 0..l as isize {
                let err = check_constraint_at(expr, i, columns, false, &mut cache, settings);

                if err.is_err() {
                    if settings.continue_on_error {
                        eprintln!("{:?}", err);
                    } else {
                        return err;
                    }
                }
            }
        }
    };
    Ok(())
}

fn check_plookup(cs: &ConstraintSet, parents: &[Node], children: &[Node]) -> Result<()> {
    // Compute the LC \sum_k (k+1) × x_k[i]
    fn pseudo_rlc(cols: &[Vec<Fr>], i: usize) -> Fr {
        let mut ax = Fr::zero();
        for (j, col) in cols.iter().enumerate() {
            let mut x = Fr::from_str(&(j + 2).to_string()).unwrap();
            x.mul_assign(&col[i]);
            ax.add_assign(&x);
        }
        ax
    }

    // Given a list of column expression to PLookup, retrieve the corresponding values
    fn compute_cols(exps: &[Node], cs: &ConstraintSet) -> Result<Vec<Vec<Fr>>> {
        let cols = exps
            .iter()
            .map(|e| cs.compute_composite_static(e))
            .collect::<Result<Vec<_>>>()
            .with_context(|| anyhow!("while computing {:?}", exps))?;
        if !cols.iter().all(|p| p.len() == cols[0].len()) {
            return Err(anyhow!("all columns should be of the same length"));
        }

        Ok(cols)
    }

    // Check that we have the same number of columns; should be guaranteed by the com
    if children.len() != parents.len() {
        return Err(anyhow!("parents and children are not of the same length"));
    }

    // Check for emptiness in parents & children
    let children_empty = children
        .iter()
        .flat_map(|e| e.dependencies().into_iter())
        .map(|h| cs.modules.by_handle(&h).unwrap().len().unwrap_or_default())
        .all(|l| l == 0);
    let parent_empty = parents
        .iter()
        .flat_map(|n| n.dependencies().into_iter())
        .map(|h| cs.modules.by_handle(&h).unwrap().len().unwrap_or_default())
        .all(|l| l == 0);
    match (children_empty, parent_empty) {
        (true, true) | (true, false) => {
            debug!("empty plookup found; skipping");
            return Ok(());
        }
        (false, true) => return Err(anyhow!("parents are emypy, but not children")),
        (false, false) => {}
    }

    // Compute the final columns
    let parent_cols = compute_cols(parents, cs)?;
    let child_cols = compute_cols(children, cs)?;
    if parent_cols.get(0).map(|c| c.len()).unwrap_or(0) == 0
        || child_cols.get(0).map(|c| c.len()).unwrap_or(0) == 0
    {
        debug!("empty plookup; skipping");
        return Ok(());
    }

    let hashes: HashSet<_> = (0..parent_cols[0].len())
        .map(|i| pseudo_rlc(&parent_cols, i))
        .collect();

    for i in 0..child_cols[0].len() {
        if !hashes.contains(&pseudo_rlc(&child_cols, i)) {
            return Err(anyhow!(
                "@{}: {{\n{}\n}} not found in {{{}}}",
                i,
                children
                    .iter()
                    .zip(child_cols.iter().map(|c| c[i]))
                    .map(|(k, v)| format!("{}: {}", k, v.pretty()))
                    .collect::<Vec<_>>()
                    .join("\n"),
                parents
                    .iter()
                    .map(|k| format!("{}", k))
                    .collect::<Vec<_>>()
                    .join(", ")
            ));
        }
    }

    Ok(())
}

pub fn check(
    cs: &ConstraintSet,
    only: &Option<Vec<String>>,
    skip: &[String],
    with_bar: bool,
    expand: bool,
    settings: DebugSettings,
) -> Result<()> {
    if cs.modules.is_empty() {
        info!("Skipping empty trace");
        return Ok(());
    }

    #[cfg(feature = "interactive")]
    let bar = if with_bar {
        {
            Some(
                ProgressBar::new(cs.constraints.len() as u64).with_style(
                    ProgressStyle::default_bar()
                        .template("Validating {msg} {bar:40} {pos}/{len}")
                        .unwrap()
                        .progress_chars("##-"),
                ),
            )
        }
    } else {
        None
    };
    let todo = cs
        .constraints
        .iter()
        .filter(|c| only.as_ref().map(|o| o.contains(&c.name())).unwrap_or(true))
        .filter(|c| !skip.contains(&c.name()))
        .collect::<Vec<_>>();
    if todo.is_empty() {
        return Err(anyhow!("refusing to check an empty constraint set"));
    }

    let failed = todo
        .par_iter()
        .with_max_len(1)
        .inspect(|_| {
            #[cfg(feature = "interactive")]
            {
                if let Some(b) = &bar {
                    b.inc(1)
                }
            }
        })
        .filter_map(|c| {
            match c {
                Constraint::Vanishes {
                    handle: name,
                    domain,
                    expr,
                } => {
                    if matches!(expr.e(), Expression::Void) {
                        return None;
                    }

                    if name.name == "INV_CONSTRAINTS" && !expand {
                        return None;
                    }

                    match expr.as_ref().e() {
                        Expression::List(es) => {
                            for e in es {
                                if let Err(trace) =
                                    check_constraint(e, domain, &cs.modules, name, settings)
                                {
                                    if settings.report {
                                        error!(
                                            "{} failed:\n{}\n",
                                            name.to_string().red().bold(),
                                            trace
                                        );
                                    }
                                    return Some(name.to_owned());
                                }
                            }
                            None
                        }
                        _ => {
                            if let Err(trace) =
                                check_constraint(expr, domain, &cs.modules, name, settings)
                            {
                                if settings.report {
                                    error!(
                                        "{} failed:\n{}\n",
                                        name.to_string().red().bold(),
                                        trace
                                    );
                                }
                                Some(name.to_owned())
                            } else {
                                None
                            }
                        }
                    }
                }
                Constraint::Plookup {
                    handle: name,
                    including: parents,
                    included: children,
                } => {
                    if let Err(trace) = check_plookup(cs, parents, children) {
                        if settings.report {
                            error!("{} failed:\n{:?}\n", name, trace);
                        }
                        Some(name.to_owned())
                    } else {
                        None
                    }
                }
                Constraint::Permutation {
                    handle: _name,
                    from: _from,
                    to: _to,
                } => {
                    // warn!("Permutation validation not yet implemented");
                    None
                }
                Constraint::InRange {
                    handle: _,
                    exp: _e,
                    max: _range,
                } => {
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
        Err(anyhow!(
            "Constraints failed: {}",
            failed
                .into_iter()
                .map(|x| x.to_string().bold().red().to_string())
                .collect::<Vec<_>>()
                .join(", ")
        ))
    }
}
