use cached::SizedCache;
#[cfg(feature = "interactive")]
use indicatif::{ProgressBar, ProgressStyle};
use itertools::Itertools;
use owo_colors::OwoColorize;
use rayon::prelude::*;
use std::collections::HashSet;

use anyhow::*;
use log::*;

use crate::{
    column::{ColumnSet, Value, ValueBacking},
    compiler::{Constraint, ConstraintSet, Domain, EvalSettings, Expression, Node},
    pretty::*,
    structs::Handle,
};

#[derive(Clone, Copy, Debug)]
pub struct DebugSettings {
    /// whether to skip reporting s-exps reducing to 0
    unclutter: bool,
    /// whether to dim s-exps reducing to 0
    dim: bool,
    /// whether to stop reporting a constraint on the first failure
    continue_on_error: bool,
    /// whether to report computation details on failing constraints
    report: bool,
    /// how many lines to show left of the failure point
    context_span_before: isize,
    /// how many lines to show right of the failure point
    context_span_after: isize,
    /// whether to report all the module columns in a failing constraint or only the involved ones
    full_trace: bool,
    /// whether to display the original source code along the compiled form
    src: bool,
}
impl DebugSettings {
    pub fn new() -> Self {
        DebugSettings {
            unclutter: false,
            dim: false,
            continue_on_error: false,
            report: false,
            context_span_before: 2,
            context_span_after: 2,
            full_trace: false,
            src: false,
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
    pub fn src(self, x: bool) -> Self {
        Self { src: x, ..self }
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
            context_span_before: x,
            context_span_after: x,
            ..self
        }
    }
    pub fn and_context_span_before(self, x: Option<isize>) -> Self {
        if let Some(before) = x {
            Self {
                context_span_before: before,
                ..self
            }
        } else {
            self
        }
    }
    pub fn and_context_span_after(self, x: Option<isize>) -> Self {
        if let Some(after) = x {
            Self {
                context_span_after: after,
                ..self
            }
        } else {
            self
        }
    }
    pub fn full_trace(self, x: bool) -> Self {
        Self {
            full_trace: x,
            ..self
        }
    }
}

fn columns_len(
    expr: &Node,
    columns: &ColumnSet,
    name: &Handle,
    with_padding: bool,
) -> Result<Option<usize>> {
    let cols_lens = expr
        .dependencies()
        .into_iter()
        .map(|handle| {
            if with_padding {
                columns.padded_len(&handle)
            } else {
                columns.len(&handle)
            }
        })
        .collect::<Vec<_>>();
    if cols_lens.is_empty() {
        return Ok(None);
    }

    if cols_lens.iter().all(|l| l.is_none()) {
        debug!("Skipping constraint `{}` with empty columns", name);
        return Ok(None);
    }

    if !cols_lens
        .iter()
        .filter(|l| l.is_some())
        .all(|&l| l.unwrap_or_default() == cols_lens[0].unwrap_or_default())
    {
        error!(
            "all columns in {} are not of the same length:\n{}",
            expr,
            expr.dependencies()
                .iter()
                .map(|handle| format!(
                    "\t{}: {}",
                    handle,
                    columns
                        .padded_len(handle)
                        .map(|x| x.to_string())
                        .unwrap_or_else(|| "nil".into()),
                ))
                .collect::<Vec<_>>()
                .join("\n")
        );
    }
    let l = cols_lens[0].unwrap_or(0);
    if l == 0 {
        bail!("empty trace, aborting")
    } else {
        Ok(Some(l))
    }
}

/// Pretty print an expresion and all its intermediate value for debugging (or
/// eye-candy) purposes
///
/// # Arguments
///
/// * `expr`     - The expression to dissect
/// * `i`        - The evaluation point; may be negative
/// * `wrap`     - If set, negative indices wrap; otherwise they go into the padding
/// * `settings` - The global debugging settings
fn fail(
    cs: &ConstraintSet,
    expr: &Node,
    i: isize,
    wrap: bool,
    settings: DebugSettings,
) -> Result<()> {
    let handles = if settings.full_trace {
        let module = &cs
            .handle(
                expr.dependencies()
                    .iter()
                    .next()
                    .expect("un-handled column"),
            )
            .module;
        cs.columns
            .all()
            .into_iter()
            .filter(|h| &cs.handle(h).module == module)
            .sorted_by_key(|h| cs.handle(h).name.clone())
            .collect::<Vec<_>>()
    } else {
        expr.dependencies()
            .iter()
            .cloned()
            .sorted_by_key(|h| cs.handle(h).name.clone())
            .collect::<Vec<_>>()
    };

    let mut m_columns = vec![vec![String::new()]
        .into_iter()
        .chain(handles.iter().map(|h| cs.handle(h).name.to_string()))
        .collect::<Vec<_>>()];

    let (eval_columns_range, idx_highlight) = if wrap {
        (
            (i - settings.context_span_before)..=i + settings.context_span_after,
            // - 1 to account for the title column
            (i - settings.context_span_before) - 1,
        )
    } else {
        (
            (i - settings.context_span_before).max(0)..=i + settings.context_span_after,
            (i - settings.context_span_before).max(0) - 1,
        )
    };
    for j in eval_columns_range {
        m_columns.push(
            vec![j.to_string()]
                .into_iter()
                .chain(handles.iter().map(|handle| {
                    cs.columns
                        .get(handle, j, true)
                        .map(|x| {
                            x.pretty_with_base(cs.columns.column(handle).unwrap().base)
                                .to_string()
                        })
                        .unwrap_or_else(|| "nil".into())
                }))
                .collect(),
        )
    }

    let mut trace = String::new();
    for ii in 0..m_columns[0].len() {
        for (j, col) in m_columns.iter().enumerate() {
            let padding = col.iter().map(String::len).max().unwrap() + 2;
            if j as isize + idx_highlight == i {
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

    bail!(
        trace
            + &expr.debug(
                &|n| n.eval(
                    i,
                    |handle, i, wrap| cs.columns.get(handle, i, wrap),
                    &mut None,
                    &Default::default(),
                ),
                settings.unclutter,
                settings.dim,
                settings.src,
            )
    )
}

fn check_constraint_at(
    cs: &ConstraintSet,
    expr: &Node,
    i: isize,
    wrap: bool,
    fail_on_oob: bool,
    cache: &mut Option<SizedCache<Value, Value>>,
    settings: DebugSettings,
) -> Result<()> {
    let r = expr.eval(
        i,
        |handle, i, wrap| cs.columns.get_raw(handle, i, wrap),
        cache,
        &EvalSettings::new().wrap(wrap),
    );
    if let Some(r) = r {
        if !r.is_zero() {
            return fail(cs, expr, i, wrap, settings);
        }
    } else if fail_on_oob {
        return fail(cs, expr, i, wrap, settings);
    }
    Ok(())
}

fn check_inrange(name: &Handle, expr: &Node, columns: &ColumnSet, max: &Value) -> Result<()> {
    let l = columns_len(expr, columns, name, false)?;
    if let Some(l) = l {
        for i in 0..l as isize {
            let r = expr
                .eval(
                    i,
                    |handle, i, wrap| columns.get_raw(handle, i, wrap),
                    &mut None,
                    &Default::default(),
                )
                .unwrap();
            if r.ge(max) {
                bail!(
                    "{} = {} > {}",
                    expr.to_string().white().bold(),
                    r.pretty().red().bold(),
                    max.pretty().blue()
                )
            }
        }
        Ok(())
    } else {
        Ok(())
    }
}

fn check_constraint(
    cs: &ConstraintSet,
    expr: &Node,
    domain: &Option<Domain>,
    name: &Handle,
    settings: DebugSettings,
) -> Result<()> {
    let l = columns_len(expr, &cs.columns, name, true)?;
    if let Some(l) = l {
        let mut cache = Some(cached::SizedCache::with_size(200000)); // ~1.60MB cache
        match domain {
            Some(is) => {
                for i in is.iter() {
                    check_constraint_at(cs, expr, i, true, true, &mut cache, settings)?;
                }
            }
            None => {
                for i in 0..l as isize {
                    let err = check_constraint_at(cs, expr, i, false, false, &mut cache, settings);

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
    } else {
        warn!(
            "constraint {} will not be checked, because it does not involve any column",
            name.pretty()
        );
        Ok(())
    }
}

fn check_plookup(cs: &ConstraintSet, parents: &[Node], children: &[Node]) -> Result<()> {
    // Compute the LC \sum_k (k+1) × x_k[i]
    fn pseudo_rlc(cols: &[&ValueBacking], i: usize, columns: &ColumnSet) -> Value {
        let mut ax = Value::zero();

        for (j, col) in cols.iter().enumerate() {
            let mut x = Value::from(j + 2);
            let mut col_value = col.get(i as isize, false, columns).unwrap();
            col_value.to_bi();

            x.mul_assign(&col_value);
            ax.add_assign(&x);
        }
        ax
    }

    // Check that we have the same number of columns; should be guaranteed by the com
    if children.len() != parents.len() {
        bail!("parents and children are not of the same length")
    }

    // Check for emptiness in parents & children
    let children_empty = children
        .iter()
        .flat_map(|e| e.dependencies().into_iter())
        .map(|h| cs.columns.len(&h).unwrap_or_default())
        .all(|l| l == 0);
    let parent_empty = parents
        .iter()
        .flat_map(|n| n.dependencies().into_iter())
        .map(|h| cs.columns.len(&h).unwrap_or_default())
        .all(|l| l == 0);
    match (children_empty, parent_empty) {
        (true, true) | (true, false) => {
            warn!("empty plookup found; skipping");
            return Ok(());
        }
        (false, true) => bail!(
            "parents ({}) are empty, but not children",
            parents.iter().map(|p| p.pretty()).join(", ")
        ),
        (false, false) => {}
    }

    let parent_cols = parents
        .iter()
        .map(|n| {
            if let Expression::Column { handle, .. } = n.e() {
                handle
            } else {
                unreachable!()
            }
        })
        .collect::<Vec<_>>();
    let child_cols = parents
        .iter()
        .map(|n| {
            if let Expression::Column { handle, .. } = n.e() {
                handle
            } else {
                unreachable!()
            }
        })
        .collect::<Vec<_>>();
    if parent_cols
        .get(0)
        .map(|p| cs.columns.register_of(p).len().unwrap())
        .unwrap_or(0)
        == 0
        || child_cols
            .get(0)
            .map(|c| cs.columns.register_of(c).len().unwrap())
            .unwrap_or(0)
            == 0
    {
        warn!("empty lookup; skipping");
        return Ok(());
    }

    let parent_backings = parent_cols
        .iter()
        .map(|h| cs.columns.backing(h).unwrap())
        .collect::<Vec<_>>();

    let child_backings = child_cols
        .iter()
        .map(|h| cs.columns.backing(h).unwrap())
        .collect::<Vec<_>>();

    let hashes: HashSet<_> = (0..parent_backings.iter().find_map(|p| p.len()).unwrap())
        .map(|i| pseudo_rlc(&parent_backings, i, &cs.columns))
        .collect();

    for i in 0..child_backings.iter().find_map(|c| c.len()).unwrap() {
        if !hashes.contains(&pseudo_rlc(&child_backings, i, &cs.columns)) {
            bail!(
                "@{}: {{\n{}\n}} not found in {{{}}}",
                i,
                children
                    .iter()
                    .zip(
                        child_backings
                            .iter()
                            .map(|v| v.get(i as isize, false, &cs.columns).unwrap())
                    )
                    .map(|(handle, value)| format!("{}: {}", handle.pretty(), value.pretty()))
                    .collect::<Vec<_>>()
                    .join("\n"),
                parents
                    .iter()
                    .map(|handle| handle.pretty())
                    .collect::<Vec<_>>()
                    .join(", ")
            );
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
    if cs.columns.is_empty() {
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
        bail!("refusing to check an empty constraint set")
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
                                if let Err(trace) = check_constraint(cs, e, domain, name, settings)
                                {
                                    if settings.report {
                                        println!(
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
                            if let Err(trace) = check_constraint(cs, expr, domain, name, settings) {
                                if settings.report {
                                    println!(
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
                            println!("{} failed:\n{:?}\n", name, trace);
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
                    ..
                } => {
                    // warn!("Permutation validation not yet implemented");
                    None
                }
                Constraint::InRange { handle, exp, max } => {
                    if let Err(trace) = check_inrange(handle, exp, &cs.columns, max) {
                        if settings.report {
                            println!("{} failed:\n{:?}\n", handle, trace);
                        }
                        Some(handle.to_owned())
                    } else {
                        None
                    }
                }
                Constraint::Normalization { .. } => {
                    todo!()
                }
            }
        })
        .collect::<HashSet<_>>();
    if failed.is_empty() {
        info!("Validation successful");
        Ok(())
    } else {
        bail!(
            "constraints failed: {}",
            failed
                .into_iter()
                .map(|x| x.to_string().bold().red().to_string())
                .collect::<Vec<_>>()
                .join(", ")
        )
    }
}
