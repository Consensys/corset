use crate::{
    column::{ColumnSet, Value},
    compiler::{Constraint, ConstraintSet, Domain, EvalSettings, Expression, Node},
    pretty::*,
    structs::Handle,
};
use anyhow::*;
use cached::SizedCache;
use itertools::Itertools;
use log::*;
use owo_colors::OwoColorize;
use rayon::prelude::*;
use std::collections::HashSet;
use thiserror::Error;

#[derive(Error, Debug)]
enum CheckingError {
    #[error("columns for {} not found in trace file", .0.pretty())]
    NoColumnsFound(Handle),
    #[error("")]
    FailingConstraint(Handle, String),
    #[error("")]
    MismatchingLengths(Error),
}

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

fn check_inrange(expr: &Node, cs: &ConstraintSet, max: &Value) -> Result<()> {
    let l = cs.dependencies_len(expr, false)?;
    if let Some(l) = l {
        for i in 0..l as isize {
            let r = expr
                .eval(
                    i,
                    |handle, i, wrap| cs.columns.get_raw(handle, i, wrap),
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
    domain: &Option<Domain<isize>>,
    name: &Handle,
    settings: DebugSettings,
) -> Result<()> {
    let l = cs
        .dependencies_len(expr, true)
        .map_err(CheckingError::MismatchingLengths)?;
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
                    let err = check_constraint_at(cs, expr, i, false, false, &mut cache, settings)
                        .map_err(|e| CheckingError::FailingConstraint(name.clone(), e.to_string()));

                    if err.is_err() {
                        if settings.continue_on_error {
                            eprintln!("{:?}", err);
                        } else {
                            bail!(err.err().unwrap());
                        }
                    }
                }
            }
        };
        info!("{} validated", name.pretty());
        Ok(())
    } else {
        bail!(CheckingError::NoColumnsFound(name.clone()))
    }
}

fn check_lookup(
    cs: &ConstraintSet,
    handle: &Handle,
    parents: &[Node],
    children: &[Node],
) -> Result<()> {
    // Compute the LC \sum_k (k+1) × x_k[i]
    fn pseudo_rlc(exps: &[Node], i: usize, cs: &ColumnSet) -> Value {
        let mut ax = Value::zero();

        for (j, exp) in exps.iter().enumerate() {
            let mut x = Value::from(j + 2);
            let col_value = exp
                .eval(
                    i as isize,
                    |handle, j, _| {
                        cs.get(handle, j, false)
                            .or_else(|| cs.column(handle).unwrap().padding_value.as_ref().cloned())
                    },
                    &mut None,
                    &EvalSettings::default(),
                )
                .unwrap_or_default();

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
            warn!("skipping empty lookup {}", handle.pretty());
            return Ok(());
        }
        (false, true) => bail!(
            "parents ({}) are empty, but not children",
            parents.iter().map(|p| p.pretty()).join(", ")
        ),
        (false, false) => {}
    }

    let parent_module = cs.module_of_exprs(parents).unwrap();
    let parent_len = cs.iter_len(&parent_module);

    let child_module = cs.module_of_exprs(children).unwrap();
    let child_len = cs.iter_len(&child_module);

    let parent_hashes: HashSet<_> = (0..parent_len)
        .map(|i| pseudo_rlc(parents, i, &cs.columns))
        .collect();

    for i in 0..child_len {
        if !parent_hashes.contains(&pseudo_rlc(children, i, &cs.columns)) {
            let pretty_expected_matches = parents
                .iter()
                .zip(children.iter().zip(children.iter().map(|e| {
                    e.eval(
                        i as isize,
                        |handle, j, _| {
                            cs.columns.get(handle, j, false).or_else(|| {
                                cs.columns
                                    .column(handle)
                                    .unwrap()
                                    .padding_value
                                    .as_ref()
                                    .cloned()
                            })
                        },
                        &mut None,
                        &EvalSettings::default(),
                    )
                    .unwrap_or_default()
                })))
                .map(|(parent, (child, value))| {
                    format!(
                        "{} - {}: {}",
                        parent.pretty(),
                        child.pretty(),
                        value.pretty_with_base(Base::Hex)
                    )
                })
                .join("\n");
            bail!("mismatch line {}:\n{}", i, pretty_expected_matches);
        }
    }

    Ok(())
}

pub fn check(
    cs: &ConstraintSet,
    only: &Option<Vec<String>>,
    skip: &[String],
    settings: DebugSettings,
) -> Result<()> {
    if cs.columns.is_empty() {
        info!("Skipping empty trace");
        return Ok(());
    }

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

                    match expr.as_ref().e() {
                        Expression::List(es) => {
                            for e in es {
                                if let Err(err) = check_constraint(cs, e, domain, name, settings) {
                                    match err.downcast_ref::<CheckingError>() {
                                        Some(err) => match err {
                                            CheckingError::NoColumnsFound(_) => {
                                                warn!("{}", err);
                                                break;
                                            }
                                            CheckingError::FailingConstraint(handle, trace) => {
                                                if settings.report {
                                                    println!(
                                                        "{} failed:\n{}\n",
                                                        handle.to_string().red().bold(),
                                                        trace
                                                    );
                                                }
                                                return Some(name.to_owned());
                                            }
                                            CheckingError::MismatchingLengths(err) => {
                                                error!("{err}");
                                                return Some(name.to_owned());
                                            }
                                        },
                                        None => {
                                            warn!("{}", err);
                                            break;
                                        }
                                    }
                                }
                            }
                            None
                        }
                        _ => {
                            if let Err(err) = check_constraint(cs, expr, domain, name, settings) {
                                match err.downcast_ref::<CheckingError>() {
                                    Some(CheckingError::NoColumnsFound(_)) => {
                                        warn!("{}", err);
                                        None
                                    }
                                    Some(CheckingError::FailingConstraint(handle, trace)) => {
                                        if settings.report {
                                            println!(
                                                "{} failed:\n{}\n",
                                                handle.to_string().red().bold(),
                                                trace
                                            );
                                        }
                                        Some(name.to_owned())
                                    }
                                    Some(CheckingError::MismatchingLengths(err)) => {
                                        error!("{err}");
                                        return Some(name.to_owned());
                                    }
                                    None => {
                                        warn!("{}", err);
                                        None
                                    }
                                }
                            } else {
                                None
                            }
                        }
                    }
                }
                Constraint::Lookup {
                    handle,
                    including,
                    included,
                } => {
                    if let Err(trace) = check_lookup(cs, handle, including, included) {
                        if settings.report {
                            println!("{} failed:\n{:?}\n", handle, trace);
                        }
                        Some(handle.to_owned())
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
                    if let Err(trace) = check_inrange(exp, &cs, max) {
                        if settings.report {
                            println!("{} failed:\n{:?}\n", handle, trace);
                        }
                        Some(handle.to_owned())
                    } else {
                        None
                    }
                }
                Constraint::Normalization { .. } => {
                    // We trust ourselves
                    None
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
