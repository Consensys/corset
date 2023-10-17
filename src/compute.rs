use anyhow::{anyhow, bail, Context, Result};
use log::*;
use logging_timer::time;
use owo_colors::OwoColorize;
use rayon::prelude::*;
use std::{cmp::Ordering, collections::HashSet};

use crate::{
    column::{ColumnSet, Computation, ExoOperation, Value, ValueBacking},
    compiler::{ColumnRef, ConstraintSet, EvalSettings, Node},
    dag::ComputationDag,
    errors::RuntimeError,
    import,
    pretty::Pretty,
    structs::Handle,
};

/// Given a set of operation and their arguments, generate the traces required
/// to prove the operation and its results.
fn compute_ancillaries(
    cs: &mut ConstraintSet,
    exo_operations: HashSet<(ExoOperation, Value, Value)>,
) -> Result<()> {
    let mut add_ops = vec![Value::zero()];
    let mut add_args1 = vec![Value::zero()];
    let mut add_args2 = vec![Value::zero()];
    let mut add_results = vec![Value::zero()];
    let mut add_done = vec![Value::zero()];

    let mut mul_args1 = vec![Value::zero()];
    let mut mul_args2 = vec![Value::zero()];
    let mut mul_results = vec![Value::zero()];
    let mut mul_done = vec![Value::zero()];
    let do_it = !exo_operations.is_empty();
    for (op, arg1, arg2) in exo_operations.into_iter() {
        // TODO: actually compute the trace
        // TODO: incorporate the constraints
        let mut r = arg1.clone();
        match op {
            ExoOperation::Add => {
                r.add_assign(&arg2);
            }
            ExoOperation::Sub => {
                r.sub_assign(&arg2);
            }
            ExoOperation::Mul => {
                r.mul_assign(&arg2);
            }
        }

        match op {
            ExoOperation::Add | ExoOperation::Sub => {
                add_ops.push(if op == ExoOperation::Add {
                    Value::one()
                } else {
                    Value::zero()
                });
                add_args1.push(arg1);
                add_args2.push(arg2);
                add_results.push(r);
                add_done.push(Value::one());
            }
            ExoOperation::Mul => {
                mul_args1.push(arg1);
                mul_args2.push(arg2);
                mul_results.push(r);
                mul_done.push(Value::one());
            }
        }
    }

    use crate::compiler::generator::{ADDER_MODULE, MULER_MODULE};
    cs.effective_len_or_set(ADDER_MODULE, add_done.len() as isize);
    cs.effective_len_or_set(MULER_MODULE, mul_done.len() as isize);

    if do_it {
        let h: ColumnRef = Handle::new(ADDER_MODULE, "op").into();
        trace!("Filling {}", h.pretty());
        cs.columns
            .set_raw_value(&h, add_ops, 0)
            .with_context(|| anyhow!("while filling {}", h.pretty()))?;
        let h: ColumnRef = Handle::new(ADDER_MODULE, "arg-1").into();
        trace!("Filling {}", h.pretty());
        cs.columns
            .set_raw_value(&h, add_args1, 0)
            .with_context(|| anyhow!("while filling {}", h.pretty()))?;
        let h: ColumnRef = Handle::new(ADDER_MODULE, "arg-2").into();
        trace!("Filling {}", h.pretty());
        cs.columns
            .set_raw_value(&h, add_args2, 0)
            .with_context(|| anyhow!("while filling {}", h.pretty()))?;
        let h: ColumnRef = Handle::new(ADDER_MODULE, "result").into();
        trace!("Filling {}", h.pretty());
        cs.columns
            .set_raw_value(&h, add_results, 0)
            .with_context(|| anyhow!("while filling {}", h.pretty()))?;
        let h: ColumnRef = Handle::new(ADDER_MODULE, "done").into();
        trace!("Filling {}", h.pretty());
        cs.columns
            .set_raw_value(&h, add_done, 0)
            .with_context(|| anyhow!("while filling {}", h.pretty()))?;

        let h: ColumnRef = Handle::new(MULER_MODULE, "arg-1").into();
        trace!("Filling {}", h.pretty());
        cs.columns
            .set_raw_value(&h, mul_args1, 0)
            .with_context(|| anyhow!("while filling {}", h.pretty()))?;
        let h: ColumnRef = Handle::new(MULER_MODULE, "arg-2").into();
        trace!("Filling {}", h.pretty());
        cs.columns
            .set_raw_value(&h, mul_args2, 0)
            .with_context(|| anyhow!("while filling {}", h.pretty()))?;
        let h: ColumnRef = Handle::new(MULER_MODULE, "result").into();
        trace!("Filling {}", h.pretty());
        cs.columns
            .set_raw_value(&h, mul_results, 0)
            .with_context(|| anyhow!("while filling {}", h.pretty()))?;
        let h: ColumnRef = Handle::new(MULER_MODULE, "done").into();
        trace!("Filling {}", h.pretty());
        cs.columns
            .set_raw_value(&h, mul_done, 0)
            .with_context(|| anyhow!("while filling {}", h.pretty()))?;
    }

    Ok(())
}

#[time("info", "Computing expanded columns")]
fn compute_all(cs: &mut ConstraintSet) -> Result<()> {
    // Computations are split in sequentially dependent sets, where each set as
    // to be completely computed before the next one is started, but all
    // computations within a set can be processed in parallel
    let jobs = ComputationDag::from_computations(cs.computations.iter());

    let mut exo_operations = HashSet::new();

    for processing_slice in jobs.job_slices() {
        trace!("Processing computation slice {:?}", processing_slice);
        let comps = processing_slice
            .iter()
            .filter_map(|h| cs.computations.computation_idx_for(h))
            .collect::<HashSet<_>>()
            .iter()
            .map(|i| cs.computations.get(*i).unwrap().to_owned())
            .collect::<Vec<_>>();

        for r in comps
            .iter()
            // .into_par_iter() // TODO: is that a bottleneck?
            .filter_map(|comp| apply_computation(cs, &comp, &mut exo_operations))
            .collect::<Vec<_>>()
            .into_iter()
        {
            match r {
                Ok(xs) => {
                    for (h, backing) in xs.into_iter() {
                        trace!("Filling {}", h.pretty());
                        cs.columns
                            .set_backing(&h, backing)
                            .with_context(|| anyhow!("while filling {}", h.pretty()))?;
                    }
                }
                Err(e) => warn!("{}", e),
            }
        }
    }

    compute_ancillaries(cs, exo_operations)?;

    Ok(())
}

fn ensure_is_computed(h: &ColumnRef, cs: &ConstraintSet) -> Result<()> {
    if !cs.columns.is_computed(h) {
        bail!(err_missing_column(cs.columns.column(h).unwrap()))
    }
    Ok(())
}

fn compute_interleaved(
    cs: &ConstraintSet,
    froms: &[ColumnRef],
    target: &ColumnRef,
) -> Result<Vec<ComputedColumn>> {
    for from in froms.iter() {
        ensure_is_computed(from, cs)?;
    }

    if !froms
        .iter()
        .map(|h| cs.columns.len(h).unwrap())
        .collect::<Vec<_>>()
        .windows(2)
        .all(|w| w[0] == w[1])
    {
        bail!("interleaving columns of incoherent lengths")
    }

    let final_len = froms.iter().map(|h| cs.columns.len(h).unwrap()).sum();
    let count = froms.len();
    let values = (0..final_len)
        .map(|k| {
            let i = k / count;
            let j = k % count;
            cs.columns
                .get(&froms[j], i as isize, false)
                .unwrap()
                .clone()
        })
        .collect();

    Ok(vec![(target.to_owned(), ValueBacking::from_vec(values, 0))])
}

fn compute_sorted(
    cs: &ConstraintSet,
    froms: &[ColumnRef],
    tos: &[ColumnRef],
    signs: &[bool],
) -> Result<Vec<ComputedColumn>> {
    let spilling = cs.spilling_for_column(&froms[0]).unwrap();
    for from in froms.iter() {
        ensure_is_computed(from, cs)?;
    }

    if !froms
        .windows(2)
        .all(|w| cs.columns.padded_len(&w[0]) == cs.columns.padded_len(&w[1]))
    {
        bail!("sorted columns are of incoherent lengths")
    }
    let len = cs.columns.len(&froms[0]).unwrap();

    let mut sorted_is = (0..len).collect::<Vec<_>>();
    sorted_is.sort_by(|i, j| {
        for (sign, from) in signs.iter().zip(froms.iter()) {
            let x_i = cs.columns.get(from, *i as isize, false).unwrap();
            let x_j = cs.columns.get(from, *j as isize, false).unwrap();
            if let x @ (Ordering::Greater | Ordering::Less) = x_i.cmp(&x_j) {
                return if *sign { x } else { x.reverse() };
            }
        }
        Ordering::Equal
    });

    Ok(froms
        .iter()
        .enumerate()
        .map(|(k, from)| {
            let value: Vec<Value> = vec![Value::zero(); spilling as usize]
                .into_iter()
                .chain(sorted_is.iter().map(|i| {
                    cs.columns
                        .get(from, (*i).try_into().unwrap(), false)
                        .unwrap()
                        .clone()
                }))
                .collect();

            (tos[k].to_owned(), ValueBacking::from_vec(value, spilling))
        })
        .collect::<Vec<_>>())
}

fn compute_exoconstant(
    cs: &ConstraintSet,
    to: &ColumnRef,
    value: &Value,
) -> Result<Vec<ComputedColumn>> {
    let spilling = cs.spilling_for_column(to).unwrap();
    let len = cs
        .effective_len_for(&cs.columns.column(to).unwrap().handle.module)
        .unwrap() as usize;

    // Constant columns take value 0 in the padding
    let value: Vec<Value> = vec![Value::zero(); spilling as usize + 1] // TODO: WTF spilling off-by-one?
        .into_iter()
        .chain(std::iter::repeat(value.clone()).take(len))
        .collect();

    Ok(vec![(
        to.to_owned(),
        ValueBacking::from_vec(value, spilling),
    )])
}

fn compute_exooperation(
    cs: &ConstraintSet,
    op: ExoOperation,
    sources: &[Node; 2],
    target: &ColumnRef,
    exo_operations: &mut HashSet<(ExoOperation, Value, Value)>,
) -> Result<Vec<ComputedColumn>> {
    let spilling = cs.spilling_for_column(target).unwrap();
    let len = cs
        .effective_len_for(&cs.columns.column(target).unwrap().handle.module)
        .unwrap();

    let mut cache = Some(cached::SizedCache::with_size(200000)); // ~1.60MB cache
    let getter = |handle: &ColumnRef, j, _| {
        let r = cs.columns.get(handle, j, false).or_else(|| {
            cs.columns
                .column(handle)
                .unwrap()
                .padding_value
                .as_ref()
                .map(|x| x.clone())
        });
        r
    };

    let value: Vec<Value> = (-spilling..=len)
        .map(|i| {
            let mut r1 = sources[0]
                .eval(i, getter, &mut cache, &EvalSettings { wrap: false })
                .unwrap();
            let r2 = sources[1]
                .eval(i, getter, &mut cache, &EvalSettings { wrap: false })
                .unwrap();
            exo_operations.insert((op, r1.clone(), r2.clone()));

            match op {
                ExoOperation::Add => {
                    r1.add_assign(&r2);
                }
                ExoOperation::Sub => {
                    r1.sub_assign(&r2);
                }
                ExoOperation::Mul => {
                    r1.mul_assign(&r2);
                }
            }
            r1
        })
        .collect();

    Ok(vec![(
        target.to_owned(),
        ValueBacking::from_vec(value, spilling),
    )])
}

fn compute_cyclic(
    cs: &ConstraintSet,
    froms: &[ColumnRef],
    to: &ColumnRef,
    modulo: usize,
) -> Result<Vec<ComputedColumn>> {
    let spilling = cs.spilling_for_column(&froms[0]).unwrap();
    for from in froms.iter() {
        ensure_is_computed(from, cs)?;
    }
    let len = cs.columns.len(&froms[0]).unwrap();
    if len < modulo {
        bail!(
            "unable to compute cyclic column {}: {} < {}",
            to.to_string().bold().white(),
            len,
            modulo
        )
    }

    let value: Vec<Value> = vec![Value::zero(); spilling as usize]
        .into_iter()
        .chain((0..len).map(|i| (i % modulo).into()))
        .collect();

    // TODO: replace with generator function
    Ok(vec![(
        to.to_owned(),
        ValueBacking::from_vec(value, spilling),
    )])
}

type ComputedColumn = (ColumnRef, ValueBacking);
pub fn compute_composite(
    cs: &ConstraintSet,
    exp: &Node,
    target: &ColumnRef,
) -> Result<Vec<ComputedColumn>> {
    let cols_in_expr = exp.dependencies();
    for from in &cols_in_expr {
        ensure_is_computed(from, cs)?;
    }

    let module = cs.columns.module_of(target);
    let spilling = cs.spilling_of(&module).unwrap();

    Ok(vec![(
        target.to_owned(),
        if let Result::Ok(cst) = exp.pure_eval() {
            let v = Value::from(cst);
            ValueBacking::from_fn(Box::new(move |_, _: &ColumnSet| Some(v.clone())), spilling)
        } else {
            let captured_exp = exp.clone();
            ValueBacking::from_expression(captured_exp, spilling)
        },
    )])
}

fn compute_sorting_auxs(cs: &ConstraintSet, comp: &Computation) -> Result<Vec<ComputedColumn>> {
    if let Computation::SortingConstraints {
        ats,
        eq,
        delta,
        delta_bytes,
        signs,
        froms,
        sorted,
    } = comp
    {
        assert!(delta_bytes.len() == 16);
        for from in froms.iter().chain(sorted.iter()) {
            ensure_is_computed(from, cs)?;
        }

        let spilling = cs.spilling_for_column(&froms[0]).unwrap();
        let len = cs.columns.len(&froms[0]).unwrap();

        let mut at_values = std::iter::repeat_with(|| vec![Value::zero(); spilling as usize])
            .take(ats.len())
            .collect::<Vec<_>>();
        // in the spilling, all @ == 0; thus Eq = 1
        let mut eq_values = vec![Value::one(); spilling as usize];
        let mut delta_values = vec![Value::zero(); spilling as usize];
        let mut delta_bytes_values =
            std::iter::repeat_with(|| vec![Value::zero(); spilling as usize])
                .take(delta_bytes.len())
                .collect::<Vec<_>>();
        for i in 0..len as isize {
            // Compute @s
            let mut found = false;
            for l in 0..ats.len() {
                let eq = cs
                    .columns
                    .get(&sorted[l], i, false)
                    .zip(cs.columns.get(&sorted[l], i - 1, false)) // may fail @0 if no padding; in this case, @ = 0
                    .map(|(v1, v2)| v1.eq(&v2))
                    .unwrap_or(true);

                let v = if !eq {
                    if found {
                        Value::zero()
                    } else {
                        found = true;
                        Value::one()
                    }
                } else {
                    Value::zero()
                };

                at_values[l].push(v);
            }

            // Compute Eq
            eq_values.push(if found { Value::zero() } else { Value::one() });

            // Compute Delta
            let mut delta = Value::zero();
            if eq_values.last().unwrap().is_zero() {
                for l in 0..ats.len() {
                    let mut term = cs.columns.get(&sorted[l], i, false).unwrap().clone();
                    term.sub_assign(&cs.columns.get(&sorted[l], i - 1, false).unwrap());
                    term.mul_assign(at_values[l].last().unwrap());
                    if !signs[l] {
                        term.negate();
                    }
                    delta.add_assign(&term);
                }
            }
            delta_values.push(delta.clone());

            delta
                .into_repr()
                .flat_map(|u| u.to_le_bytes().into_iter())
                .map(|i| Value::from(i as usize))
                .enumerate()
                .take(16)
                .for_each(|(i, b)| delta_bytes_values[i].push(b));
        }

        Ok(vec![
            (eq.to_owned(), ValueBacking::from_vec(eq_values, spilling)),
            (
                delta.to_owned(),
                ValueBacking::from_vec(delta_values, spilling),
            ),
        ]
        .into_iter()
        .chain(
            ats.iter()
                .zip(at_values.into_iter())
                .map(|(at, value)| (at.to_owned(), ValueBacking::from_vec(value, spilling))),
        )
        .chain(
            delta_bytes
                .iter()
                .zip(delta_bytes_values.into_iter())
                .map(|(delta_byte, value)| {
                    (
                        delta_byte.to_owned(),
                        ValueBacking::from_vec(value, spilling),
                    )
                }),
        )
        .collect())
    } else {
        unreachable!()
    }
}

pub fn apply_computation(
    cs: &ConstraintSet,
    computation: &Computation,
    exo_operations: &mut HashSet<(ExoOperation, Value, Value)>,
) -> Option<Result<Vec<ComputedColumn>>> {
    trace!("Computing {}", computation.pretty_target());
    match computation {
        Computation::Composite { target, exp } => {
            if !cs.columns.is_computed(target) {
                Some(compute_composite(cs, exp, target))
            } else {
                None
            }
        }
        Computation::Interleaved { target, froms } => {
            if !cs.columns.is_computed(target) {
                Some(compute_interleaved(cs, froms, target))
            } else {
                None
            }
        }
        Computation::Sorted { froms, tos, signs } => {
            if !cs.columns.is_computed(&tos[0]) {
                Some(compute_sorted(cs, froms, tos, signs))
            } else {
                None
            }
        }
        Computation::CyclicFrom {
            target,
            froms,
            modulo,
        } => {
            if !cs.columns.is_computed(target) {
                Some(compute_cyclic(cs, froms, target, *modulo))
            } else {
                None
            }
        }
        Computation::ExoOperation {
            op,
            sources,
            target,
        } => {
            if !cs.columns.is_computed(target) {
                let r = compute_exooperation(cs, *op, sources, target, exo_operations);
                Some(r)
            } else {
                None
            }
        }
        Computation::ExoConstant { value, target } => {
            if !cs.columns.is_computed(target) {
                Some(compute_exoconstant(cs, target, value))
            } else {
                None
            }
        }
        comp @ Computation::SortingConstraints { eq, .. } => {
            // NOTE all are computed at once, checking an arbitrary one (here
            // eq) is enough
            if !cs.columns.is_computed(eq) {
                Some(compute_sorting_auxs(cs, comp))
            } else {
                None
            }
        }
    }
}

fn err_missing_column<'a>(c: &crate::column::Column) -> RuntimeError<'a> {
    if matches!(c.kind, crate::compiler::Kind::Atomic) {
        RuntimeError::EmptyColumn(c.handle.clone())
    } else {
        RuntimeError::NotComputed(c.handle.clone())
    }
}

fn prepare(cs: &mut ConstraintSet, fail_on_missing: bool) -> Result<()> {
    compute_all(cs).with_context(|| "while computing columns")?;
    for h in cs.columns.all() {
        if !cs.columns.is_computed(&h) {
            let err = err_missing_column(cs.columns.column(&h).unwrap());
            if fail_on_missing {
                bail!(err)
            } else {
                error!("{}", err);
            }
        }
    }

    Ok(())
}

pub fn compute_trace(tracefile: &str, cs: &mut ConstraintSet, fail_on_missing: bool) -> Result<()> {
    import::read_trace(tracefile, cs)?;
    prepare(cs, fail_on_missing)
}

// This is only used by the lib
#[allow(dead_code)]
pub fn compute_trace_str(
    trace: &[u8],
    cs: &mut ConstraintSet,
    fail_on_missing: bool,
) -> Result<()> {
    import::read_trace_str(trace, cs)?;
    prepare(cs, fail_on_missing)
}
