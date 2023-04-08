use anyhow::{anyhow, bail, Context, Result};
use colored::Colorize;
use log::*;
use logging_timer::time;
use pairing_ce::{
    bn256::Fr,
    ff::{Field, PrimeField},
};
use rayon::prelude::*;
use std::{cmp::Ordering, collections::HashSet};

use crate::{
    column::Computation,
    compiler::{ConstraintSet, EvalSettings, Node},
    dag::ComputationDag,
    errors::RuntimeError,
    import,
    pretty::Pretty,
    structs::Handle,
};

#[time("info", "Computing expanded columns")]
fn compute_all(cs: &mut ConstraintSet) -> Result<()> {
    let mut jobs: ComputationDag = Default::default();
    for m in cs
        .modules
        .cols
        .keys()
        .cloned()
        .collect::<Vec<_>>()
        .into_iter()
    {
        cs.spilling_or_insert(&m);
    }
    for c in cs.computations.iter() {
        jobs.insert_computation(c)
    }

    let todos = jobs.job_slices();
    for slice in todos {
        let comps = slice
            .iter()
            .filter_map(|h| cs.computations.computation_idx_for(h))
            .collect::<HashSet<_>>()
            .iter()
            .map(|i| cs.computations.get(*i).unwrap().to_owned())
            .collect::<Vec<_>>();

        comps
            .into_par_iter()
            .filter_map(|comp| apply_computation(cs, &comp))
            .collect::<Vec<_>>()
            .into_iter()
            .for_each(|r| match r {
                Ok(xs) => xs.into_iter().for_each(|(h, v, spilling)| {
                    trace!("Filling {}", h.pretty());
                    cs.get_mut(&h).unwrap().set_raw_value(v, spilling);
                }),
                Err(e) => warn!("{}", e),
            });
    }

    Ok(())
}

fn ensure_is_computed(h: &Handle, cs: &ConstraintSet) -> Result<()> {
    let c = cs.get(h)?;
    if !c.is_computed() {
        bail!(err_missing_column(c, h))
    }
    Ok(())
}

fn compute_interleaved(
    cs: &ConstraintSet,
    froms: &[Handle],
    target: &Handle,
) -> Result<Vec<ComputedColumn>> {
    for from in froms.iter() {
        ensure_is_computed(from, cs)?;
    }

    if !froms
        .iter()
        .map(|h| cs.get(h).unwrap().len().unwrap())
        .collect::<Vec<_>>()
        .windows(2)
        .all(|w| w[0] == w[1])
    {
        bail!("interleaving columns of incoherent lengths")
    }

    let final_len = froms
        .iter()
        .map(|h| cs.get(h).unwrap().len().unwrap())
        .sum();
    let count = froms.len();
    let values = (0..final_len)
        .map(|k| {
            let i = k / count;
            let j = k % count;
            *cs.modules
                .get(&froms[j])
                .unwrap()
                .get(i as isize, false)
                .unwrap()
        })
        .collect();

    Ok(vec![(target.to_owned(), values, 0)])
}

fn compute_sorted(
    cs: &ConstraintSet,
    froms: &[Handle],
    tos: &[Handle],
    signs: &[bool],
) -> Result<Vec<ComputedColumn>> {
    let spilling = cs.spilling(&froms[0].module).unwrap();
    for from in froms.iter() {
        ensure_is_computed(from, cs)?;
    }

    let from_cols = froms.iter().map(|c| cs.get(c).unwrap()).collect::<Vec<_>>();

    if !from_cols
        .windows(2)
        .all(|w| w[0].padded_len() == w[1].padded_len())
    {
        bail!("sorted columns are of incoherent lengths")
    }
    let len = from_cols[0].len().unwrap();

    let mut sorted_is = (0..len).collect::<Vec<_>>();
    sorted_is.sort_by(|i, j| {
        for (sign, from) in signs.iter().zip(from_cols.iter()) {
            let x_i = from.get(*i as isize, false).unwrap();
            let x_j = from.get(*j as isize, false).unwrap();
            if let x @ (Ordering::Greater | Ordering::Less) = x_i.cmp(x_j) {
                return if *sign { x } else { x.reverse() };
            }
        }
        Ordering::Equal
    });

    Ok(froms
        .iter()
        .enumerate()
        .map(|(k, from)| {
            let value: Vec<Fr> = vec![Fr::zero(); spilling as usize]
                .into_iter()
                .chain(sorted_is.iter().map(|i| {
                    *cs.get(from)
                        .unwrap()
                        .get((*i).try_into().unwrap(), false)
                        .unwrap()
                }))
                .collect();

            (tos[k].to_owned(), value, spilling)
        })
        .collect::<Vec<_>>())
}

fn compute_cyclic(
    cs: &ConstraintSet,
    froms: &[Handle],
    to: &Handle,
    modulo: usize,
) -> Result<Vec<ComputedColumn>> {
    let spilling = cs.spilling(&froms[0].module).unwrap();
    for from in froms.iter() {
        ensure_is_computed(from, cs)?;
    }
    let len = cs.get(&froms[0]).unwrap().len().unwrap();
    if len < modulo {
        bail!(
            "unable to compute cyclic column {}: {} < {}",
            to.to_string().bold().white(),
            len,
            modulo
        )
    }

    let value: Vec<Fr> = vec![Fr::zero(); spilling as usize]
        .into_iter()
        .chain((0..len).map(|i| Fr::from_str(&((i % modulo).to_string())).unwrap()))
        .collect();

    Ok(vec![(to.to_owned(), value, spilling)])
}

type ComputedColumn = (Handle, Vec<Fr>, isize);
pub fn compute_composite(
    cs: &ConstraintSet,
    exp: &Node,
    target: &Handle,
) -> Result<Vec<ComputedColumn>> {
    let spilling = cs.spilling(&target.module).unwrap();
    let cols_in_expr = exp.dependencies();
    for from in &cols_in_expr {
        ensure_is_computed(from, cs)?;
    }
    let length = *cols_in_expr
        .iter()
        .map(|handle| Ok(cs.get(handle).unwrap().len().unwrap().to_owned()))
        .collect::<Result<Vec<_>>>()?
        .iter()
        .max()
        .unwrap();

    let mut cache = Some(cached::SizedCache::with_size(200000)); // ~1.60MB cache
    let values = (-spilling..length as isize)
        .map(|i| {
            exp.eval(
                i,
                &mut |handle, j, _| cs.modules._cols[handle.id.unwrap()].get(j, false).cloned(),
                &mut cache,
                &EvalSettings { wrap: false },
            )
            .unwrap_or_else(Fr::zero)
        })
        .collect();

    Ok(vec![(target.to_owned(), values, spilling)])
}

pub fn compute_composite_static(cs: &ConstraintSet, exp: &Node) -> Result<Vec<Fr>> {
    let cols_in_expr = exp.dependencies();
    for c in &cols_in_expr {
        ensure_is_computed(c, cs)?;
    }

    let length = *cols_in_expr
        .iter()
        .map(|handle| {
            Ok(cs
                .get(handle)
                .with_context(|| anyhow!("while reading {}", handle.to_string().red().bold()))?
                .len()
                .ok_or_else(|| anyhow!("{} has no len", handle.to_string().red().bold()))?
                .to_owned())
        })
        .collect::<Result<Vec<_>>>()?
        .iter()
        .max()
        .unwrap();

    let mut cache = Some(cached::SizedCache::with_size(200000)); // ~1.60MB cache
    let values = (0..length as isize)
        .map(|i| {
            exp.eval(
                i,
                &mut |handle, j, _| cs.modules._cols[handle.id.unwrap()].get(j, false).cloned(),
                &mut cache,
                &EvalSettings { wrap: false },
            )
            .unwrap_or_else(Fr::zero)
        })
        .collect::<Vec<_>>();

    Ok(values)
}

fn compute_sorting_auxs(
    cs: &ConstraintSet,
    ats: &[Handle],
    eq: &Handle,
    delta: &Handle,
    delta_bytes: &[Handle],
    signs: &[bool],
    from: &[Handle],
    sorted: &[Handle],
) -> Result<Vec<ComputedColumn>> {
    assert!(delta_bytes.len() == 16);
    for from in from.iter().chain(sorted.iter()) {
        ensure_is_computed(from, cs)?;
    }

    let spilling = cs.spilling(&from[0].module).unwrap();
    let len = cs.modules.by_handle(&from[0]).unwrap().len().unwrap();

    let mut at_values = std::iter::repeat_with(|| vec![Fr::zero(); spilling as usize])
        .take(ats.len())
        .collect::<Vec<_>>();
    // in the spilling, all @ == 0; thus Eq = 1
    let mut eq_values = vec![Fr::one(); spilling as usize];
    let mut delta_values = vec![Fr::zero(); spilling as usize];
    let mut delta_bytes_values = std::iter::repeat_with(|| vec![Fr::zero(); spilling as usize])
        .take(delta_bytes.len())
        .collect::<Vec<_>>();
    let sorted_cols = sorted
        .iter()
        .map(|f| {
            cs.modules
                .by_handle(f)
                .ok_or_else(|| anyhow!("column `{}` not found", f))
        })
        .collect::<Result<Vec<_>>>()?;
    for i in 0..len as isize {
        // Compute @s
        let mut found = false;
        for l in 0..ats.len() {
            let eq = sorted_cols[l]
                .get(i, false)
                .zip(sorted_cols[l].get(i - 1, false)) // may fail @0 if no padding; in this case, @ = 0
                .map(|(v1, v2)| v1.eq(v2))
                .unwrap_or(true);

            let v = if !eq {
                if found {
                    Fr::zero()
                } else {
                    found = true;
                    Fr::one()
                }
            } else {
                Fr::zero()
            };

            at_values[l].push(v);
        }

        // Compute Eq
        eq_values.push(if found { Fr::zero() } else { Fr::one() });

        // Compute Delta
        let mut delta = Fr::zero();
        if eq_values.last().unwrap().is_zero() {
            for l in 0..ats.len() {
                let mut term = *sorted_cols[l].get(i, false).unwrap();
                term.sub_assign(sorted_cols[l].get(i - 1, false).unwrap());
                term.mul_assign(at_values[l].last().unwrap());
                if !signs[l] {
                    term.negate();
                }
                delta.add_assign(&term);
            }
        }
        // delta.sub_assign(&Fr::one());
        delta_values.push(delta);

        delta
            .into_repr()
            .as_ref()
            .iter()
            .flat_map(|u| u.to_le_bytes().into_iter())
            .map(|i| Fr::from_str(&i.to_string()).unwrap())
            .enumerate()
            .take(16)
            .for_each(|(i, b)| delta_bytes_values[i].push(b));
    }

    Ok(vec![
        (eq.to_owned(), eq_values, spilling),
        (delta.to_owned(), delta_values, spilling),
    ]
    .into_iter()
    .chain(
        ats.iter()
            .zip(at_values.into_iter())
            .map(|(at, value)| (at.to_owned(), value, spilling)),
    )
    .chain(
        delta_bytes
            .iter()
            .zip(delta_bytes_values.into_iter())
            .map(|(delta_byte, value)| (delta_byte.to_owned(), value, spilling)),
    )
    .collect())
}

pub fn apply_computation(
    cs: &ConstraintSet,
    computation: &Computation,
) -> Option<Result<Vec<ComputedColumn>>> {
    trace!("Computing {}", computation.pretty_target());
    match computation {
        Computation::Composite { target, exp } => {
            if !cs.get(target).unwrap().is_computed() {
                Some(compute_composite(cs, exp, target))
            } else {
                None
            }
        }
        Computation::Interleaved { target, froms } => {
            if !cs.get(target).unwrap().is_computed() {
                Some(compute_interleaved(cs, froms, target))
            } else {
                None
            }
        }
        Computation::Sorted { froms, tos, signs } => {
            if !cs.get(&tos[0]).unwrap().is_computed() {
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
            if !cs.get(target).unwrap().is_computed() {
                Some(compute_cyclic(cs, froms, target, *modulo))
            } else {
                None
            }
        }
        Computation::SortingConstraints {
            ats,
            eq,
            delta,
            delta_bytes,
            signs,
            froms,
            sorted,
        } => {
            // NOTE all are computed at once, no need to check all of them
            if !cs.get(eq).unwrap().is_computed() {
                Some(compute_sorting_auxs(
                    cs,
                    ats,
                    eq,
                    delta,
                    delta_bytes,
                    signs,
                    froms,
                    sorted,
                ))
            } else {
                None
            }
        }
    }
}

fn err_missing_column<'a>(c: &crate::column::Column, h: &Handle) -> RuntimeError<'a> {
    if matches!(c.kind, crate::compiler::Kind::Atomic) {
        RuntimeError::EmptyColumn(h.clone())
    } else {
        RuntimeError::NotComputed(h.clone())
    }
}

fn prepare(cs: &mut ConstraintSet, fail_on_missing: bool) -> Result<()> {
    compute_all(cs).with_context(|| "while computing columns")?;
    for h in cs.modules.handles() {
        let column = cs.get(&h).unwrap();
        if !column.is_computed() {
            let err = err_missing_column(column, &h);
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

pub fn compute_trace_str(
    trace: &[u8],
    cs: &mut ConstraintSet,
    fail_on_missing: bool,
) -> Result<()> {
    import::read_trace_str(trace, cs)?;
    prepare(cs, fail_on_missing)
}
