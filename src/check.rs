use std::collections::HashSet;

use eyre::*;
use log::*;
use pairing_ce::{bn256::Fr, ff::Field};

use crate::{
    column::ColumnSet,
    compiler::{ConstraintSet, Expression},
    utils::*,
};

fn check_constraint(
    expr: &Expression,
    domain: &Option<Vec<isize>>,
    columns: &ColumnSet<Fr>,
) -> Result<()> {
    match domain {
        Some(is) => {
            for i in is {
                let r = expr.eval(
                    *i,
                    &mut |module, name, i, idx| {
                        columns
                            .get(module, name)
                            .unwrap()
                            .get(i, idx)
                            .unwrap()
                            .cloned()
                    },
                    false,
                );
                if let Some(x) = r {
                    if !x.is_zero() {
                        return Err(eyre!("Should vanish: {:?}", r));
                    }
                } else {
                    return Err(eyre!("{} out of range for {:?}/{:?}: {:?}", i, expr, is, r));
                }
            }
            Ok(())
        }
        None => {
            let cols_lens = expr
                .dependencies()
                .into_iter()
                .map(|(module, name)| columns.get(&module, &name).unwrap().len().unwrap())
                .collect::<Vec<_>>();
            if !cols_lens.iter().all(|&l| l == cols_lens[0]) {
                error!(
                    "all columns in `{:?}` are not of the same length: `{:?}`",
                    &expr,
                    expr.dependencies()
                        .iter()
                        .map(|(module, name)| format!(
                            "{}/{}: {}",
                            module,
                            name,
                            columns.get(module, name).unwrap().len().unwrap()
                        ))
                        .collect::<Vec<_>>()
                        .join(", ")
                );
            }
            let l = cols_lens[0];
            for i in 0..l as isize {
                let r = expr.eval(
                    i,
                    &mut |module, name, i, idx| {
                        columns
                            .get(module, name)
                            .unwrap()
                            .get(i, idx)
                            .unwrap()
                            .cloned()
                    },
                    false,
                );
                if let Some(r) = r {
                    if !r.is_zero() {
                        let _ = expr.eval(
                            i,
                            &mut |module, name, i, idx| {
                                columns
                                    .get(module, name)
                                    .unwrap()
                                    .get(i, idx)
                                    .unwrap()
                                    .cloned()
                            },
                            true,
                        );
                        return Err(eyre!("{:?}|{}/{}\n -> {}", expr, i, l, pretty(&r),));
                    }
                } else {
                    info!("{} out of range for {:?}", i, expr);
                }
            }
            Ok(())
        }
    }
}

pub fn check(cs: &ConstraintSet) -> Result<()> {
    let mut failed = HashSet::new();
    for c in cs.constraints.iter() {
        match c {
            crate::compiler::Constraint::Vanishes { name, domain, expr } => {
                if matches!(**expr, Expression::Void) {
                    warn!("Ignoring Void expression {}", name);
                    continue;
                }

                match expr.as_ref() {
                    Expression::List(es) => {
                        for e in es {
                            if check_constraint(e, domain, &cs.columns).is_err() {
                                failed.insert(name.to_owned());
                            };
                        }
                    }
                    _ => {
                        if check_constraint(expr, domain, &cs.columns).is_err() {
                            failed.insert(name.to_owned());
                        }
                    }
                }
            }
            crate::compiler::Constraint::Plookup(_, _) => todo!(),
        }
    }
    if failed.is_empty() {
        Ok(())
    } else {
        Err(eyre!(
            "Constraints failed: {}",
            failed.into_iter().collect::<Vec<_>>().join(", ")
        ))
    }
}
