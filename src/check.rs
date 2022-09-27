use eyre::*;
use log::*;
use pairing_ce::ff::{Field, PrimeField};

use crate::compiler::{ConstraintSet, Expression};

pub fn check(cs: &ConstraintSet) -> Result<()> {
    for c in cs.constraints.iter() {
        match c {
            crate::compiler::Constraint::Vanishes { name, domain, expr } => {
                if matches!(**expr, Expression::Void) {
                    warn!("Ignoring Void expression {}", name);
                    continue;
                }

                match domain {
                    Some(is) => {
                        for i in is {
                            let r = expr.eval(
                                *i,
                                &mut |module, name, i, idx| {
                                    cs.columns
                                        .get(module, name)
                                        .unwrap()
                                        .get(i, idx)
                                        .unwrap()
                                        .cloned()
                                },
                                false,
                            );
                            println!("{:?} -> {:?}", expr, r.map(|r| r.into_repr().to_string()));
                        }
                    }
                    None => {
                        let cols_lens = expr
                            .dependencies()
                            .into_iter()
                            .map(|(module, name)| {
                                cs.columns.get(&module, &name).unwrap().len().unwrap()
                            })
                            .collect::<Vec<_>>();
                        if !cols_lens.iter().all(|&l| l == cols_lens[0]) {
                            bail!(
                                "all columns in `{:?}` are not of the same length: `{:?}`",
                                &expr,
                                expr.dependencies()
                                    .iter()
                                    .map(|(module, name)| format!(
                                        "{}/{}: {}",
                                        module,
                                        name,
                                        cs.columns.get(module, name).unwrap().len().unwrap()
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
                                    cs.columns
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
                                    error!(
                                        "{:?}|{}/{}\n -> {:?}",
                                        expr,
                                        i,
                                        l,
                                        r.into_repr().to_string()
                                    );
                                    let _ = expr.eval(
                                        i,
                                        &mut |module, name, i, idx| {
                                            cs.columns
                                                .get(module, name)
                                                .unwrap()
                                                .get(i, idx)
                                                .unwrap()
                                                .cloned()
                                        },
                                        true,
                                    );
                                }
                            } else {
                                info!("{} out of range for {:?}", i, expr)
                            }
                        }
                    }
                }
            }
            crate::compiler::Constraint::Plookup(_, _) => todo!(),
        }
    }
    Ok(())
}
