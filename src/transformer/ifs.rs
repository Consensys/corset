use anyhow::Result;
use num_traits::Zero;

use crate::compiler::{Constraint, ConstraintSet, Expression, Intrinsic, Node};

use super::{flatten_list, wrap};

fn do_expand_ifs(e: &mut Node) -> Result<()> {
    match e.e_mut() {
        Expression::List(es) => {
            for e in es.iter_mut() {
                do_expand_ifs(e)?;
            }
        }
        Expression::Funcall { func, args, .. } => {
            for e in args.iter_mut() {
                do_expand_ifs(e)?;
            }
            if matches!(func, Intrinsic::IfZero | Intrinsic::IfNotZero) {
                let cond = args[0].clone();
                // If the condition reduces to a constant, we can determine the result
                if let Ok(constant_cond) = cond.pure_eval() {
                    match func {
                        Intrinsic::IfZero => {
                            if constant_cond.is_zero() {
                                *e = args[1].clone();
                            } else {
                                *e = flatten_list(args.get(2).cloned().unwrap_or_else(Node::zero));
                            }
                        }
                        Intrinsic::IfNotZero => {
                            if !constant_cond.is_zero() {
                                *e = args[1].clone();
                            } else {
                                *e = flatten_list(args.get(2).cloned().unwrap_or_else(Node::zero));
                            }
                        }
                        _ => unreachable!(),
                    }
                } else {
                    let conds = {
                        let cond_not_zero = cond.clone();
                        // If the condition is binary, cond_zero = 1 - x...
                        let cond_zero = if args[0].t().is_bool() {
                            Intrinsic::Sub.call(&[Node::one(), cond])?
                        } else {
                            // ...otherwise, cond_zero = 1 - x.INV(x)
                            Intrinsic::Sub.call(&[
                                Node::one(),
                                Intrinsic::Mul
                                    .call(&[cond.clone(), Intrinsic::Inv.call(&[cond])?])?,
                            ])?
                        };
                        match func {
                            Intrinsic::IfZero => [cond_zero, cond_not_zero],
                            Intrinsic::IfNotZero => [cond_not_zero, cond_zero],
                            _ => unreachable!(),
                        }
                    };

                    // Order the then/else blocks
                    let then_else = vec![args.get(1), args.get(2)]
                        .into_iter()
                        .enumerate()
                        // Only keep the non-empty branches
                        .filter_map(|(i, ex)| ex.map(|ex| (i, ex)))
                        // Ensure branches are wrapped in lists
                        .map(|(i, ex)| (i, wrap(ex.clone())))
                        // Map the corresponding then/else operations on the branches
                        .flat_map(|(i, exs)| {
                            if let Expression::List(exs) = exs.e() {
                                exs.iter()
                                    .map(|ex: &Node| {
                                        ex.flat_map(&|e| {
                                            Intrinsic::Mul
                                                .call(&[conds[i].clone(), e.clone()])
                                                .unwrap()
                                        })
                                    })
                                    .collect::<Vec<_>>()
                            } else {
                                unreachable!()
                            }
                        })
                        .flatten()
                        .collect::<Vec<_>>();
                    *e = if then_else.len() == 1 {
                        then_else[0].clone()
                    } else {
                        Node::from_expr(Expression::List(then_else))
                    }
                };
            }
        }
        _ => (),
    }

    Ok(())
}

fn raise_ifs(mut e: Node) -> Node {
    match e.e_mut() {
        Expression::Funcall { func, ref mut args } => {
            *args = args.iter_mut().map(|a| raise_ifs(a.clone())).collect();

            match func {
                Intrinsic::Add | Intrinsic::Sub | Intrinsic::Mul => {
                    for (i, a) in args.iter().enumerate() {
                        if let Expression::Funcall {
                            func: func_if @ (Intrinsic::IfZero | Intrinsic::IfNotZero),
                            args: args_if,
                        } = a.e()
                        {
                            let cond = args_if[0].clone();
                            let new_then = func
                                .call(
                                    &args
                                        .iter()
                                        .take(i)
                                        .chain(std::iter::once(&args_if[1]))
                                        .cloned()
                                        .collect::<Vec<_>>(),
                                )
                                .unwrap();
                            let new_else = func
                                .call(
                                    &args
                                        .iter()
                                        .take(i)
                                        .cloned()
                                        .chain(std::iter::once(
                                            args_if.get(2).cloned().unwrap_or_else(|| {
                                                Node::from_expr(Expression::Void)
                                            }),
                                        ))
                                        .filter(|e| !matches!(e.e(), Expression::Void))
                                        .collect::<Vec<_>>(),
                                )
                                .unwrap();

                            let new_e = raise_ifs(
                                func_if
                                    .call(&[cond, new_then, new_else])
                                    .unwrap()
                                    .with_type(a.t()),
                            );
                            return new_e;
                        }
                    }
                    e
                }
                Intrinsic::IfZero
                | Intrinsic::IfNotZero
                | Intrinsic::Neg
                | Intrinsic::Inv
                | Intrinsic::Normalize
                | Intrinsic::Exp
                | Intrinsic::Begin => e,
            }
        }
        Expression::List(xs) => {
            for x in xs.iter_mut() {
                *x = raise_ifs(x.clone());
            }
            e
        }
        _ => e,
    }
}

pub fn expand_ifs(cs: &mut ConstraintSet) {
    for c in cs.constraints.iter_mut() {
        if let Constraint::Vanishes { expr, .. } = c {
            *expr = Box::new(raise_ifs(*expr.clone()));
        }
    }
    for c in cs.constraints.iter_mut() {
        if let Constraint::Vanishes { expr: e, .. } = c {
            do_expand_ifs(e).unwrap();
        }
    }
}
