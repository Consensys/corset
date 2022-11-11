use num_traits::One;
use pairing_ce::{bn256::Fr, ff::Field};

use crate::{
    column::{ColumnSet, Computation},
    compiler::{
        Builtin, ComputationTable, Constraint, ConstraintSet, Expression, Handle, Kind, Magma, Type,
    },
};
use anyhow::Result;

const RESERVED_MODULE: &str = "RESERVED";

fn invert_expr(e: &Expression) -> Expression {
    Expression::Funcall {
        func: Builtin::Inv,
        args: vec![e.to_owned()],
    }
}

fn validate_inv(cs: &mut Vec<Expression>, x_expr: &Expression, inv_x_col: &Handle) {
    cs.push(Expression::Funcall {
        func: Builtin::Mul,
        args: vec![
            x_expr.clone(),
            Expression::Funcall {
                func: Builtin::Sub,
                args: vec![
                    Expression::Funcall {
                        func: Builtin::Mul,
                        args: vec![
                            x_expr.clone(),
                            Expression::Column(
                                inv_x_col.clone(),
                                Type::Column(Magma::Integer),
                                Kind::Composite(Box::new(Expression::Funcall {
                                    func: Builtin::Inv,
                                    args: vec![x_expr.clone()],
                                })),
                            ),
                        ],
                    },
                    Expression::Const(One::one(), Some(Fr::one())),
                ],
            },
        ],
    });
    cs.push(Expression::Funcall {
        func: Builtin::Mul,
        args: vec![
            Expression::Column(
                inv_x_col.clone(),
                Type::Column(Magma::Integer),
                Kind::Composite(Box::new(Expression::Funcall {
                    func: Builtin::Inv,
                    args: vec![x_expr.clone()],
                })),
            ),
            Expression::Funcall {
                func: Builtin::Sub,
                args: vec![
                    Expression::Funcall {
                        func: Builtin::Mul,
                        args: vec![
                            x_expr.clone(),
                            Expression::Column(
                                inv_x_col.clone(),
                                Type::Column(Magma::Integer),
                                Kind::Composite(Box::new(Expression::Funcall {
                                    func: Builtin::Inv,
                                    args: vec![x_expr.clone()],
                                })),
                            ),
                        ],
                    },
                    Expression::Const(One::one(), Some(Fr::one())),
                ],
            },
        ],
    });
}

fn validate_computation(cs: &mut Vec<Expression>, x_expr: &Expression, x_col: &Handle) {
    cs.push(Expression::Funcall {
        func: Builtin::Sub,
        args: vec![
            x_expr.clone(),
            Expression::Column(
                x_col.to_owned(),
                Type::Column(Magma::Integer),
                Kind::Composite(Box::new(x_expr.clone())),
            ),
        ],
    })
}

fn expression_to_name(e: &Expression, prefix: &str) -> String {
    format!("{}_{}", prefix, e).replace(' ', "_")
}

fn do_expand_ifs(e: &mut Expression) {
    match e {
        Expression::List(es) => {
            for e in es.iter_mut() {
                do_expand_ifs(e);
            }
        }
        Expression::Funcall { func, args, .. } => {
            for e in args.iter_mut() {
                do_expand_ifs(e);
            }
            if matches!(func, Builtin::IfZero | Builtin::IfNotZero) {
                let cond = args[0].clone();
                let conds = {
                    let cond_not_zero = cond.clone();
                    // If the condition is binary, cond_zero = 1 - x...
                    let cond_zero = if args[0].t().is_bool() {
                        Expression::Funcall {
                            func: Builtin::Sub,
                            args: vec![Expression::Const(One::one(), Some(Fr::one())), cond],
                        }
                    } else {
                        // ...otherwise, cond_zero = 1 - x.INV(x)
                        Expression::Funcall {
                            func: Builtin::Sub,
                            args: vec![
                                Expression::Const(One::one(), Some(Fr::one())),
                                Expression::Funcall {
                                    func: Builtin::Mul,
                                    args: vec![
                                        cond.clone(),
                                        Expression::Funcall {
                                            func: Builtin::Inv,
                                            args: vec![cond],
                                        },
                                    ],
                                },
                            ],
                        }
                    };
                    match func {
                        Builtin::IfZero => [cond_zero, cond_not_zero],
                        Builtin::IfNotZero => [cond_not_zero, cond_zero],
                        _ => unreachable!(),
                    }
                };

                // Order the then/else blocks
                let then_else = vec![args.get(1), args.get(2)]
                    .into_iter()
                    .enumerate()
                    // Only keep the non-empty branches
                    .filter_map(|(i, ex)| ex.map(|ex| (i, ex)))
                    // Ensure branches are wrapped in in lists
                    .map(|(i, ex)| {
                        (
                            i,
                            match ex {
                                Expression::List(_) => ex.clone(),
                                ex => Expression::List(vec![ex.clone()]),
                            },
                        )
                    })
                    // Map the corresponding then/else operations on the branches
                    .flat_map(|(i, exs)| {
                        if let Expression::List(exs) = exs {
                            exs.into_iter()
                                .map(|ex: Expression| {
                                    ex.flat_fold(&|ex| Expression::Funcall {
                                        func: Builtin::Mul,
                                        args: vec![conds[i].clone(), ex.clone()],
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
                    Expression::List(then_else)
                };
            }
        }
        _ => (),
    }
}

/// For all Builtin::Inv encountered, create a new column and the associated constraints
/// pre-computing and proving the inverted column.
fn expand_inv<T: Clone + Ord>(
    e: &mut Expression,
    cols: &mut ColumnSet<T>,
    comps: &mut ComputationTable,
    new_cs: &mut Vec<Expression>,
) -> Result<()> {
    match e {
        Expression::List(es) => {
            for e in es.iter_mut() {
                expand_inv(e, cols, comps, new_cs)?;
            }
            Ok(())
        }
        Expression::Funcall { func, args, .. } => {
            for e in args.iter_mut() {
                expand_inv(e, cols, comps, new_cs)?;
            }
            if matches!(func, Builtin::Inv) {
                let inverted = &mut args[0];
                let inverted_handle =
                    Handle::new(RESERVED_MODULE, expression_to_name(inverted, "INV"));
                if cols.get(&inverted_handle).is_err() {
                    validate_inv(new_cs, inverted, &inverted_handle);
                    cols.insert_column(
                        &inverted_handle,
                        Type::Column(Magma::Integer),
                        Kind::Composite(()),
                        true,
                    )?;
                    comps.insert(
                        &inverted_handle,
                        Computation::Composite {
                            target: inverted_handle.clone(),
                            exp: invert_expr(inverted),
                        },
                    )?;
                }
                *e = Expression::Column(
                    inverted_handle.clone(),
                    Type::Column(Magma::Integer),
                    Kind::Atomic,
                )
            }
            Ok(())
        }
        _ => Ok(()),
    }
}

fn expand_expr<T: Clone + Ord>(
    e: &Expression,
    cols: &mut ColumnSet<T>,
    comps: &mut ComputationTable,
    new_cs: &mut Vec<Expression>,
) -> Result<Expression> {
    match e {
        Expression::Column(..) => Ok(e.clone()),
        e => {
            let new_handle = Handle::new(RESERVED_MODULE, expression_to_name(e, "EXPAND"));
            validate_computation(new_cs, e, &new_handle);
            cols.insert_column(
                &new_handle,
                Type::Column(Magma::Integer),
                Kind::Phantom,
                true,
            )?;

            let _ = comps.insert(
                &new_handle,
                Computation::Composite {
                    target: new_handle.clone(),
                    exp: e.clone(),
                },
            );
            Ok(Expression::Column(
                new_handle,
                Type::Column(Magma::Integer),
                Kind::Phantom,
            ))
        }
    }
}

pub fn expand_ifs(cs: &mut ConstraintSet) {
    for c in cs.constraints.iter_mut() {
        if let Constraint::Vanishes { expr: e, .. } = c {
            do_expand_ifs(e); // Ifs create Inv and must be expanded first
        }
    }
}

pub fn expand(cs: &mut ConstraintSet) -> Result<()> {
    let mut new_cs_inv = vec![];
    let mut new_cs_exps = vec![];
    for c in cs.constraints.iter_mut() {
        match c {
            Constraint::Vanishes { expr: e, .. } => {
                expand_inv(e, &mut cs.modules, &mut cs.computations, &mut new_cs_inv)?;
            }
            Constraint::Plookup(_name, parents, children) => {
                for e in parents.iter_mut().chain(children.iter_mut()) {
                    *e = expand_expr(e, &mut cs.modules, &mut cs.computations, &mut new_cs_exps)?;
                }
            }
            Constraint::Permutation(..) => (),
            Constraint::InRange(_, e, _) => {
                *e = expand_expr(e, &mut cs.modules, &mut cs.computations, &mut new_cs_exps)?;
            }
        }
    }
    if !new_cs_inv.is_empty() {
        cs.constraints.push(Constraint::Vanishes {
            name: "INV_CONSTRAINTS".into(),
            domain: None,
            expr: Expression::List(new_cs_inv).into(),
        });
    }
    if !new_cs_exps.is_empty() {
        cs.constraints.push(Constraint::Vanishes {
            name: "EXPANSION_CONSTRAINTS".into(),
            domain: None,
            expr: Expression::List(new_cs_exps).into(),
        });
    }

    Ok(())
}
