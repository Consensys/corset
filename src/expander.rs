use num_bigint::BigInt;
use num_traits::{ToPrimitive, Zero};

use crate::{
    column::{ColumnSet, Computation},
    compiler::{
        Builtin, ComputationTable, Constraint, ConstraintSet, Expression, Handle, Kind, Magma, Type,
    },
};
use anyhow::Result;

const RESERVED_MODULE: &str = "RESERVED";

fn invert_expr(e: &Expression) -> Expression {
    Builtin::Inv.call(&[e.to_owned()])
}

fn validate_inv(cs: &mut Vec<Expression>, x_expr: &Expression, inv_x_col: &Handle) {
    cs.push(Builtin::Mul.call(&[
        x_expr.clone(),
        Builtin::Sub.call(&[
            Builtin::Mul.call(&[
                x_expr.clone(),
                Expression::Column(
                    inv_x_col.clone(),
                    Type::Column(Magma::Integer),
                    Kind::Composite(Box::new(Builtin::Inv.call(&[x_expr.clone()]))),
                ),
            ]),
            Expression::one(),
        ]),
    ]));
    cs.push(Builtin::Mul.call(&[
        Expression::Column(
            inv_x_col.clone(),
            Type::Column(Magma::Integer),
            Kind::Composite(Box::new(Builtin::Inv.call(&[x_expr.clone()]))),
        ),
        Builtin::Sub.call(&[
            Builtin::Mul.call(&[
                x_expr.clone(),
                Expression::Column(
                    inv_x_col.clone(),
                    Type::Column(Magma::Integer),
                    Kind::Composite(Box::new(Builtin::Inv.call(&[x_expr.clone()]))),
                ),
            ]),
            Expression::one(),
        ]),
    ]));
}

fn validate_computation(cs: &mut Vec<Expression>, x_expr: &Expression, x_col: &Handle) {
    cs.push(Builtin::Sub.call(&[
        x_expr.clone(),
        Expression::Column(
            x_col.to_owned(),
            Type::Column(Magma::Integer),
            Kind::Composite(Box::new(x_expr.clone())),
        ),
    ]))
}

fn expression_to_name(e: &Expression, prefix: &str) -> String {
    format!("{}_{}", prefix, e).replace(' ', "_")
}

fn wrap(ex: Expression) -> Expression {
    match ex {
        Expression::List(_) => ex,
        _ => Expression::List(vec![ex]),
    }
}

fn flatten_list(mut e: Expression) -> Expression {
    match e {
        Expression::List(ref mut xs) => {
            if xs.len() == 1 {
                flatten_list(xs.pop().unwrap())
            } else {
                e
            }
        }
        _ => e,
    }
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
                // If the condition reduces to a constant, we can determine the result
                if let Ok(constant_cond) = cond.pure_eval() {
                    match func {
                        Builtin::IfZero => {
                            if constant_cond.is_zero() {
                                *e = args[1].clone();
                            } else {
                                *e = flatten_list(
                                    args.get(2).cloned().unwrap_or_else(|| Expression::zero()),
                                );
                            }
                        }
                        Builtin::IfNotZero => {
                            if !constant_cond.is_zero() {
                                *e = args[1].clone();
                            } else {
                                *e = flatten_list(
                                    args.get(2).cloned().unwrap_or_else(|| Expression::zero()),
                                );
                            }
                        }
                        _ => unreachable!(),
                    }
                } else {
                    let conds = {
                        let cond_not_zero = cond.clone();
                        // If the condition is binary, cond_zero = 1 - x...
                        let cond_zero = if args[0].t().is_bool() {
                            Builtin::Sub.call(&[Expression::one(), cond])
                        } else {
                            // ...otherwise, cond_zero = 1 - x.INV(x)
                            Builtin::Sub.call(&[
                                Expression::one(),
                                Builtin::Mul.call(&[cond.clone(), Builtin::Inv.call(&[cond])]),
                            ])
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
                        // Ensure branches are wrapped in lists
                        .map(|(i, ex)| (i, wrap(ex.clone())))
                        // Map the corresponding then/else operations on the branches
                        .flat_map(|(i, exs)| {
                            if let Expression::List(exs) = exs {
                                exs.into_iter()
                                    .map(|ex: Expression| {
                                        ex.flat_fold(&|ex| {
                                            Builtin::Mul.call(&[conds[i].clone(), ex.clone()])
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
                    }
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

fn do_expand_expr<T: Clone + Ord>(
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

fn do_lower_shifts(e: &mut Expression, depth: isize) {
    match e {
        Expression::Funcall { func, args } => {
            if matches!(func, Builtin::Shift) {
                let shift = args[1].pure_eval().unwrap().to_isize().unwrap();
                *e = args[0].clone();
                do_lower_shifts(e, depth + shift);
            } else {
                for x in args.iter_mut() {
                    do_lower_shifts(x, depth)
                }
            }
        }
        Expression::Const(_, _) => (),
        Expression::Column(_, _, _) => {
            if depth != 0 {
                let column = e.clone();
                *e = Builtin::Shift.call(&[column, Expression::Const(BigInt::from(depth), None)])
            }
        }
        Expression::ArrayColumn(_, _, _) => (),
        Expression::List(xs) => {
            for x in xs.iter_mut() {
                do_lower_shifts(x, depth)
            }
        }
        Expression::Void => (),
    }
}

pub fn lower_shifts(cs: &mut ConstraintSet) {
    for c in cs.constraints.iter_mut() {
        match c {
            Constraint::Vanishes { expr: e, .. } => {
                do_lower_shifts(e, 0);
            }
            Constraint::Plookup(_name, parents, children) => {
                for e in parents.iter_mut().chain(children.iter_mut()) {
                    do_lower_shifts(e, 0);
                }
            }
            Constraint::Permutation(..) => (),
            Constraint::InRange(_, e, _) => {
                do_lower_shifts(e, 0);
            }
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
                    *e =
                        do_expand_expr(e, &mut cs.modules, &mut cs.computations, &mut new_cs_exps)?;
                }
            }
            Constraint::Permutation(..) => (),
            Constraint::InRange(_, e, _) => {
                *e = do_expand_expr(e, &mut cs.modules, &mut cs.computations, &mut new_cs_exps)?;
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

    cs.update_ids();

    Ok(())
}
