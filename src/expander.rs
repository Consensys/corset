use num_traits::One;
use pairing_ce::{bn256::Fr, ff::Field};

use crate::{
    column::{ColumnSet, Computation},
    compiler::{
        Builtin, ComputationTable, Constraint, ConstraintSet, Expression, Handle, Kind, Magma, Type,
    },
};
use eyre::*;

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
