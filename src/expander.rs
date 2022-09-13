use convert_case::{Case, Casing};
use num_traits::One;

use crate::{
    column::ColumnSet,
    compiler::{Builtin, Constraint, ConstraintsSet, Expression, Kind, Type},
};
use eyre::*;

fn validate_inv(cs: &mut Vec<Expression>, x_expr: &Expression, inv_x_col: &str) {
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
                                inv_x_col.into(),
                                Type::Numeric,
                                Kind::Composite(Box::new(Expression::Funcall {
                                    func: Builtin::Inv,
                                    args: vec![x_expr.clone()],
                                })),
                            ),
                        ],
                    },
                    Expression::Const(One::one()),
                ],
            },
        ],
    });
    cs.push(Expression::Funcall {
        func: Builtin::Mul,
        args: vec![
            Expression::Column(
                inv_x_col.into(),
                Type::Numeric,
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
                                inv_x_col.into(),
                                Type::Numeric,
                                Kind::Composite(Box::new(Expression::Funcall {
                                    func: Builtin::Inv,
                                    args: vec![x_expr.clone()],
                                })),
                            ),
                        ],
                    },
                    Expression::Const(One::one()),
                ],
            },
        ],
    });
}

fn validate_plookup(cs: &mut Vec<Expression>, x_expr: &Expression, x_col: &str) {
    cs.push(Expression::Funcall {
        func: Builtin::Sub,
        args: vec![
            x_expr.clone(),
            Expression::Column(
                x_col.into(),
                Type::Numeric,
                Kind::Composite(Box::new(x_expr.clone())),
            ),
        ],
    })
}

fn expression_to_name(e: &Expression, prefix: &str) -> String {
    format!("%%{}_{:?}%%", prefix, e).to_case(Case::ScreamingSnake)
}

fn expand_expr<T: Copy + Ord>(
    e: &mut Expression,
    cols: &mut ColumnSet<T>,
    new_cs: &mut Vec<Expression>,
) -> Result<()> {
    match e {
        Expression::List(es) => {
            for e in es.iter_mut() {
                expand_expr(e, cols, new_cs)?;
            }
            Ok(())
        }
        Expression::Funcall { func, args, .. } => {
            for e in args.iter_mut() {
                expand_expr(e, cols, new_cs)?;
            }
            if matches!(func, Builtin::Inv) {
                let inverted = &mut args[0];
                let inv_colname = expression_to_name(inverted, "INV");
                validate_inv(new_cs, inverted, &inv_colname);
                cols.insert_composite(&inv_colname, inverted, true)?;
                *e = Expression::Column(
                    inv_colname,
                    Type::Numeric,
                    Kind::Composite(Box::new(inverted.clone())),
                )
            }
            Ok(())
        }
        _ => Ok(()),
    }
}

fn expand_plookup<T: Ord + Copy>(
    e: &Expression,
    cols: &mut ColumnSet<T>,
    new_cs: &mut Vec<Expression>,
) -> Result<()> {
    match e {
        Expression::Column(..) => Ok(()),
        e => {
            let plookup_colname = expression_to_name(e, "PLKP");
            validate_plookup(new_cs, e, &plookup_colname);
            cols.insert_composite(plookup_colname, e, true)?;
            Ok(())
        }
    }
}

pub fn expand(cs: &mut ConstraintsSet) -> Result<()> {
    let mut new_cs_inv = vec![];
    let mut new_cs_plookup = vec![];
    for c in cs.constraints.iter_mut() {
        match c {
            Constraint::Vanishes { expr: e, .. } => {
                expand_expr(e, &mut cs.columns, &mut new_cs_inv)?;
            }
            Constraint::Plookup(parents, children) => {
                for e in parents.iter().chain(children.iter()) {
                    expand_plookup(e, &mut cs.columns, &mut new_cs_plookup)?;
                }
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
    if !new_cs_plookup.is_empty() {
        cs.constraints.push(Constraint::Vanishes {
            name: "PLOOKUPS_CONSTRAINTS".into(),
            domain: None,
            expr: Expression::List(new_cs_plookup).into(),
        });
    }

    Ok(())
}