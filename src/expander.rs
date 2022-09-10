use convert_case::{Case, Casing};

use crate::{
    column::Column,
    compiler::{Builtin, Columns, Constraint, ConstraintsSet, Expression},
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
                        args: vec![x_expr.clone(), Expression::Column(inv_x_col.into())],
                    },
                    Expression::Const(1),
                ],
            },
        ],
    });
    cs.push(Expression::Funcall {
        func: Builtin::Mul,
        args: vec![
            Expression::Column(inv_x_col.into()),
            Expression::Funcall {
                func: Builtin::Sub,
                args: vec![
                    Expression::Funcall {
                        func: Builtin::Mul,
                        args: vec![x_expr.clone(), Expression::Column(inv_x_col.into())],
                    },
                    Expression::Const(1),
                ],
            },
        ],
    });
}

fn validate_plookup(cs: &mut Vec<Expression>, x_expr: &Expression, x_col: &str) {
    cs.push(Expression::Funcall {
        func: Builtin::Sub,
        args: vec![x_expr.clone(), Expression::Column(x_col.into())],
    })
}

fn expression_to_name(e: &Expression, prefix: &str) -> String {
    format!("%%{}_{:?}%%", prefix, e).to_case(Case::ScreamingSnake)
}

fn expand_expr(e: &mut Expression, cols: &mut Columns, new_cs: &mut Vec<Expression>) -> Result<()> {
    match e {
        Expression::List(es) => {
            for e in es.iter_mut() {
                expand_expr(e, cols, new_cs)?;
            }
            Ok(())
        }
        Expression::Funcall { func, args } => {
            for e in args.iter_mut() {
                expand_expr(e, cols, new_cs)?;
            }
            if matches!(func, Builtin::Inv) {
                let inverted = &mut args[0];
                println!("Found an INV: {:?}", inverted);
                let inv_colname = expression_to_name(inverted, "INV");
                let inv_column = Column::Composite::<u32>(inverted.clone());
                cols.insert(inv_colname.clone(), inv_column);
                validate_inv(new_cs, inverted, &inv_colname);
                *e = Expression::Column(inv_colname)
            }
            Ok(())
        }
        _ => Ok(()),
    }
}

fn expand_plookup(e: &Expression, cols: &mut Columns, new_cs: &mut Vec<Expression>) -> Result<()> {
    match e {
        Expression::Column(_) => Ok(()),
        e => {
            let plookup_column = Column::Composite::<u32>(e.clone());
            let plookup_colname = expression_to_name(e, "PLKP");
            validate_plookup(new_cs, e, &plookup_colname);
            cols.insert(plookup_colname, plookup_column);
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

    dbg!(&cs);
    Ok(())
}
