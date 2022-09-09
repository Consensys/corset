use convert_case::{Case, Casing};
use std::collections::HashMap;

use crate::{
    column::Column,
    compiler::{Builtin, ConstraintsSet, Expression},
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

fn expression_to_name(e: &Expression) -> String {
    format!("%%INV_{:?}%%", e).to_case(Case::ScreamingSnake)
}

fn rec_expand(
    e: &mut Expression,
    cols: &mut HashMap<String, Column<u32>>,
    new_cs: &mut Vec<Expression>,
) -> Result<()> {
    match e {
        Expression::Constraint { expr: e, .. } => rec_expand(e, cols, new_cs),
        Expression::List(es) => {
            for e in es.iter_mut() {
                rec_expand(e, cols, new_cs)?;
            }
            Ok(())
        }
        Expression::Funcall { func, args } => {
            for e in args.iter_mut() {
                rec_expand(e, cols, new_cs)?;
            }
            if matches!(func, Builtin::Inv) {
                let inverted = &mut args[0];
                println!("Found an INV: {:?}", inverted);
                let inv_colname = expression_to_name(inverted);
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

pub fn expand(cs: &mut ConstraintsSet) -> Result<()> {
    dbg!(&cs);

    let mut new_cs = vec![];
    for c in cs.constraints.iter_mut() {
        rec_expand(c, &mut cs.columns, &mut new_cs)?;
    }
    cs.constraints.push(Expression::Constraint {
        name: "INV_CONSTRAINTS".into(),
        domain: None,
        expr: Expression::List(new_cs).into(),
    });

    dbg!(&cs);
    Ok(())
}
