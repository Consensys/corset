use anyhow::Result;
use num_bigint::BigInt;
use num_traits::ToPrimitive;

use crate::compiler::{Builtin, Constraint, ConstraintSet, Expression, Node};

fn do_lower_shifts(e: &mut Node, depth: isize) -> Result<()> {
    match e.e_mut() {
        Expression::Funcall { func, args } => {
            if matches!(func, Builtin::Shift) {
                let shift = args[1].pure_eval().unwrap().to_isize().unwrap();
                *e = args[0].clone();
                do_lower_shifts(e, depth + shift)?;
            } else {
                for x in args.iter_mut() {
                    do_lower_shifts(x, depth)?;
                }
            }
        }
        Expression::Const(_, _) => (),
        Expression::Column(..) => {
            if depth != 0 {
                let column = e.clone();
                *e = Builtin::Shift.call(&[
                    column,
                    Node::from_expr(Expression::Const(BigInt::from(depth), None)),
                ])?
            }
        }
        Expression::ArrayColumn(..) => (),
        Expression::List(xs) => {
            for x in xs.iter_mut() {
                do_lower_shifts(x, depth)?;
            }
        }
        Expression::Void => (),
    }

    Ok(())
}

pub fn lower_shifts(cs: &mut ConstraintSet) -> Result<()> {
    for c in cs.constraints.iter_mut() {
        match c {
            Constraint::Vanishes { expr: e, .. } => {
                do_lower_shifts(e, 0)?;
            }
            Constraint::Plookup {
                handle: _name,
                including: parents,
                included: children,
            } => {
                for e in parents.iter_mut().chain(children.iter_mut()) {
                    do_lower_shifts(e, 0)?;
                }
            }
            Constraint::Permutation { .. } => (),
            Constraint::InRange {
                handle: _,
                exp: e,
                max: _,
            } => {
                do_lower_shifts(e, 0)?;
            }
        }
    }

    Ok(())
}
