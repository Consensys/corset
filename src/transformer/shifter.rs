use num_traits::ToPrimitive;

use crate::compiler::{Constraint, ConstraintSet, Expression, Intrinsic, Node};

fn do_lower_shifts(e: &mut Node, depth: isize) {
    match e.e_mut() {
        Expression::Funcall { func, args } => {
            if matches!(func, Intrinsic::Shift) {
                let shift = args[1].pure_eval().unwrap().to_isize().unwrap();
                *e = args[0].clone();
                do_lower_shifts(e, depth + shift);
            } else {
                for x in args.iter_mut() {
                    do_lower_shifts(x, depth);
                }
            }
        }
        Expression::Const(_) => (),
        Expression::Column { .. } | Expression::ExoColumn { .. } => {
            if depth != 0 {
                let column = e.clone();
                *e = Intrinsic::Shift
                    .call(&[column, Expression::Const(depth.into()).into()])
                    .unwrap();
            }
        }
        Expression::ArrayColumn { .. } => (),
        Expression::List(xs) => {
            for x in xs.iter_mut() {
                do_lower_shifts(x, depth);
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
            Constraint::Plookup {
                handle: _name,
                including: parents,
                included: children,
            } => {
                for e in parents.iter_mut().chain(children.iter_mut()) {
                    do_lower_shifts(e, 0);
                }
            }
            Constraint::Permutation { .. } => (),
            Constraint::InRange {
                handle: _,
                exp: e,
                max: _,
            } => {
                do_lower_shifts(e, 0);
            }
        }
    }
}
