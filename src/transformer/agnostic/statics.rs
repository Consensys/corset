use crate::{
    compiler::{Constraint, ConstraintSet, Expression, Node},
    structs::Field,
};

fn do_precompute<F: Field>(e: &mut Node<Expression<F>, F>) {
    if let Result::Ok(value) = e.pure_eval() {
        *e = Node::from_bigint(value)
    } else {
        match e.e_mut() {
            crate::compiler::Expression::Funcall { args, .. } => {
                for x in args {
                    do_precompute(x)
                }
            }
            crate::compiler::Expression::List(xs) => {
                for x in xs {
                    do_precompute(x)
                }
            }
            _ => (),
        }
    }
}

pub fn precompute<F: Field>(cs: &mut ConstraintSet<F>) {
    for c in cs.constraints.iter_mut() {
        if let Constraint::Vanishes { expr: e, .. } = c {
            do_precompute(e);
        }
    }
}
