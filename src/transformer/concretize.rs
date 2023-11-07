use crate::compiler::{Constraint, ConstraintSet, Expression, Node};
use std::str::FromStr;

impl Node {
    pub(crate) fn concretize(&mut self) {
        match self.e_mut() {
            Expression::Funcall { args, .. } => {
                for a in args {
                    a.concretize()
                }
            }
            Expression::Const(ref mut x) => x.to_native(),
            Expression::Column { .. } => {}
            Expression::ArrayColumn { .. } => {}
            Expression::ExoColumn { .. } => {}
            Expression::List(ls) => {
                for l in ls {
                    l.concretize()
                }
            }
            Expression::Void => {}
        }
    }
}

impl ConstraintSet {
    fn concretize_constraints(&mut self) {
        for c in self.constraints.iter_mut() {
            match c {
                Constraint::Vanishes { expr, .. } => expr.concretize(),
                Constraint::Lookup { .. } => {}
                Constraint::Permutation { .. } => {}
                Constraint::InRange { exp, .. } => exp.concretize(),
                Constraint::Normalization { .. } => {}
            }
        }
    }

    fn concretize_registers(&mut self) {
        for r in self.columns.registers.iter_mut() {
            r.concretize();
        }
    }
}

pub fn concretize(cs: &mut ConstraintSet) {
    *crate::IS_NATIVE.write().unwrap() = true;
    cs.concretize_registers();
    cs.concretize_constraints();
}
