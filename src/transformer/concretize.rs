use crate::{
    column::Computation,
    compiler::{Constraint, ConstraintSet, Expression, Node},
};

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
    fn make_constraints_native(&mut self) {
        for c in self.constraints.iter_mut() {
            match c {
                Constraint::Vanishes { expr, .. } => expr.concretize(),
                Constraint::Lookup { .. } => {}
                Constraint::Permutation { .. } => {}
                Constraint::InRange { exp, max, .. } => {
                    exp.concretize();
                    max.to_native();
                }
                Constraint::Normalization { .. } => {}
            }
        }
    }

    fn make_computations_native(&mut self) {
        for c in self.computations.iter_mut() {
            match c {
                Computation::Composite { exp, .. } => exp.concretize(),
                _ => {}
            }
        }
    }

    fn make_registers_native(&mut self) {
        for r in self.columns.registers.iter_mut() {
            r.concretize();
        }
    }
}

pub fn concretize(cs: &mut ConstraintSet) {
    if *crate::IS_NATIVE.read().unwrap() {
        cs.make_registers_native();
        cs.make_constraints_native();
        cs.make_computations_native();
    }
}
