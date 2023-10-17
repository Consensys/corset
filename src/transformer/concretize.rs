use crate::compiler::{Constraint, ConstraintSet, Expression, Node};

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
                Constraint::Plookup { .. } => {}
                Constraint::Permutation { .. } => {}
                Constraint::InRange { exp, .. } => exp.concretize(),
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
    cs.concretize_constraints();
    cs.concretize_registers();
}
