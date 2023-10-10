use crate::{
    column::Column,
    compiler::{Constraint, ConstraintSet, Expression, Kind, Magma},
    pretty::Pretty,
    structs::Handle,
};
use log::*;

fn iotaize(h: &Handle, i: usize) -> Vec<Handle> {
    (0..i).map(|i| h.iota(i)).collect()
}

fn concretize_registers(cs: &mut ConstraintSet) {
    for r in cs.columns.registers.iter_mut() {
        r.concretize();
    }
}

pub fn concretize(cs: &mut ConstraintSet) {
    concretize_registers(cs);
}
