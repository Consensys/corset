use std::fs;
use anyhow::*;
use crate::compiler::{ConstraintSet};
use crate::{transformer::ExpansionLevel, ConstraintSetBuilder};
use crate::compiler;

#[test]
fn eval_test_01() {
    check("tests/iszero.lisp")
}

fn check(name: &str) {
    let cs = compile(name).unwrap();
}

fn compile(name: &str) -> Result<ConstraintSet> {
    // Read source file
    let source = fs::read_to_string(name).unwrap();
    // Configure the build
    let mut r = ConstraintSetBuilder::from_sources(false, false);
    r.add_source(&source)?;
    r.expand_to(ExpansionLevel::top());
    // Done
    r.into_constraint_set()
}
