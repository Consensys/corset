use anyhow::{bail, Result};
use owo_colors::OwoColorize;
use std::collections::HashMap;

use crate::{
    column::{Column, Computation},
    compiler::{
        ColumnRef, Constraint, ConstraintSet, Domain, Intrinsic, Kind, Magma, Node, RawMagma,
    },
    pretty::Base,
    structs::Handle,
};

fn process_binarity(column_ref: ColumnRef, cs: &mut ConstraintSet) {
    let handle = cs.handle(&column_ref);
    let x = Node::column().handle(column_ref.clone()).build();
    cs.insert_constraint(Constraint::Vanishes {
        handle: Handle::new(handle.module.clone(), format!("{}-binarity", handle.name)),
        domain: None,
        expr: Box::new(
            Intrinsic::Mul
                .call(&[
                    x.clone(),
                    Intrinsic::Sub
                        .call(&[Node::from_isize(1), x.clone()])
                        .unwrap(),
                ])
                .unwrap(),
        ),
    })
}

fn process_arbitrary(column_ref: ColumnRef, bits: usize, cs: &mut ConstraintSet) {
    let handle = cs.handle(&column_ref);
    let x = Node::column().handle(column_ref.clone()).build();
    // Determine upper bound
    let upper_bound = RawMagma::Integer(bits).upper_bound().clone();
    // Add range constraint
    cs.insert_constraint(Constraint::InRange {
        handle: Handle::new(handle.module.clone(), format!("{}-arbitrary", handle.name)),
        max: upper_bound,
        exp: x,
    })
}

/// Responsible for enforcing type constraints on any user-defined
/// column marked with `@prove`.  For `binary@prove` columns, this
/// requires adding a single constraint to enforce binariry.  For
/// other columns, we use a range constraint instead.
pub fn validate_nhood(cs: &mut ConstraintSet) -> Result<()> {
    // cols identifies all columns that must be given type
    // constraints.  We have to put these into a separate vector
    // because, otherwise, Rust makes life quite awkward (since we
    // want to modify the constraint set).
    let mut cols = Vec::new();
    //
    for (h, c) in cs.columns.iter() {
        // only atomic columns (i.e. filled from traces) are of interest here
        if c.kind == Kind::Commitment && c.must_prove {
            match c.t.rm() {
                RawMagma::Binary => cols.push((h, 1)),
                RawMagma::Nibble => cols.push((h, 4)),
                RawMagma::Byte => cols.push((h, 8)),
                RawMagma::Integer(n) => cols.push((h, n)),
                RawMagma::Any | RawMagma::Native => {
                    // Ignore
                }
                RawMagma::None => unreachable!(),
            }
        }
    }
    // Now process all columns identified as needed typing
    // constraints.
    for (h, bits) in cols {
        if bits == 1 {
            process_binarity(h, cs);
        } else if bits <= 16 {
            process_arbitrary(h, bits, cs);
        } else {
            bail!(
                "do you really want to prove a {}-bits integer?",
                bits.yellow().bold()
            );
        }
    }
    // Done
    Ok(())
}
