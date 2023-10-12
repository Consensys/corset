mod concretize;
mod ifs;
mod inverses;
mod nhood;
mod selectors;
mod sort;
mod splatter;
mod statics;

use anyhow::*;
use log::*;

pub use concretize::concretize;
use ifs::expand_ifs;
use inverses::expand_invs;
use nhood::validate_nhood;
use selectors::expand_constraints;
use sort::sorts;
use splatter::splatter;
pub use statics::precompute;

use crate::{
    compiler::{ConstraintSet, Expression, Intrinsic, Kind, Magma, Node},
    structs::Handle,
};

#[derive(Debug, Copy, Clone)]
pub(crate) enum AutoConstraint {
    Sorts = 1,
    Nhood = 2,
}
impl AutoConstraint {
    fn apply(&self, cs: &mut ConstraintSet) -> Result<()> {
        if (cs.transformations & *self as u32) == 0 {
            info!("Applying {:?}", self);
            match self {
                AutoConstraint::Sorts => sorts(cs)?,
                AutoConstraint::Nhood => validate_nhood(cs)?,
            }
            cs.auto_constraints |= *self as u32;
        }
        Ok(())
    }
}

#[derive(Eq, PartialEq, PartialOrd, Ord, Debug, Copy, Clone)]
pub(crate) enum ExpansionLevel {
    None = 0,
    ExpandsIfs = 1,
    Splatter = 2,
    ColumnizeExpressions = 4,
    ExpandInvs = 8,
}
impl From<u8> for ExpansionLevel {
    fn from(x: u8) -> Self {
        match x {
            0 => ExpansionLevel::None,
            1 => ExpansionLevel::ExpandsIfs,
            2 => ExpansionLevel::Splatter,
            3 => ExpansionLevel::ColumnizeExpressions,
            4 | _ => ExpansionLevel::ExpandInvs,
        }
    }
}
impl ExpansionLevel {
    pub fn all() -> u8 {
        5
    }

    pub fn apply(&self, cs: &mut ConstraintSet) -> Result<()> {
        if (cs.transformations & *self as u32) == 0 {
            info!("Applying {:?}", self);
            match self {
                ExpansionLevel::None => {}
                ExpansionLevel::ExpandsIfs => expand_ifs(cs),
                ExpansionLevel::Splatter => splatter(cs),
                ExpansionLevel::ColumnizeExpressions => expand_constraints(cs)?,
                ExpansionLevel::ExpandInvs => expand_invs(cs)?,
            }
            cs.transformations |= *self as u32;
        }

        Ok(())
    }
}

pub(crate) fn expand_to(
    cs: &mut ConstraintSet,
    level: u8,
    auto_constraints: &[AutoConstraint],
) -> Result<()> {
    let level: ExpansionLevel = level.into();

    for c in auto_constraints.iter() {
        c.apply(cs)?;
    }

    for transformation in [
        ExpansionLevel::ExpandsIfs,
        ExpansionLevel::Splatter,
        ExpansionLevel::ColumnizeExpressions,
        ExpansionLevel::ExpandInvs,
    ] {
        if level >= transformation {
            transformation.apply(cs)?;
        }
    }

    cs.convert_refs_to_ids()?;
    cs.validate()
}

fn validate_computation(cs: &mut Vec<Node>, x_expr: &Node, x_col: &Handle) {
    cs.push(
        Intrinsic::Sub
            .call(&[
                x_expr.clone(),
                Node::column()
                    .handle(x_col.to_owned())
                    .kind(Kind::Composite(Box::new(x_expr.clone())))
                    .t(Magma::Native)
                    .build(),
            ])
            .unwrap(),
    )
}

fn expression_to_name(e: &Node, prefix: &str) -> String {
    format!("{}_{}", prefix, e).replace(' ', "_")
}

/// Wraps `ex` into a `List` if it is not already one.
fn wrap(ex: Node) -> Node {
    match ex.e() {
        Expression::List(_) => ex,
        _ => Node::from_expr(Expression::List(vec![ex])),
    }
}

fn flatten_list(mut e: Node) -> Node {
    match e.e_mut() {
        Expression::List(ref mut xs) => {
            if xs.len() == 1 {
                flatten_list(xs.pop().unwrap())
            } else {
                e
            }
        }
        _ => e,
    }
}
