mod ifs;
mod inverses;
mod nhood;
mod selectors;
mod shifter;
mod sort;
mod splatter;
mod statics;

use anyhow::*;
use log::*;

pub use ifs::expand_ifs;
pub use inverses::expand_invs;
pub use nhood::validate_nhood;
pub use selectors::expand_constraints;
pub use shifter::lower_shifts;
pub use sort::sorts;
pub use splatter::splatter;
pub use statics::precompute;

use crate::{
    compiler::{ConstraintSet, Expression, Intrinsic, Kind, Magma, Node},
    structs::Handle,
};

#[derive(Debug, Copy, Clone)]
pub(crate) enum AutoConstraint {
    Sorts,
    Nhood,
}
impl AutoConstraint {
    fn apply(&self, cs: &mut ConstraintSet) -> Result<()> {
        match self {
            AutoConstraint::Sorts => sorts(cs),
            AutoConstraint::Nhood => validate_nhood(cs),
        }
    }
}

#[derive(Eq, PartialEq, PartialOrd, Ord)]
pub(crate) enum ExpansionLevel {
    None,
    LowerShifts,
    ExpandsIfs,
    Splatter,
    ExpandConstraints,
    ExpandInvs,
}
impl From<u8> for ExpansionLevel {
    fn from(x: u8) -> Self {
        match x {
            0 => ExpansionLevel::None,
            1 => ExpansionLevel::LowerShifts,
            2 => ExpansionLevel::ExpandsIfs,
            3 => ExpansionLevel::Splatter,
            4 => ExpansionLevel::ExpandConstraints,
            5 | _ => ExpansionLevel::ExpandInvs,
        }
    }
}
impl ExpansionLevel {
    pub fn all() -> u8 {
        255
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

    if level >= ExpansionLevel::LowerShifts {
        info!("Lowering shifts");
        lower_shifts(cs);
    }
    if level >= ExpansionLevel::ExpandsIfs {
        info!("Expanding Ifs");
        expand_ifs(cs);
    }
    if level >= ExpansionLevel::Splatter {
        info!("Splattering");
        splatter(cs)?;
    }
    if level >= ExpansionLevel::ExpandConstraints {
        info!("Expanding constraints");
        expand_constraints(cs)?;
    }
    if level >= ExpansionLevel::ExpandInvs {
        info!("Expanding inverses");
        expand_invs(cs)?;
    }
    if level >= ExpansionLevel::Concretize {
        info!("Concretizing to field");
        concretize::concretize(cs);
    }

    Ok(())
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
