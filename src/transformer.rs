mod ifs;
mod inverses;
mod nhood;
mod selectors;
mod shifter;
mod sort;
mod statics;

pub use ifs::expand_ifs;
pub use inverses::expand_invs;
pub use nhood::validate_nhood;
pub use selectors::expand_constraints;
pub use shifter::lower_shifts;
pub use sort::sorts;
pub use statics::precompute;

use crate::{
    compiler::{Expression, Intrinsic, Kind, Magma, Node},
    structs::Handle,
};

fn validate_computation(cs: &mut Vec<Node>, x_expr: &Node, x_col: &Handle) {
    cs.push(
        Intrinsic::Sub
            .call(&[
                x_expr.clone(),
                Node::column()
                    .handle(x_col.to_owned())
                    .kind(Kind::Composite(Box::new(x_expr.clone())))
                    .t(Magma::Integer)
                    .build(),
            ])
            .unwrap(),
    )
}

fn expression_to_name(e: &Node, prefix: &str) -> String {
    format!("{}_{}", prefix, e).replace(' ', "_")
}

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
