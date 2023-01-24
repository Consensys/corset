mod ifs;
mod inverses;
mod nhood;
mod selectors;
mod shifter;

pub use ifs::expand_ifs;
pub use inverses::expand_invs;
pub use nhood::validate_nhood;
pub use selectors::expand_constraints;
pub use shifter::lower_shifts;

use crate::compiler::{Builtin, Expression, Handle, Kind, Magma, Node, Type};

fn validate_computation(cs: &mut Vec<Node>, x_expr: &Node, x_col: &Handle) {
    cs.push(Builtin::Sub.call(&[
        x_expr.clone(),
        Node {
            _e: Expression::Column(x_col.to_owned(), Kind::Composite(Box::new(x_expr.clone()))),
            _t: Some(Type::Column(Magma::Integer)),
        },
    ]))
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
