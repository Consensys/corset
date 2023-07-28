pub mod agnostic;
pub mod specific;

use crate::{
    compiler::{Expression, Intrinsic, Kind, Magma, Node},
    structs::{Field, Handle},
};

fn validate_computation<F: Field>(
    cs: &mut Vec<Node<Expression<F>, F>>,
    x_expr: &Node<Expression<F>, F>,
    x_col: &Handle,
) {
    cs.push(
        Intrinsic::Sub
            .call(&[
                x_expr.clone(),
                Node::column()
                    .handle(x_col.to_owned())
                    .kind(Kind::Composite(Box::new(x_expr.clone())))
                    .t(Magma::default())
                    .build(),
            ])
            .unwrap(),
    )
}

fn expression_to_name<F: Field>(e: &Node<Expression<F>, F>, prefix: &str) -> String {
    format!("{}_{}", prefix, e).replace(' ', "_")
}

fn wrap<F: Field>(ex: Node<Expression<F>, F>) -> Node<Expression<F>, F> {
    match ex.e() {
        Expression::List(_) => ex,
        _ => Node::from_expr(Expression::List(vec![ex])),
    }
}

fn flatten_list<F: Field>(mut e: Node<Expression<F>, F>) -> Node<Expression<F>, F> {
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
