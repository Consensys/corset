use crate::{
    column::{ColumnSet, Computation},
    compiler::{
        //
        ComputationTable,
        Constraint,
        ConstraintSet,
        Expression,
        Intrinsic,
        Kind,
        Magma,
        Node,
        Type,
    },
    pretty::Base,
    structs::Handle,
};
use anyhow::Result;

use super::expression_to_name;

fn invert_expr(e: &Node) -> Node {
    Intrinsic::Inv.call(&[e.to_owned()]).unwrap()
}

/// For all Builtin::Inv encountered, create a new column and the associated constraints
/// pre-computing and proving the inverted column.
fn do_expand_inv(
    e: &mut Node,
    cols: &mut ColumnSet,
    comps: &mut ComputationTable,
    new_cs: &mut Vec<Node>,
) -> Result<()> {
    match e.e_mut() {
        Expression::List(es) => {
            for e in es.iter_mut() {
                do_expand_inv(e, cols, comps, new_cs)?;
            }
            Ok(())
        }
        Expression::Funcall { func, args, .. } => {
            for e in args.iter_mut() {
                do_expand_inv(e, cols, comps, new_cs)?;
            }
            if matches!(func, Intrinsic::Inv) {
                let module = &args[0].module().unwrap();
                let inverted_expr = &mut args[0];
                let inverted_handle = Handle::new(module, expression_to_name(inverted_expr, "INV"));
                if cols.get(&inverted_handle).is_err() {
                    validate_inv(new_cs, inverted_expr, &inverted_handle)?;
                    cols.insert_column(
                        &inverted_handle,
                        Type::Column(Magma::Integer),
                        true,
                        Kind::Composite(Box::new(())),
                        true,
                        None,
                        None,
                        Base::Hex,
                    )?;
                    comps.insert(
                        &inverted_handle,
                        Computation::Composite {
                            target: inverted_handle.clone(),
                            exp: invert_expr(inverted_expr),
                        },
                    )?;
                }
                *e = Node {
                    _e: Expression::Column {
                        handle: inverted_handle.clone(),
                        kind: Kind::Atomic,
                        padding_value: None,
                        base: Base::Dec,
                    },
                    _t: Some(Type::Column(Magma::Integer)),
                }
            }
            Ok(())
        }
        _ => Ok(()),
    }
}

fn validate_inv(cs: &mut Vec<Node>, x_expr: &Node, inv_x_col: &Handle) -> Result<()> {
    cs.push(Intrinsic::Mul.call(&[
        x_expr.clone(),
        Intrinsic::Sub.call(&[
            Intrinsic::Mul.call(&[
                x_expr.clone(),
                Node {
                    _e: Expression::Column {
                        handle: inv_x_col.clone(),
                        kind: Kind::Composite(Box::new(Intrinsic::Inv.call(&[x_expr.clone()])?)),
                        padding_value: None,
                        base: Base::Hex,
                    },
                    _t: Some(Type::Column(Magma::Integer)),
                },
            ])?,
            Node::one(),
        ])?,
    ])?);
    cs.push(Intrinsic::Mul.call(&[
        Node {
            _e: Expression::Column {
                handle: inv_x_col.clone(),
                kind: Kind::Composite(Box::new(Intrinsic::Inv.call(&[x_expr.clone()])?)),
                padding_value: None,
                base: Base::Hex,
            },
            _t: Some(Type::Column(Magma::Integer)),
        },
        Intrinsic::Sub.call(&[
            Intrinsic::Mul.call(&[
                x_expr.clone(),
                Node {
                    _e: Expression::Column {
                        handle: inv_x_col.clone(),
                        kind: Kind::Composite(Box::new(Intrinsic::Inv.call(&[x_expr.clone()])?)),
                        padding_value: None,
                        base: Base::Hex,
                    },
                    _t: Some(Type::Column(Magma::Integer)),
                },
            ])?,
            Node::one(),
        ])?,
    ])?);

    Ok(())
}

pub fn expand_invs(cs: &mut ConstraintSet) -> Result<()> {
    let mut new_cs_inv = vec![];
    for c in cs.constraints.iter_mut() {
        if let Constraint::Vanishes { expr: e, .. } = c {
            do_expand_inv(e, &mut cs.modules, &mut cs.computations, &mut new_cs_inv)?;
        }
    }
    if !new_cs_inv.is_empty() {
        cs.constraints.push(Constraint::Vanishes {
            handle: Handle::new("RESERVED", "INV_CONSTRAINTS"),
            domain: None,
            expr: Box::new(Expression::List(new_cs_inv).into()),
        });
    }

    cs.update_ids();
    Ok(())
}
