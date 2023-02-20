use crate::{
    column::{ColumnSet, Computation},
    compiler::{ComputationTable, Constraint, ConstraintSet, Expression, Kind, Magma, Node, Type},
    structs::Handle,
};
use anyhow::Result;

use super::{expression_to_name, validate_computation};

fn do_expand_expr(
    e: &Node,
    cols: &mut ColumnSet,
    comps: &mut ComputationTable,
    new_cs: &mut Vec<Node>,
) -> Result<Node> {
    match e.e() {
        Expression::Column(..) => Ok(e.clone()),
        _ => {
            let module = e.module().unwrap();
            let new_handle = Handle::new(module, expression_to_name(e, "EXPAND"));
            validate_computation(new_cs, e, &new_handle);
            cols.insert_column(
                &new_handle,
                Type::Column(Magma::Integer),
                true,
                Kind::Phantom,
                true,
                None,
            )?;

            let _ = comps.insert(
                &new_handle,
                Computation::Composite {
                    target: new_handle.clone(),
                    exp: e.clone(),
                },
            );
            Ok(Node {
                _e: Expression::Column(new_handle, Kind::Phantom),
                _t: Some(Type::Column(Magma::Integer)),
            })
        }
    }
}

pub fn expand_constraints(cs: &mut ConstraintSet) -> Result<()> {
    let mut new_cs_exps = vec![];
    for c in cs.constraints.iter_mut() {
        match c {
            Constraint::Plookup {
                handle: _name,
                including: parents,
                included: children,
            } => {
                for e in parents.iter_mut().chain(children.iter_mut()) {
                    *e =
                        do_expand_expr(e, &mut cs.modules, &mut cs.computations, &mut new_cs_exps)?;
                }
            }
            Constraint::InRange {
                handle: _,
                exp: e,
                max: _,
            } => {
                *e = do_expand_expr(e, &mut cs.modules, &mut cs.computations, &mut new_cs_exps)?;
            }
            _ => (),
        }
    }
    if !new_cs_exps.is_empty() {
        cs.constraints.push(Constraint::Vanishes {
            handle: Handle::new("RESERVED", "EXPANSION_CONSTRAINTS"),
            domain: None,
            expr: Box::new(Expression::List(new_cs_exps).into()),
        });
    }

    cs.update_ids();
    Ok(())
}
