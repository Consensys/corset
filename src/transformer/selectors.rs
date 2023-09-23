use crate::{
    column::{Column, ColumnSet, Computation},
    compiler::{ComputationTable, Constraint, ConstraintSet, Expression, Kind, Magma, Node},
    pretty::Base,
    structs::Handle,
};
use anyhow::*;

use super::{expression_to_name, validate_computation};

fn do_expand_expr(
    e: &Node,
    cols: &mut ColumnSet,
    comps: &mut ComputationTable,
    new_cs: &mut Vec<Node>,
) -> Result<Node> {
    match e.e() {
        Expression::Column { .. } | Expression::ExoColumn { .. } => Ok(e.clone()),
        _ => {
            let module = cols.module_of(e.dependencies().iter()).unwrap();
            let new_handle = Handle::new(module, expression_to_name(e, "EXPAND"));
            validate_computation(new_cs, e, &new_handle);
            cols.insert_column_and_register(
                Column::builder()
                    .handle(new_handle.clone())
                    .kind(Kind::Phantom)
                    .build(),
            )?;

            let _ = comps.insert(
                &new_handle.clone().into(),
                Computation::Composite {
                    target: new_handle.clone().into(),
                    exp: e.clone(),
                },
            );
            Ok(Node::column()
                .handle(new_handle)
                .kind(Kind::Phantom)
                .base(Base::Dec)
                .t(Magma::Native)
                .build())
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
                        do_expand_expr(e, &mut cs.columns, &mut cs.computations, &mut new_cs_exps)?;
                }
            }
            Constraint::InRange {
                handle: _,
                exp: e,
                max: _,
            } => {
                *e = do_expand_expr(e, &mut cs.columns, &mut cs.computations, &mut new_cs_exps)?;
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

    Ok(())
}
