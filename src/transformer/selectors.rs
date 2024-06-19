use crate::{
    column::{Column, ColumnSet, Computation},
    compiler::{ComputationTable, Constraint, ConstraintSet, Expression, Kind, Magma, Node},
    errors::CompileError,
    pretty::Base,
    structs::Handle,
};
use anyhow::*;

use super::{expression_to_name, validate_computation};

fn do_expand_expr(
    e: &Node,
    module: &str,
    cols: &mut ColumnSet,
    comps: &mut ComputationTable,
    new_cs: &mut Vec<Node>,
) -> Result<Node> {
    match e.e() {
        Expression::Column { .. } | Expression::ExoColumn { .. } => Ok(e.clone()),
        _ => {
            let new_handle = Handle::new(module, expression_to_name(e, "#EXPAND"));
            // TODO: replace name with exprs hash to 100% ensure bijectivity handle/expression
            // Only insert the computation if a column matching the expression has not already been created
            if cols
                .maybe_insert_column_and_register(
                    Column::builder()
                        .handle(new_handle.clone())
                        .kind(Kind::Computed)
                        .build(),
                )
                .is_some()
            {
                validate_computation(new_cs, e, &new_handle);
                let _ = comps.insert(
                    &new_handle.clone().into(),
                    Computation::Composite {
                        target: new_handle.clone().into(),
                        exp: e.clone(),
                    },
                );
            }

            Ok(Node::column()
                .handle(new_handle)
                .kind(Kind::Computed)
                .base(Base::Dec)
                .t(Magma::native())
                .build())
        }
    }
}

pub fn expand_constraints(cs: &mut ConstraintSet) -> Result<()> {
    let mut new_cs_exps = vec![];
    for c in cs.constraints.iter_mut() {
        match c {
            Constraint::Lookup {
                handle,
                including: parents,
                included: children,
            } => {
                let including_module = cs.columns.module_forall(parents.iter()).ok_or(
                    CompileError::AmbiguousModule("target", "lookup", handle.clone()),
                )?;
                let included_module = cs.columns.module_forall(children.iter()).ok_or(
                    CompileError::AmbiguousModule("source", "lookup", handle.clone()),
                )?;
                //
                for e in parents.iter_mut() {
                    *e = do_expand_expr(
                        e,
                        &including_module,
                        &mut cs.columns,
                        &mut cs.computations,
                        &mut new_cs_exps,
                    )?;
                }
                for e in children.iter_mut() {
                    *e = do_expand_expr(
                        e,
                        &included_module,
                        &mut cs.columns,
                        &mut cs.computations,
                        &mut new_cs_exps,
                    )?;
                }
            }
            Constraint::InRange {
                handle,
                exp: e,
                max: _,
            } => {
                let module = cs
                    .columns
                    .module_for(e.dependencies())
                    // NOTE: this error is actually very hard to
                    // trigger (though it is possible).  We can
                    // essentially ignore its possibility.
                    .ok_or(CompileError::AmbiguousModule(
                        "target",
                        "range",
                        handle.clone(),
                    ))?;
                //
                *e = do_expand_expr(
                    e,
                    &module,
                    &mut cs.columns,
                    &mut cs.computations,
                    &mut new_cs_exps,
                )?;
            }
            _ => (),
        }
    }
    if !new_cs_exps.is_empty() {
        cs.insert_constraint(Constraint::Vanishes {
            handle: Handle::new("RESERVED", "EXPANSION_CONSTRAINTS"),
            domain: None,
            expr: Box::new(Expression::List(new_cs_exps).into()),
        });
    }

    Ok(())
}
