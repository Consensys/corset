use std::collections::HashSet;

use crate::{
    column::{Column, Computation},
    compiler::{ColumnRef, Constraint, ConstraintSet, Expression, Intrinsic, Kind, Magma, Node},
    pretty::Base,
    structs::Handle,
};
use anyhow::{anyhow, Context, Result};

use super::expression_to_name;

fn invert_expr(e: &Node) -> Node {
    Intrinsic::Inv.call(&[e.to_owned()]).unwrap()
}

/// For all Builtin::Inv encountered, create a new column and the associated constraints
/// pre-computing and proving the inverted column.

impl Node {
    pub(crate) fn do_expand_inv(
        &mut self,
        get_module: &dyn Fn(&HashSet<ColumnRef>) -> String,
        new_cols: &mut Vec<(Handle, Node)>,
    ) -> Result<()> {
        match self.e_mut() {
            Expression::List(es) => {
                for e in es.iter_mut() {
                    e.do_expand_inv(get_module, new_cols)?;
                }
                Ok(())
            }
            Expression::Funcall { func, args, .. } => {
                for e in args.iter_mut() {
                    e.do_expand_inv(get_module, new_cols)?;
                }
                if matches!(func, Intrinsic::Normalize) {
                    let module = get_module(&args[0].dependencies());
                    let normalized_handle =
                        Handle::new(module, expression_to_name(&args[0], "NORM"));
                    new_cols.push((normalized_handle.clone(), args[0].to_owned()));
                    *self = Node::column()
                        .handle(normalized_handle)
                        .kind(Kind::Phantom)
                        .t(self.t().m().invert())
                        .build();
                }
                Ok(())
            }
            _ => Ok(()),
        }
    }
}

impl ConstraintSet {
    pub fn expand_invs(&mut self) -> Result<()> {
        let mut new_cols = vec![];

        let get_module = |rs: &HashSet<ColumnRef>| self.columns.module_for(rs.iter()).unwrap();
        for i in 0..self.constraints.len() {
            if let Constraint::Vanishes { expr: e, .. } = self.constraints.get_mut(i).unwrap() {
                e.do_expand_inv(&get_module, &mut new_cols)
                    .with_context(|| anyhow!("while expanding inverses"))?;
            }
        }

        for (normalized_handle, normalized_expr) in new_cols.into_iter() {
            if self.columns.by_handle(&normalized_handle).is_err() {
                let normalized_id = self.columns.insert_column_and_register(
                    Column::builder()
                        .handle(normalized_handle.clone())
                        .kind(Kind::Composite(Box::new(())))
                        .build(),
                )?;

                let inverted_handle = Handle::new(
                    &normalized_handle.module,
                    expression_to_name(&normalized_expr, "INV"),
                );
                let inverted_id = self.columns.insert_column_and_register(
                    Column::builder()
                        .handle(inverted_handle.to_owned())
                        .kind(Kind::Composite(Box::new(())))
                        .build(),
                )?;

                self.computations.insert(
                    &inverted_id,
                    Computation::Composite {
                        target: inverted_id.clone(),
                        exp: invert_expr(&normalized_expr),
                    },
                )?;
                self.constraints.push(Constraint::Normalization {
                    handle: normalized_handle.clone(),
                    reference: normalized_expr.to_owned(),
                    inverted: inverted_id,
                    normalized: normalized_id,
                })
            }
        }

        Ok(())
    }
}

// TODO: move that into the Wizard
fn validate_inv(cs: &mut Vec<Node>, x_expr: &Node, inv_x_col: &ColumnRef) -> Result<()> {
    // X × (X × /X - 1)
    cs.push(
        Intrinsic::Mul.call(&[
            x_expr.clone(),
            Intrinsic::Sub.call(&[
                Intrinsic::Mul.call(&[
                    x_expr.clone(),
                    Node::column()
                        .handle(inv_x_col.clone())
                        .kind(Kind::Phantom)
                        .t(Magma::native())
                        .build(),
                ])?,
                Node::one(),
            ])?,
        ])?,
    );

    // /X × (X × /X - 1)
    cs.push(
        Intrinsic::Mul.call(&[
            Node::column()
                .handle(inv_x_col.clone())
                .kind(Kind::Phantom)
                .base(Base::Hex)
                .t(Magma::native())
                .build(),
            Intrinsic::Sub.call(&[
                Intrinsic::Mul.call(&[
                    x_expr.clone(),
                    Node::column()
                        .handle(inv_x_col.clone())
                        .kind(Kind::Phantom)
                        .base(Base::Hex)
                        .t(Magma::native())
                        .build(),
                ])?,
                Node::one(),
            ])?,
        ])?,
    );

    Ok(())
}

pub fn expand_invs(cs: &mut ConstraintSet) -> Result<()> {
    cs.expand_invs()
}
