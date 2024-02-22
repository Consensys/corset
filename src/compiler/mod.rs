use crate::{
    column::{ColumnSet, Computation},
    structs::Handle,
};
use anyhow::*;
use log::*;
use std::collections::HashMap;

pub use common::*;
pub use generator::{Constraint, ConstraintSet, EvalSettings};
pub use node::{ColumnRef, Expression, Node};
use num_bigint::BigInt;
use owo_colors::OwoColorize;
pub use tables::ComputationTable;
pub use types::*;

use self::parser::Ast;
use crate::{column::Column, compiler::tables::Symbol, errors::CompileError};

pub mod codetyper;
mod common;
pub mod generator;
mod node;
pub mod parser;
pub mod tables;
mod types;

pub(crate) const MAIN_MODULE: &str = "<prelude>";

pub struct CompileSettings {
    pub debug: bool,
}

pub fn make<S1: AsRef<str>, S2: AsRef<str>>(
    sources: &[(S1, S2)],
    settings: &CompileSettings,
) -> Result<(Vec<Ast>, ConstraintSet)> {
    let (mut ctx, asts) = parser::parse(sources, settings)?;

    //
    // Reduce the AST and create the constraints
    //
    let mut constraints = vec![];
    for (name, ast) in asts.iter() {
        for constraint in generator::pass(ast, ctx.clone(), settings) {
            constraints.push(
                constraint.with_context(|| anyhow!("compiling {}", name.bright_white().bold()))?,
            );
        }
    }
    // Sort by decreasing complexity for more efficient multi-threaded computation
    constraints.sort_by_cached_key(|x| -(x.size() as isize));

    let mut columns: ColumnSet = Default::default();
    let mut constants: HashMap<Handle, BigInt> = Default::default();
    let mut computations = ctx.computations();

    ctx.visit_mut::<()>(&mut |handle, symbol| {
        match symbol {
            Symbol::Alias(_) => {}
            Symbol::Final(symbol, used) => {
                if !*used {
                    warn!("{}", CompileError::NotUsed(handle.clone()));
                }

                match symbol.e() {
                    Expression::Column {
                        handle,
                        kind: k,
                        padding_value,
                        base,
                        must_prove,
                        ..
                    } => {
                        let column = Column::builder()
                            .handle(handle.as_handle().clone())
                            .and_padding_value(padding_value.to_owned())
                            .kind(k.to_nil())
                            .t(symbol.t().m())
                            .must_prove(*must_prove)
                            .used(*used)
                            .base(*base)
                            .build();
                        let id = columns.insert_column(column)?;
                        match k {
                            Kind::Commitment | Kind::Computed => (),
                            Kind::Expression(e) => {
                                computations
                                    .insert(
                                        &id,
                                        Computation::Composite {
                                            target: id.clone(),
                                            exp: *e.clone(),
                                        },
                                    )
                                    .map(|_| ())?;
                                constraints.push(Constraint::Vanishes {
                                    handle: Handle::new(
                                        &handle.as_handle().module,
                                        format!("prove-{}", handle.as_handle().name),
                                    ),
                                    domain: None,
                                    expr: Box::new(
                                        Intrinsic::Sub
                                            .call(&[Node::column().handle(id).build(), *e.clone()])
                                            .unwrap(),
                                    ),
                                })
                            }
                        }
                    }
                    Expression::ExoColumn {
                        handle,
                        padding_value,
                        base,
                        ..
                    } => {
                        let column = Column::builder()
                            .handle(handle.as_handle().clone())
                            .and_padding_value(padding_value.to_owned())
                            .used(*used)
                            .kind(Kind::Commitment)
                            .t(symbol.t().m())
                            .base(*base)
                            .build();
                        columns.insert_column(column)?;
                    }
                    Expression::Const(ref x) => {
                        constants.insert(handle, x.clone().into());
                    }
                    _ => {}
                }
            }
        }
        Ok(())
    })?;

    let perspectives = ctx
        .tree
        .borrow()
        .metadata()
        .perspectives
        .iter()
        .map(|(module, persps)| {
            (
                module.to_owned(),
                persps
                    .iter()
                    .map(|(perspective, trigger)| {
                        (perspective.to_owned(), trigger.clone().unwrap())
                    })
                    .collect::<HashMap<_, _>>(),
            )
        })
        .collect::<HashMap<_, _>>();

    let mut cs = ConstraintSet::new(columns, constraints, constants, computations, perspectives)?;
    crate::transformer::precompute(&mut cs);
    Ok((asts.into_iter().map(|x| x.1).collect(), cs))
}
