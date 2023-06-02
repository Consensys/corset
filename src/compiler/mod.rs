use crate::{
    column::{ColumnID, ColumnSet, Computation},
    structs::Handle,
};
use anyhow::*;
use itertools::Itertools;
use log::*;
use std::collections::HashMap;

pub use common::*;
pub use generator::{Constraint, ConstraintSet, EvalSettings};
pub use node::{ColumnRef, Expression, Node};
pub use parser::{Ast, AstNode, Kind, Token};
pub use tables::ComputationTable;
pub use types::*;

pub mod codetyper;
mod common;
mod compiletime;
mod definitions;
pub mod generator;
mod node;
mod parser;
pub mod tables;
mod types;

pub(crate) const MAIN_MODULE: &str = "<prelude>";

pub struct CompileSettings {
    pub debug: bool,
    pub allow_dups: bool,
}

#[cfg(feature = "parser")]
pub fn make<S: AsRef<str>>(
    sources: &[(&str, S)],
    settings: &CompileSettings,
) -> Result<(Vec<Ast>, ConstraintSet)> {
    use num_bigint::BigInt;
    use owo_colors::OwoColorize;

    use crate::{
        column::Column,
        compiler::tables::{Scope, Symbol},
        errors::CompileError,
    };

    let mut asts = vec![];
    let mut ctx = Scope::new();

    for (name, content) in sources.iter() {
        info!("Parsing {}", name.bright_white().bold());
        let ast = parser::parse(content.as_ref()).with_context(|| anyhow!("parsing `{}`", name))?;
        definitions::pass(&ast, ctx.clone())
            .with_context(|| anyhow!("parsing definitions in `{}`", name))?;
        asts.push((name, ast));
    }

    let mut columns: ColumnSet = Default::default();
    let mut constants: HashMap<Handle, BigInt> = Default::default();
    let mut computations = ctx.computations();

    for (name, ast) in asts.iter_mut() {
        info!(
            "Evaluating compile-time values in {}",
            name.bright_white().bold()
        );
        compiletime::pass(ast, ctx.clone(), settings).with_context(|| {
            anyhow!(
                "evaluating compile-time values in {}",
                name.bright_white().bold()
            )
        })?
    }

    let constraints = asts
        .iter()
        .map(|(name, ast)| {
            generator::pass(ast, ctx.clone(), settings)
                .with_context(|| anyhow!("compiling constraints in {}", name.bright_white().bold()))
        })
        .collect::<Result<Vec<_>>>()?
        .into_iter()
        .flatten()
        // Sort by decreasing complexity for more efficient multi-threaded computation
        .sorted_by_cached_key(|x| -(x.size() as isize))
        .collect::<Vec<_>>();

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
                        ..
                    } => {
                        let column = Column::builder()
                            .and_padding_value(padding_value.to_owned())
                            .handle(handle.as_handle().clone())
                            .used(*used)
                            .kind(k.to_nil())
                            .t(symbol.t().magma())
                            .base(*base)
                            .build();
                        let id = columns.insert_column(column, settings.allow_dups)?;
                        match k {
                            Kind::Atomic | Kind::Phantom => (),
                            Kind::Composite(e) => computations.insert(
                                &id,
                                Computation::Composite {
                                    target: id.clone(),
                                    exp: *e.clone(),
                                },
                            )?,
                            Kind::Interleaved(_, froms) => computations.insert(
                                &id,
                                Computation::Interleaved {
                                    target: id.clone(),
                                    froms: froms.as_ref().unwrap().to_owned(),
                                },
                            )?,
                        }
                    }
                    Expression::Const(ref x, _) => {
                        constants.insert(handle, x.clone());
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
                        (perspective.to_owned(), trigger.clone().unwrap().into())
                    })
                    .collect::<HashMap<_, _>>(),
            )
        })
        .collect::<HashMap<_, _>>();

    let cs = ConstraintSet::new(columns, constraints, constants, computations, perspectives)?;
    Ok((asts.into_iter().map(|x| x.1).collect(), cs))
}
