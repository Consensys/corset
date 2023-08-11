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
use num_bigint::BigInt;
use owo_colors::OwoColorize;
pub use parser::{Ast, AstNode, Kind, Token};
pub use tables::ComputationTable;
pub use types::*;

use crate::{
    column::Column,
    compiler::tables::{Scope, Symbol},
    errors::CompileError,
};

pub mod codetyper;
mod common;
mod compiletime;
mod definitions;
mod fmtparser;
pub mod generator;
mod node;
mod parser;
pub mod tables;
mod types;

pub(crate) const MAIN_MODULE: &str = "<prelude>";

pub struct CompileSettings {
    pub debug: bool,
}

fn maybe_bail<R>(errs: Vec<Result<R>>) -> Result<Vec<R>> {
    let mut err_count = 0;
    let mut r = vec![];

    for e in errs.into_iter() {
        match e {
            Result::Ok(o) => {
                r.push(o);
            }
            Err(e) => {
                err_count += 1;
                error!("{:?}", e);
            }
        }
    }

    if err_count > 0 {
        bail!("{} errors found", err_count)
    } else {
        Ok(r)
    }
}

#[cfg(feature = "parser")]
pub fn parse_ast<S1: AsRef<str>, S2: AsRef<str>>(
    sources: &[(S1, S2)],
) -> Result<Vec<(String, Ast)>> {
    maybe_bail(
        sources
            .iter()
            .map(|(name, content)| {
                info!("Parsing {}", name.as_ref().bright_white().bold());
                parser::parse(content.as_ref())
                    .with_context(|| anyhow!("parsing `{}`", name.as_ref()))
                    .map(|ast| (name.as_ref().to_string(), ast))
            })
            .collect::<Vec<_>>(),
    )
}

#[cfg(feature = "parser")]
pub fn parse_simple_ast<S1: AsRef<str>, S2: AsRef<str>>(
    sources: &[(S1, S2)],
) -> Result<Vec<(String, Ast)>> {
    maybe_bail(
        sources
            .iter()
            .map(|(name, content)| {
                info!("Parsing {}", name.as_ref().bright_white().bold());
                fmtparser::parse(content.as_ref())
                    .with_context(|| anyhow!("parsing `{}`", name.as_ref()))
                    .map(|ast| (name.as_ref().to_string(), ast))
            })
            .collect::<Vec<_>>(),
    )
}

#[cfg(feature = "parser")]
pub fn make<S1: AsRef<str>, S2: AsRef<str>>(
    sources: &[(S1, S2)],
    settings: &CompileSettings,
) -> Result<(Vec<Ast>, ConstraintSet)> {
    let mut asts: Vec<(&str, Ast)> = vec![];
    let mut ctx = Scope::new();

    maybe_bail(
        sources
            .iter()
            .map(|(name, content)| {
                info!("Parsing {}", name.as_ref().bright_white().bold());
                parser::parse(content.as_ref())
                    .with_context(|| anyhow!("parsing `{}`", name.as_ref()))
                    .and_then(|ast| {
                        let r = definitions::pass(&ast, ctx.clone())
                            .with_context(|| anyhow!("parsing definitions in `{}`", name.as_ref()));
                        if r.is_ok() {
                            asts.push((name.as_ref(), ast));
                        }
                        r
                    })
            })
            .collect::<Vec<_>>(),
    )?;

    let mut columns: ColumnSet = Default::default();
    let mut constants: HashMap<Handle, BigInt> = Default::default();

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

    let constraints = maybe_bail(
        asts.iter()
            .flat_map(|(name, ast)| generator::pass(ast, ctx.clone(), name, settings))
            .collect(),
    )?
    .into_iter()
    // Sort by decreasing complexity for more efficient multi-threaded computation
    .sorted_by_cached_key(|x| -(x.size() as isize))
    .collect::<Vec<_>>();

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
                        let id = columns.insert_column(column)?;
                        match k {
                            Kind::Atomic | Kind::Phantom => (),
                            Kind::Composite(e) => computations.insert(
                                &id,
                                Computation::Composite {
                                    target: id.clone(),
                                    exp: *e.clone(),
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
