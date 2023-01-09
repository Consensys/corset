use self::definitions::Symbol;
use crate::column::{ColumnSet, Computation};
use anyhow::*;
use definitions::SymbolTable;
use itertools::Itertools;
use log::*;
use std::{cell::RefCell, collections::HashMap, rc::Rc};

pub use common::*;
pub use definitions::ComputationTable;
pub use generator::{Builtin, Constraint, ConstraintSet, EvalSettings, Expression, Node};
pub use parser::{Ast, AstNode, Kind, Token};

mod common;
mod compiletime;
mod definitions;
mod generator;
mod parser;

const MAIN_MODULE: &str = "<prelude>";

pub struct CompileSettings {
    pub debug: bool,
    pub allow_dups: bool,
}

pub enum PaddingStrategy {
    #[allow(dead_code)]
    Full,
    OneLine,
    None,
}

#[cfg(feature = "interactive")]
pub fn make<S: AsRef<str>>(
    sources: &[(&str, S)],
    settings: &CompileSettings,
) -> Result<(Vec<Ast>, ConstraintSet)> {
    use colored::Colorize;
    use num_bigint::BigInt;

    let mut asts = vec![];
    let ctx = Rc::new(RefCell::new(SymbolTable::new_root()));

    for (name, content) in sources.iter() {
        info!("Parsing {}", name.bright_white().bold());
        let ast = parser::parse(content.as_ref()).with_context(|| anyhow!("parsing `{}`", name))?;
        definitions::pass(&ast, ctx.clone())
            .with_context(|| anyhow!("parsing definitions in `{}`", name))?;
        asts.push((name, ast));
    }

    let mut columns: ColumnSet<pairing_ce::bn256::Fr> = Default::default();
    let mut constants: HashMap<Handle, BigInt> = Default::default();
    let mut computations = ctx.borrow().computation_table.clone().take();

    for (name, ast) in asts.iter_mut() {
        info!("Compiling {}", name.bright_white().bold());
        compiletime::pass(ast, ctx.clone(), settings).with_context(|| {
            anyhow!(
                "evaluating compile-time values in {}",
                name.bright_white().bold()
            )
        })?
    }

    ctx.borrow_mut().visit_mut::<()>(&mut |_, handle, symbol| {
        match symbol {
            Symbol::Alias(_) => {}
            Symbol::Final(symbol, _) => match symbol.e() {
                Expression::Column(handle, k) => {
                    columns.insert_column(
                        handle,
                        symbol.t(),
                        false, // Columns use state is only known later on
                        k.to_nil(),
                        settings.allow_dups,
                    )?;
                    match k {
                        Kind::Atomic | Kind::Phantom => (),
                        Kind::Composite(e) => computations.insert(
                            handle,
                            Computation::Composite {
                                target: handle.clone(),
                                exp: *e.clone(),
                            },
                        )?,
                        Kind::Interleaved(froms) => computations.insert(
                            handle,
                            Computation::Interleaved {
                                target: handle.clone(),
                                froms: froms.clone(),
                            },
                        )?,
                    }
                }
                Expression::ArrayColumn(handle, range) => {
                    columns.insert_array(handle, range, symbol.t(), settings.allow_dups)?
                }
                Expression::Const(ref x, _) => {
                    constants.insert(handle, x.clone());
                }
                _ => todo!("{:?}", symbol),
            },
        }
        Ok(())
    })?;

    let constraints = asts
        .iter()
        .map(|(name, ast)| {
            generator::pass(ast, ctx.clone(), settings)
                .with_context(|| anyhow!("compiling constraints in {}", name.bright_white().bold()))
        })
        .collect::<Result<Vec<_>>>()?
        .into_iter()
        .flatten()
        // Sort by decreasing size for more efficient multi-threaded computation
        .sorted_by_cached_key(|x| -(x.size() as isize))
        .collect::<Vec<_>>();

    ctx.borrow_mut()
        .visit_mut::<()>(&mut |module, handle, symbol| {
            if let Symbol::Final(symbol, used) = symbol {
                if !*used {
                    warn!(
                        "[{}] {} is never used",
                        module.blue(),
                        handle.name.bright_white().bold()
                    );
                } else {
                    if let Expression::Column(handle, _) = symbol.e() {
                        columns.get_mut(&handle).unwrap().used = *used;
                    }
                }
            }
            Ok(())
        })?;

    Ok((
        asts.into_iter().map(|x| x.1).collect(),
        ConstraintSet::new(columns, constraints, constants, computations),
    ))
}
