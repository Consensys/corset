use self::definitions::Symbol;
use crate::column::{ColumnSet, Computation};
use anyhow::*;
use definitions::SymbolTable;
use itertools::Itertools;
use log::*;
use std::{cell::RefCell, collections::HashMap, rc::Rc};

pub use common::*;
pub use definitions::ComputationTable;
pub use generator::{Builtin, Constraint, ConstraintSet, EvalSettings, Expression};
pub use parser::{Ast, AstNode, Kind, Token};

mod common;
mod compiletime;
mod definitions;
mod generator;
mod parser;

const MAIN_MODULE: &str = "<top-level>";

const ALLOW_DUP: bool = true;

#[cfg(feature = "interactive")]
pub fn make<S: AsRef<str>>(sources: &[(&str, S)]) -> Result<(Vec<Ast>, ConstraintSet)> {
    use colored::Colorize;

    let mut asts = vec![];
    let ctx = Rc::new(RefCell::new(SymbolTable::new_root()));

    for (name, content) in sources.iter() {
        let ast = parser::parse(content.as_ref()).with_context(|| anyhow!("parsing `{}`", name))?;
        definitions::pass(&ast, ctx.clone())
            .with_context(|| anyhow!("parsing definitions in `{}`", name))?;
        asts.push((name, ast));
    }

    let mut columns: ColumnSet<pairing_ce::bn256::Fr> = Default::default();
    let mut constants: HashMap<Handle, i64> = Default::default();
    let mut computations = ctx.borrow().computation_table.clone();

    for (name, ast) in asts.iter_mut() {
        compiletime::pass(ast, ctx.clone())
            .with_context(|| anyhow!("compiling constraints in {}", name.bright_white()))?
    }

    ctx.borrow_mut().visit_mut::<()>(&mut |(handle, symbol)| {
        match &mut symbol.0 {
            Symbol::Alias(_) => {}
            Symbol::Final(ref mut symbol, _) => match symbol {
                Expression::Column(ref mut handle, t, k) => {
                    columns.insert_column(handle, *t, k.to_nil(), ALLOW_DUP)?;
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
                Expression::ArrayColumn(handle, range, t) => {
                    columns.insert_array(handle, range, *t, ALLOW_DUP)?
                }
                Expression::Const(ref x, _) => {
                    constants.insert(handle, x.try_into().unwrap());
                }
                x => todo!("{:?}", x),
            },
        }
        Ok(())
    })?;

    let constraints = asts
        .iter()
        .map(|(name, ast)| {
            generator::pass(ast, ctx.clone())
                .with_context(|| anyhow!("compiling constraints in {}", name.bright_white()))
        })
        .collect::<Result<Vec<_>>>()?
        .into_iter()
        .flatten()
        // Sort by decreasing size for more efficient multi-threaded computation
        .sorted_by_cached_key(|x| -(x.size() as isize))
        .collect::<Vec<_>>();

    ctx.borrow_mut().visit_mut::<()>(&mut |(handle, symbol)| {
        if let Symbol::Final(_, used) = symbol.0 {
            if !used {
                warn!("symbol is never used: {}", handle);
            }
        }
        Ok(())
    })?;

    Ok((
        asts.into_iter().map(|x| x.1).collect(),
        ConstraintSet::new(columns, constraints, constants, computations),
    ))
}
