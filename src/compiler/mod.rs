use self::definitions::Symbol;
use crate::column::{Column, ColumnSet};
use definitions::SymbolTable;
use eyre::*;
use std::{cell::RefCell, collections::HashMap, rc::Rc};

pub use common::Type;
pub use generator::{Builtin, Constraint, ConstraintsSet, Expression};
pub use parser::{Ast, AstNode, Kind, Token};

mod common;
mod definitions;
mod generator;
mod parser;

const MAIN_MODULE: &str = "MAIN";

pub fn make<S: AsRef<str>>(sources: &[(&str, S)]) -> Result<(Vec<Ast>, ConstraintsSet)> {
    let mut asts = vec![];
    let ctx = Rc::new(RefCell::new(SymbolTable::new_root()));

    for (name, content) in sources.iter() {
        let ast = parser::parse(content.as_ref()).with_context(|| eyre!("parsing `{}`", name))?;
        definitions::pass(&ast, ctx.clone())
            .with_context(|| eyre!("parsing definitions in `{}`", name))?;
        asts.push((name, ast));
    }

    let mut columns: ColumnSet<u32> = Default::default();
    for s in ctx.borrow().symbols() {
        match &s.1 .0 {
            Symbol::Alias(_) => {}
            Symbol::Final(symbol, used) => {
                if !used {
                    eprintln!("WARN unused: {:?}", symbol);
                }
                match symbol {
                    Expression::Column(module, name, t, k) => match k {
                        Kind::Atomic => columns.insert_atomic(module, name, *t, true)?,
                        Kind::Interleaved(cols) => {
                            columns.insert_interleaved(module, name, cols, true)?
                        }
                        Kind::Composite(_) => todo!(),
                        Kind::Sorted(_) => todo!(),
                    },
                    Expression::ArrayColumn(module, name, range, t) => {
                        columns.insert_array(module, name, *t, range, true)?
                    }
                    _ => {}
                }
            }
        }
    }

    let r = ConstraintsSet {
        constraints: asts
            .iter()
            .map(|(name, ast)| {
                generator::pass(ast, ctx.clone())
                    .with_context(|| eyre!("compiling constraints in `{}`", name))
            })
            .collect::<Result<Vec<_>>>()?
            .into_iter()
            .flatten()
            .collect(),
        columns,
    };

    Ok((asts.into_iter().map(|x| x.1).collect(), r))
}
