use self::definitions::Symbol;
use crate::column::ColumnSet;
use definitions::SymbolTable;
use eyre::*;
use log::*;
use std::{cell::RefCell, collections::HashMap, rc::Rc};

pub use common::Type;
pub use generator::{Builtin, Constraint, ConstraintSet, Expression};
pub use parser::{Ast, AstNode, Kind, Token};

mod common;
mod definitions;
mod generator;
mod parser;

const MAIN_MODULE: &str = "MAIN";

pub fn make<S: AsRef<str>>(sources: &[(&str, S)]) -> Result<(Vec<Ast>, ConstraintSet)> {
    let mut asts = vec![];
    let ctx = Rc::new(RefCell::new(SymbolTable::new_root()));

    for (name, content) in sources.iter() {
        let ast = parser::parse(content.as_ref()).with_context(|| eyre!("parsing `{}`", name))?;
        definitions::pass(&ast, ctx.clone())
            .with_context(|| eyre!("parsing definitions in `{}`", name))?;
        asts.push((name, ast));
    }

    let constraints = asts
        .iter()
        .map(|(name, ast)| {
            generator::pass(ast, ctx.clone())
                .with_context(|| eyre!("compiling constraints in `{}`", name))
        })
        .collect::<Result<Vec<_>>>()?
        .into_iter()
        .flatten()
        .collect();

    let mut columns: ColumnSet<pairing_ce::bn256::Fr> = Default::default();
    let mut constants: HashMap<String, i64> = Default::default();
    for s in ctx.borrow().symbols() {
        match &s.2 .0 {
            Symbol::Alias(_) => {}
            Symbol::Final(symbol, used) => {
                if !used {
                    warn!("{:?} unused", symbol);
                }
                match symbol {
                    Expression::Column(module, name, t, k) => match k {
                        Kind::Atomic => columns.insert_atomic(module, name, *t, true)?,
                        Kind::Interleaved(cols) => {
                            columns.insert_interleaved(module, name, cols, true)?
                        }
                        Kind::Sorted(from, to) => {
                            columns.insert_sorted(module, name, from, to, true)?
                        }
                        Kind::Composite(_) => todo!(),
                    },
                    Expression::ArrayColumn(module, name, range, t) => {
                        columns.insert_array(module, name, *t, range, true)?
                    }
                    Expression::Const(x) => {
                        constants.insert(s.1.to_owned(), x.try_into().unwrap());
                    }
                    _ => {}
                }
            }
        }
    }

    let r = ConstraintSet {
        constraints,
        columns,
        constants,
    };

    Ok((asts.into_iter().map(|x| x.1).collect(), r))
}
