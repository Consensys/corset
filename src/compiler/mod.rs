use self::definitions::Symbol;
use crate::column::{ColumnSet, Computation};
use definitions::SymbolTable;
use eyre::*;
use log::*;
use std::{cell::RefCell, collections::HashMap, rc::Rc};

pub use common::*;
pub use definitions::ComputationTable;
pub use generator::{Builtin, Constraint, ConstraintSet, Expression};
pub use parser::{Ast, AstNode, Kind, Token};

mod common;
mod definitions;
mod generator;
mod parser;

const MAIN_MODULE: &str = "MAIN";

const ALLOW_DUP: bool = true;

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
    let mut constants: HashMap<Handle, i64> = Default::default();
    let mut computations = ctx.borrow().computation_table.clone();
    for s in ctx.borrow().symbols() {
        match &s.1 .0 {
            Symbol::Alias(_) => {}
            Symbol::Final(symbol, used) => {
                if !used {
                    warn!("{} unused", symbol);
                }
                match symbol {
                    Expression::Column(handle, t, k) => {
                        columns.insert_column(handle, *t, ALLOW_DUP)?;
                        match k {
                            Kind::Atomic => (),
                            Kind::Composite(e) => computations.insert(
                                handle,
                                Computation::Composite {
                                    target: handle.clone(),
                                    exp: *e.clone(),
                                },
                            ),
                            Kind::Interleaved(froms) => computations.insert(
                                handle,
                                Computation::Interleaved {
                                    target: handle.clone(),
                                    froms: froms.clone(),
                                },
                            ),
                        }
                    }
                    Expression::ArrayColumn(handle, range, t) => {
                        columns.insert_array(handle, range, *t, ALLOW_DUP)?
                    }
                    Expression::Const(x) => {
                        constants.insert(s.0.to_owned(), x.try_into().unwrap());
                    }
                    x => todo!("{:?}", x),
                }
            }
        }
    }

    let r = ConstraintSet {
        constraints,
        columns,
        constants,
        computations,
    };

    Ok((asts.into_iter().map(|x| x.1).collect(), r))
}
