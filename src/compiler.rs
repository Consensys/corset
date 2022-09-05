use eyre::*;
use std::{cell::RefCell, rc::Rc};

use definitions::SymbolTable;

pub use generator::ConstraintsSet;

mod common;
mod definitions;
mod generator;
mod parser;

pub fn make<S: AsRef<str>>(sources: &[(&str, S)]) -> Result<ConstraintsSet> {
    let mut asts = vec![];
    let ctx = Rc::new(RefCell::new(SymbolTable::new_root()));

    for (name, content) in sources.iter() {
        let ast = parser::parse(content.as_ref()).with_context(|| eyre!("parsing `{}`", name))?;
        definitions::pass(&ast, ctx.clone())
            .with_context(|| eyre!("parsing definitions in `{}`", name))?;
        asts.push((name, ast));
    }

    let constraints = asts
        .into_iter()
        .map(|(name, ast)| {
            generator::pass(&ast, ctx.clone())
                .with_context(|| eyre!("compiling constraints in `{}`", name))
        })
        .collect::<Result<Vec<_>>>()?
        .into_iter()
        .flatten()
        .collect();
    Ok(ConstraintsSet { constraints })
}
