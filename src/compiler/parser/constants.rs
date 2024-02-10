use anyhow::*;

use crate::compiler::{generator::make_ast_error, tables::Scope, CompileSettings, Node};

use super::{Ast, AstNode, Token};

fn reduce(e: &AstNode, ctx: &mut Scope, settings: &CompileSettings) -> Result<()> {
    match &e.class {
        Token::DefModule(name) => {
            *ctx = ctx.switch_to_module(name)?.public(true);
            Ok(())
        }

        Token::DefConsts(cs) => {
            for (name, exp) in cs.iter() {
                let value = match &exp.class {
                    // If the constant value is iota, assign it to a deterministic pseudo-random value
                    Token::Symbol(x) if ["iota", "ι", "ɩ"].contains(&x.as_str()) => {
                        let mut hasher = std::collections::hash_map::DefaultHasher::new();
                        std::hash::Hash::hash(&name, &mut hasher);
                        Node::from_const((std::hash::Hasher::finish(&hasher) >> 1) as isize)
                    }
                    _ => crate::compiler::generator::reduce(exp, ctx, settings)?.unwrap(),
                };
                ctx.insert_constant(
                    name,
                    value.pure_eval().with_context(|| make_ast_error(exp))?,
                    true,
                )?;
            }
            Ok(())
        }
        _ => Ok(()),
    }
}

/// The `Definitions` pass skim through an [`Ast`] and fill the
/// [`SymbolTableTree`] with all the required elements (columns, functions,
/// perspectives, constraints, aliases, ...)
pub fn pass(ast: &Ast, ctx: Scope, settings: &CompileSettings) -> Result<()> {
    let mut module = ctx;
    for e in ast.exprs.iter() {
        reduce(e, &mut module, settings)?;
    }

    Ok(())
}
