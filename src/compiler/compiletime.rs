#![cfg(feature = "parser")]
use anyhow::*;

use super::{
    generator::{make_ast_error, reduce},
    tables::Scope,
    Ast, AstNode, CompileSettings, Node, Token,
};

fn compile_time_constants(e: &AstNode, ctx: &mut Scope, settings: &CompileSettings) -> Result<()> {
    match &e.class {
        Token::DefModule(name) => {
            *ctx = ctx.switch_to_module(name)?;
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
                    _ => reduce(exp, ctx, settings)?.unwrap(),
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

pub fn pass(ast: &Ast, ctx: Scope, settings: &CompileSettings) -> Result<()> {
    let mut module = ctx;
    for exp in ast.exprs.iter() {
        compile_time_constants(exp, &mut module, settings).with_context(|| make_ast_error(exp))?;
    }

    Ok(())
}
