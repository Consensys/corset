#![cfg(feature = "parser")]
use anyhow::*;

use crate::structs::Field;

use super::{
    generator::{make_ast_error, reduce},
    tables::Scope,
    Ast, AstNode, CompileSettings, Token,
};

fn compile_time_constants<F: Field>(
    e: &AstNode,
    ctx: &mut Scope<F>,
    settings: &CompileSettings,
) -> Result<()> {
    match &e.class {
        Token::DefModule(name) => {
            *ctx = ctx.switch_to_module(name)?;
            Ok(())
        }
        Token::DefConsts(cs) => {
            for (name, exp) in cs.iter() {
                let value = reduce(exp, ctx, settings)?.unwrap();
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

pub fn pass<F: Field>(ast: &Ast, ctx: Scope<F>, settings: &CompileSettings) -> Result<()> {
    let mut module = ctx;
    for exp in ast.exprs.iter() {
        compile_time_constants(exp, &mut module, settings).with_context(|| make_ast_error(exp))?;
    }

    Ok(())
}
