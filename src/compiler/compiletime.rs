#![cfg(feature = "parser")]
use anyhow::*;

use super::{
    generator::{make_ast_error, reduce},
    tables::Scope,
    Ast, AstNode, CompileSettings, Token,
};

fn compile_time_constants(e: &AstNode, ctx: &mut Scope, settings: &CompileSettings) -> Result<()> {
    match &e.class {
        Token::DefModule(name) => {
            *ctx = ctx.switch_to_module(name)?;
            Ok(())
        }
        Token::DefConsts(cs) => {
            for constant in cs.iter() {
                if let Token::DefConst(name, exp) = &constant.class {
                    let value = reduce(&exp, ctx, settings)?
                        .unwrap()
                        .pure_eval()
                        .with_context(|| make_ast_error(&exp))?;

                    ctx.insert_constant(name.as_symbol()?, value, true)?;
                }
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
