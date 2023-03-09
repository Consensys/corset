use anyhow::*;
use std::{cell::RefCell, rc::Rc};

use super::{
    definitions::SymbolTable,
    generator::{make_ast_error, reduce},
    Ast, AstNode, CompileSettings, Token,
};

fn compile_time_constants(
    e: &AstNode,
    root_ctx: Rc<RefCell<SymbolTable>>,
    ctx: &mut Rc<RefCell<SymbolTable>>,
    settings: &CompileSettings,
) -> Result<()> {
    match &e.class {
        Token::DefModule(name) => {
            *ctx = SymbolTable::derived(root_ctx, name, name, false, true);
            Ok(())
        }
        Token::DefConsts(cs) => {
            for (name, exp) in cs.iter() {
                let value = reduce(exp, ctx, settings)?.unwrap();
                ctx.borrow_mut().insert_constant(
                    name,
                    value.pure_eval().with_context(|| make_ast_error(exp))?,
                )?;
            }
            Ok(())
        }
        _ => Ok(()),
    }
}

pub fn pass(ast: &Ast, ctx: Rc<RefCell<SymbolTable>>, settings: &CompileSettings) -> Result<()> {
    let mut module = ctx.clone();
    for exp in ast.exprs.iter() {
        compile_time_constants(exp, ctx.clone(), &mut module, settings)
            .with_context(|| make_ast_error(exp))?;
    }

    Ok(())
}
