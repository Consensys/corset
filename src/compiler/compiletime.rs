use anyhow::*;
use std::{cell::RefCell, rc::Rc};

use super::{
    definitions::SymbolTable,
    generator::{make_src_error, reduce},
    Ast, AstNode, Token,
};

fn reduce_compiletime(
    e: &AstNode,
    root_ctx: Rc<RefCell<SymbolTable>>,
    ctx: &mut Rc<RefCell<SymbolTable>>,
) -> Result<()> {
    match &e.class {
        Token::DefModule(name) => {
            *ctx = SymbolTable::derived(root_ctx, name);
            Ok(())
        }
        Token::DefConsts(cs) => {
            for (name, exp) in cs.iter() {
                let (value, _) = reduce(exp, root_ctx.clone(), ctx)?.unwrap();
                ctx.borrow_mut().insert_constant(
                    name,
                    value.pure_eval().with_context(|| make_src_error(exp))?,
                )?;
            }
            Ok(())
        }
        _ => Ok(()),
    }
}

pub fn pass(ast: &Ast, ctx: Rc<RefCell<SymbolTable>>) -> Result<()> {
    let mut module = ctx.clone();
    for exp in ast.exprs.iter() {
        reduce_compiletime(exp, ctx.clone(), &mut module).with_context(|| make_src_error(exp))?;
    }

    Ok(())
}
