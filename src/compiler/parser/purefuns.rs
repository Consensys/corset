use anyhow::*;

use crate::compiler::generator::{Defined, Function, FunctionClass, Specialization};
use crate::compiler::tables::Scope;
use crate::structs::Handle;

use super::{Ast, AstNode, Token};

fn reduce(e: &AstNode, ctx: &mut Scope) -> Result<()> {
    match &e.class {
        Token::DefModule(name) => {
            *ctx = ctx.switch_to_module(name)?.public(true);
            Ok(())
        }
        Token::Defpurefun {
            name,
            args,
            body,
            in_types,
            out_type,
            nowarn,
        } => {
            let module_name = ctx.module();
            ctx.insert_function(
                name,
                Function {
                    handle: Handle::new(module_name, name),
                    class: FunctionClass::UserDefined(Defined {
                        specializations: vec![Specialization {
                            pure: true,
                            args: args.to_owned(),
                            in_types: in_types.to_vec(),
                            out_type: *out_type,
                            body: *body.clone(),
                            nowarn: *nowarn,
                        }],
                    }),
                },
            )
        }
        _ => Ok(()),
    }
}

/// The `Definitions` pass skim through an [`Ast`] and fill the
/// [`SymbolTableTree`] with all the required elements (columns, functions,
/// perspectives, constraints, aliases, ...)
pub fn pass(ast: &Ast, ctx: Scope) -> Result<()> {
    let mut module = ctx;
    for e in ast.exprs.iter() {
        reduce(e, &mut module)?;
    }

    Ok(())
}
