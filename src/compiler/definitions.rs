use anyhow::*;
use log::*;
use std::cell::RefCell;
use std::rc::Rc;

use super::generator::{Defined, Function, FunctionClass};
use super::tables::SymbolTable;
use super::{Expression, Magma, Node, Type};
use crate::column::Computation;
use crate::compiler::parser::*;
use crate::structs::Handle;

fn reduce(
    e: &AstNode,
    root_ctx: Rc<RefCell<SymbolTable>>,
    ctx: &mut Rc<RefCell<SymbolTable>>,
) -> Result<()> {
    match &e.class {
        Token::Value(_)
        | Token::Symbol(_)
        | Token::Keyword(_)
        | Token::List(_)
        | Token::Range(_)
        | Token::Type(_)
        | Token::DefPlookup { .. }
        | Token::DefConsts(..)
        | Token::DefInrange(..) => Ok(()),

        Token::DefConstraint { name, .. } => ctx.borrow_mut().insert_constraint(name),
        Token::DefModule(name) => {
            *ctx = SymbolTable::derived(root_ctx, name, name, false, true);
            Ok(())
        }
        Token::DefColumns(cols) => cols
            .iter()
            .fold(Ok(()), |ax, col| ax.and(reduce(col, root_ctx.clone(), ctx))),
        Token::DefColumn {
            name: col,
            t,
            kind,
            padding_value,
        } => {
            let module_name = ctx.borrow().name.to_owned();
            let symbol = Node {
                _e: Expression::Column(
                    Handle::new(&module_name, col),
                    // Convert Kind<AstNode> to Kind<Expression>
                    match kind {
                        Kind::Atomic => Kind::Atomic,
                        Kind::Phantom => Kind::Phantom,
                        Kind::Composite(_) => Kind::Phantom, // The actual expression is computed by the generator
                        Kind::Interleaved(_, _) => Kind::Phantom, // The interleaving is later on set by the generator
                    },
                    padding_value.to_owned(),
                ),
                _t: Some(*t),
            };
            ctx.borrow_mut().insert_symbol(col, symbol)
        }
        Token::DefArrayColumn {
            name: col,
            domain: range,
            t,
        } => {
            let handle = Handle::new(&ctx.borrow().name, col);
            ctx.borrow_mut().insert_symbol(
                col,
                Node {
                    _e: Expression::ArrayColumn(handle, range.to_owned()),
                    _t: Some(*t),
                },
            )?;
            Ok(())
        }
        Token::DefPermutation {
            from: froms,
            to: tos,
        } => {
            if tos.len() != froms.len() {
                bail!(
                    "cardinality mismatch in permutation declaration: {:?} vs. {:?}",
                    tos,
                    froms
                );
            }

            let mut _froms = Vec::new();
            let mut _tos = Vec::new();
            for (to, from) in tos.iter().zip(froms.iter()) {
                let from_handle = Handle::new(&ctx.borrow().name, &from);
                let to_handle = Handle::new(&ctx.borrow().name, &to);
                ctx.borrow_mut()
                    .resolve_symbol(from)
                    .with_context(|| "while defining permutation")?;
                ctx.borrow_mut()
                    .insert_symbol(
                        to,
                        Node {
                            _e: Expression::Column(to_handle.clone(), Kind::Phantom, None),
                            _t: Some(Type::Column(Magma::Integer)),
                        },
                    )
                    .unwrap_or_else(|e| warn!("while defining permutation: {}", e));
                _froms.push(from_handle);
                _tos.push(to_handle);
            }

            ctx.borrow_mut()
                .computation_table
                .borrow_mut()
                .insert_many(
                    &_tos,
                    Computation::Sorted {
                        froms: _froms,
                        tos: _tos.clone(),
                    },
                )?;
            Ok(())
        }
        Token::DefAliases(aliases) => aliases.iter().fold(Ok(()), |ax, alias| {
            ax.and(reduce(alias, root_ctx.clone(), ctx))
        }),
        Token::Defun { name, args, body } => {
            let module_name = ctx.borrow().name.to_owned();
            ctx.borrow_mut().insert_function(
                name,
                Function {
                    handle: Handle::new(&module_name, name),
                    class: FunctionClass::UserDefined(Defined {
                        pure: false,
                        args: args.to_owned(),
                        body: *body.clone(),
                    }),
                },
            )
        }
        Token::Defpurefun { name, args, body } => {
            let module_name = ctx.borrow().name.to_owned();
            ctx.borrow_mut().insert_function(
                name,
                Function {
                    handle: Handle::new(&module_name, name),
                    class: FunctionClass::UserDefined(Defined {
                        pure: true,
                        args: args.to_owned(),
                        body: *body.clone(),
                    }),
                },
            )
        }
        Token::DefAlias(from, to) => ctx
            .borrow_mut()
            .insert_alias(from, to)
            .with_context(|| anyhow!("defining {} -> {}", from, to)),
        Token::DefunAlias(from, to) => ctx
            .borrow_mut()
            .insert_funalias(from, to)
            .with_context(|| anyhow!("defining {} -> {}", from, to)),
    }
}

pub fn pass(ast: &Ast, ctx: Rc<RefCell<SymbolTable>>) -> Result<()> {
    let mut current_ctx = ctx.clone();
    for e in ast.exprs.iter() {
        reduce(e, ctx.clone(), &mut current_ctx)?;
    }

    Ok(())
}
