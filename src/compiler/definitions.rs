use anyhow::*;
use log::*;
use num_bigint::BigInt;
use num_traits::FromPrimitive;

use super::generator::{Defined, Function, FunctionClass, Specialization};
use super::tables::Scope;
use super::{Magma, Node};
use crate::compiler::parser::*;
use crate::structs::Handle;

fn reduce(e: &AstNode, ctx: &mut Scope) -> Result<()> {
    match &e.class {
        Token::Value(_)
        | Token::Symbol(_)
        | Token::Keyword(_)
        | Token::List(_)
        | Token::Range(_)
        | Token::DefPlookup { .. }
        | Token::DefInrange(..) => Ok(()),

        Token::IndexedSymbol { name: _, index } => reduce(index, ctx),
        Token::DefConstraint { name, .. } => ctx.insert_constraint(name),
        Token::DefModule(name) => {
            *ctx = ctx.switch_to_module(name)?.public(true);
            Ok(())
        }
        Token::DefColumns(columns) => columns
            .iter()
            .fold(Ok(()), |ax, col| ax.and(reduce(col, ctx))),
        Token::DefPerspective { name, columns, .. } => {
            let mut new_ctx = ctx
                .derive(&format!("in-{}", name))?
                .public(true)
                .with_perspective(name)?;
            columns
                .iter()
                .fold(Ok(()), |ax, col| ax.and(reduce(col, &mut new_ctx)))
        }
        Token::DefColumn {
            name: col,
            t,
            kind,
            padding_value,
            base,
        } => {
            let module_name = ctx.module();
            let symbol = Node::column()
                .handle(Handle::maybe_with_perspective(
                    module_name,
                    col,
                    ctx.perspective(),
                ))
                .base(*base)
                .kind(match kind {
                    Kind::Atomic => Kind::Atomic,
                    Kind::Phantom => Kind::Phantom,
                    // The actual expression is computed by the generator
                    Kind::Composite(_) => Kind::Phantom,
                })
                .and_padding_value(*padding_value)
                .t(t.magma())
                .build();
            ctx.insert_symbol(col, symbol)
        }
        Token::DefInterleaving { target, froms: _ } => {
            let node = Node::column()
                .handle(Handle::maybe_with_perspective(
                    // TODO unsure about this
                    ctx.module(),
                    target.name.clone(),
                    ctx.perspective(),
                ))
                .kind(Kind::Phantom)
                .base(target.base.clone())
                .build();

            ctx.insert_symbol(&target.name, node)
        }
        Token::DefArrayColumn {
            name: col,
            domain: range,
            t,
            base,
        } => {
            let handle = Handle::maybe_with_perspective(ctx.module(), col, ctx.perspective());
            // those are inserted for symbol lookups
            for i in range {
                let ith_handle = handle.ith(*i);
                ctx.insert_used_symbol(
                    &ith_handle.name,
                    Node::column()
                        .handle(ith_handle.clone())
                        .kind(Kind::Atomic)
                        .base(*base)
                        .t(t.magma())
                        .build(),
                )?;
            }

            // and this one for validating calls to `nth`
            ctx.insert_symbol(
                col,
                Node::array_column()
                    .handle(handle)
                    .domain(range.to_owned())
                    .base(*base)
                    .t(t.magma())
                    .build(),
            )?;
            Ok(())
        }
        Token::DefConsts(cs) => {
            // The actual value will be filled later on by the compile-time pass
            for c in cs.iter() {
                ctx.insert_constant(&c.0, BigInt::from_i8(0).unwrap(), false)?;
            }
            Ok(())
        }
        Token::DefPermutation {
            from: froms,
            to: tos,
            ..
        } => {
            if tos.len() != froms.len() {
                bail!(
                    "cardinality mismatch in permutation declaration: {:?} vs. {:?}",
                    tos,
                    froms
                );
            }

            for to in tos {
                ctx.insert_symbol(
                    &to.name,
                    Node::column()
                        .handle(Handle::new(ctx.module(), to.name.clone()))
                        .kind(Kind::Phantom)
                        .t(Magma::Integer) // TODO previously we took the type of the corresponding 'from' column, is that a problem?
                        .base(to.base.clone())
                        .build(),
                )
                .with_context(|| anyhow!("while defining permutation: {}", e))?;
            }
            Ok(())
        }
        Token::DefAliases(aliases) => aliases
            .iter()
            .fold(Ok(()), |ax, alias| ax.and(reduce(alias, ctx))),
        Token::Defun {
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
                            pure: false,
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
        Token::DefAlias(from, to) => {
            let _ = ctx
                .resolve_symbol(to)
                .with_context(|| anyhow!("while defining alias `{}`", from))?;

            ctx.insert_alias(from, to)
                .with_context(|| anyhow!("defining {} -> {}", from, to))
        }
        Token::DefunAlias(from, to) => ctx
            .insert_funalias(from, to)
            .with_context(|| anyhow!("defining {} -> {}", from, to)),
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
