use anyhow::*;
use log::*;
use num_bigint::BigInt;
use num_traits::FromPrimitive;

use super::generator::{Defined, Function, FunctionClass};
use super::tables::Scope;
use super::{ColumnRef, Expression, Node};
use crate::column::Computation;
use crate::compiler::parser::*;
use crate::pretty::Base;
use crate::structs::Handle;

fn reduce(e: &AstNode, ctx: &mut Scope) -> Result<()> {
    match &e.class {
        Token::Value(_)
        | Token::Symbol(_)
        | Token::Keyword(_)
        | Token::List(_)
        | Token::Range(_)
        | Token::Type(_)
        | Token::DefPlookup { .. }
        | Token::DefInrange(..) => Ok(()),

        Token::DefConstraint { name, .. } => ctx.insert_constraint(name),
        Token::DefModule(name) => {
            *ctx = ctx.derived_from_root(name, false, true, false);
            Ok(())
        }
        Token::DefColumns(columns) => columns
            .iter()
            .fold(Ok(()), |ax, col| ax.and(reduce(col, ctx))),
        Token::DefPerspective { name, columns, .. } => {
            let mut new_ctx = ctx.create_perspective(name)?;
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
            let symbol = Node {
                _e: Expression::Column {
                    handle: Handle::maybe_with_perspective(module_name, col, ctx.perspective())
                        .into(),
                    kind: match kind {
                        Kind::Atomic => Kind::Atomic,
                        Kind::Phantom => Kind::Phantom,
                        Kind::Composite(_) => Kind::Phantom, // The actual expression is computed by the generator
                        Kind::Interleaved(_, _) => Kind::Phantom, // The interleaving is later on set by the generator
                    },
                    padding_value: padding_value.to_owned(),
                    base: *base,
                },
                _t: Some(*t),
            };
            ctx.insert_symbol(col, symbol)
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
                ctx.insert_symbol(
                    &handle.ith(*i).name,
                    Node {
                        _e: Expression::Column {
                            handle: handle.ith(*i).into(),
                            kind: Kind::Atomic,
                            base: *base,
                            padding_value: None,
                        },
                        _t: Some(*t),
                    },
                )?;
            }

            // and this one for validating calls to `nth`
            ctx.insert_symbol(
                col,
                Node {
                    _e: Expression::ArrayColumn {
                        handle,
                        domain: range.to_owned(),
                        base: *base,
                    },
                    _t: Some(*t),
                },
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
            signs,
        } => {
            if tos.len() != froms.len() {
                bail!(
                    "cardinality mismatch in permutation declaration: {:?} vs. {:?}",
                    tos,
                    froms
                );
            }

            let mut _froms = Vec::<ColumnRef>::new();
            let mut _tos = Vec::<ColumnRef>::new();
            for (to, from) in tos.iter().zip(froms.iter()) {
                let to_handle = Handle::new(ctx.module(), to);
                let from_actual = ctx.resolve_symbol(from)?;

                if let Expression::Column { handle, .. } = from_actual.e() {
                    ctx.insert_symbol(
                        to,
                        Node::column()
                            .handle(to_handle.clone())
                            .kind(Kind::Phantom)
                            .t(from_actual.t().magma())
                            .base(Base::Hex)
                            .build(),
                    )
                    .unwrap_or_else(|e| warn!("while defining permutation: {}", e));
                    _froms.push(handle.clone());
                    _tos.push(to_handle.into());
                } else {
                    unreachable!()
                };
            }

            ctx.insert_many_computations(
                &_tos,
                Computation::Sorted {
                    froms: _froms,
                    tos: _tos.clone(),
                    signs: signs.clone(),
                },
            )?;
            Ok(())
        }
        Token::DefAliases(aliases) => aliases
            .iter()
            .fold(Ok(()), |ax, alias| ax.and(reduce(alias, ctx))),
        Token::Defun { name, args, body } => {
            let module_name = ctx.module();
            ctx.insert_function(
                name,
                Function {
                    handle: Handle::new(module_name, name),
                    class: FunctionClass::UserDefined(Defined {
                        pure: false,
                        args: args.to_owned(),
                        body: *body.clone(),
                    }),
                },
            )
        }
        Token::Defpurefun { name, args, body } => {
            let module_name = ctx.module();
            ctx.insert_function(
                name,
                Function {
                    handle: Handle::new(module_name, name),
                    class: FunctionClass::UserDefined(Defined {
                        pure: true,
                        args: args.to_owned(),
                        body: *body.clone(),
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

pub fn pass(ast: &Ast, ctx: Scope) -> Result<()> {
    let mut module = ctx;
    for e in ast.exprs.iter() {
        reduce(e, &mut module)?;
    }

    Ok(())
}
