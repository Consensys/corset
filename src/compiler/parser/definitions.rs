use anyhow::Context;
use anyhow::*;
use crossterm::style::Stylize;
use num_traits::ToPrimitive;
use owo_colors::OwoColorize;

use crate::compiler::generator::{self, Defined, Function, FunctionClass, Specialization};
use crate::compiler::tables::Scope;
use crate::compiler::{CompileSettings, Magma, Node};
use crate::structs::Handle;
use crate::utils::hash_strings;

use super::{Ast, AstNode, Kind, Token};

fn reduce(e: &AstNode, ctx: &mut Scope, settings: &CompileSettings) -> Result<()> {
    match &e.class {
        Token::Value(_)
        | Token::Symbol(_)
        | Token::Keyword(_)
        | Token::List(_)
        | Token::Domain(_)
        | Token::DefLookup { .. }
        | Token::Defpurefun { .. }
        | Token::DefConsts { .. }
        | Token::DefInrange(..) => Ok(()),

        Token::IndexedSymbol { name: _, index } => reduce(index, ctx, settings),
        Token::DefConstraint { name, .. } => ctx.insert_constraint(name),
        Token::DefModule(name) => {
            *ctx = ctx.switch_to_module(name)?.public(true);
            Ok(())
        }
        Token::DefColumns(columns) => columns
            .iter()
            .fold(Ok(()), |ax, col| ax.and(reduce(col, ctx, settings))),
        Token::DefPerspective { name, columns, .. } => {
            let mut new_ctx = ctx
                .derive(&format!("in-{}", name))?
                .public(true)
                .with_perspective(name)?;
            columns.iter().fold(Ok(()), |ax, col| {
                ax.and(reduce(col, &mut new_ctx, settings))
            })
        }
        Token::DefColumn {
            name,
            t,
            kind,
            padding_value,
            length_multiplier,
            must_prove,
            base,
        } => {
            let module_name = ctx.module();
            let symbol = Node::column()
                .handle(Handle::maybe_with_perspective(
                    module_name,
                    name,
                    ctx.perspective(),
                ))
                .kind(match kind {
                    Kind::Commitment => Kind::Commitment,
                    Kind::Computed => Kind::Computed, // unreachable?
                    Kind::Expression(_) => Kind::Computed,
                })
                .and_padding_value(*padding_value)
                .and_length_multiplier(*length_multiplier)
                .t(t.m())
                .must_prove(*must_prove)
                .base(*base)
                .build();
            ctx.insert_symbol(name, symbol)
        }
        Token::DefArrayColumn {
            name,
            domain,
            t,
            padding_value,
            must_prove,
            base,
        } => {
            let handle = Handle::maybe_with_perspective(ctx.module(), name, ctx.perspective());
            // those are inserted for symbol lookups
            let domain = domain.concretize(|n| {
                crate::compiler::generator::reduce(n, &mut ctx.clone(), settings)
                    .transpose()
                    .unwrap()
                    .and_then(|r| r.pure_eval())
                    .and_then(|bi| bi.to_isize().ok_or_else(|| anyhow!("{} is not an i64", bi)))
            })?;

            if domain.is_empty() {
                bail!(
                    "empty domain {} for {}",
                    domain.to_string().bold().yellow(),
                    name.bold().bright_white()
                );
            }

            for i in domain.iter() {
                let ith_handle = handle.ith(i.try_into().unwrap());
                ctx.insert_symbol(
                    &ith_handle.name,
                    Node::column()
                        .handle(ith_handle.clone())
                        .kind(Kind::Commitment)
                        .and_padding_value(*padding_value)
                        .t(t.m())
                        .must_prove(*must_prove)
                        .base(*base)
                        .build(),
                )?;
            }

            // and this one for validating calls to `nth`
            ctx.insert_symbol(
                name,
                Node::array_column()
                    .handle(handle)
                    .domain(domain)
                    .base(*base)
                    .t(t.m())
                    .build(),
            )?;
            Ok(())
        }
        Token::DefInterleaving { target, froms } => {
            let node = Node::column()
                .handle(Handle::maybe_with_perspective(
                    // TODO unsure about this
                    ctx.module(),
                    target.name.clone(),
                    ctx.perspective(),
                ))
                .kind(Kind::Computed)
                .base(target.base)
                .t(froms.iter().try_fold(Magma::BINARY, |ax, f| {
                    generator::reduce(f, ctx, settings)
                        .transpose()
                        .unwrap()
                        .map(|s| s.t().m().max(ax))
                })?)
                .build();

            ctx.insert_symbol(&target.name, node)
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
            let froms = froms
                .iter()
                .map(|f| generator::reduce(f, ctx, settings).transpose().unwrap())
                .collect::<Result<Vec<_>>>()?;
            let suffix = hash_strings(froms.iter().map(|f| match f.e() {
                crate::compiler::Expression::Const(_) => todo!(),
                crate::compiler::Expression::Funcall { .. } => todo!(),
                crate::compiler::Expression::Column { handle, .. }
                | crate::compiler::Expression::ArrayColumn { handle, .. }
                | crate::compiler::Expression::ExoColumn { handle, .. } => {
                    handle.as_handle().name.to_owned()
                }
                crate::compiler::Expression::List(_) => todo!(),
                crate::compiler::Expression::Void => todo!(),
            }));
            for pair in tos.iter().zip(froms.iter()) {
                let to = pair.0;
                let from = pair.1;
                let from_m = from.t().m();
                ctx.insert_symbol(
                    &to.name,
                    Node::column()
                        .handle(Handle::maybe_with_perspective(
                            ctx.module(),
                            to.name.clone(),
                            from.perspective().map(|p| format!("{p}-srt-{suffix}")),
                        ))
                        .kind(Kind::Computed)
                        .t(from_m)
                        .base(to.base)
                        .build(),
                )
                .with_context(|| anyhow!("while defining permutation: {}", e))?;
            }
            Ok(())
        }
        Token::DefAliases(aliases) => aliases
            .iter()
            .fold(Ok(()), |ax, alias| ax.and(reduce(alias, ctx, settings))),
        Token::Defun {
            name,
            args,
            body,
            in_types,
            out_type,
            force,
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
                            force: *force,
                        }],
                    }),
                },
            )
        }
        Token::DefAlias(from, to) => {
            // NOTE: false here (for used) is needed to prevent this
            // symbol resolution from marking the column being aliased
            // as used.
            let _ = ctx
                .resolve_symbol(to, false)
                .with_context(|| anyhow!("while defining alias `{}`", from))?;

            ctx.insert_alias(from, to)
                .with_context(|| anyhow!("defining {} -> {}", from, to))
        }
        Token::DefunAlias(from, to) => ctx
            .insert_funalias(from, to)
            .with_context(|| anyhow!("defining {} -> {}", from, to)),
        Token::BlockComment(_) | Token::InlineComment(_) => unreachable!(),
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
