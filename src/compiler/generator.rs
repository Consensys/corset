use eyre::*;
use std::cell::RefCell;
use std::rc::Rc;

use super::common::*;
use crate::compiler::definitions::SymbolTable;
use crate::compiler::parser::*;
use crate::utils::*;

#[derive(Debug)]
pub struct ConstraintsSet {
    pub constraints: Vec<Constraint>,
}

// Compared to a function, a form do not evaluate all of its arguments by default
fn apply_form(
    f: Form,
    args: &[AstNode],
    ctx: Rc<RefCell<SymbolTable>>,
) -> Result<Option<Constraint>> {
    let args = f
        .validate_args(args.to_vec())
        .with_context(|| eyre!("evaluating call to {:?}", f))?;

    match f {
        // TODO in compilation
        Form::For => {
            if let (Token::Symbol(i_name), Token::Range(is), body) =
                (&args[0].class, &args[1].class, &args[2])
            {
                let mut l = vec![];
                for i in is {
                    let new_ctx = SymbolTable::derived(ctx.clone());
                    new_ctx
                        .borrow_mut()
                        .insert_symbol(i_name, Constraint::Const(*i as i32))?;

                    let r = reduce(&body.clone(), new_ctx)?.unwrap();
                    l.push(r);
                }

                Ok(Some(Constraint::List(l)))
            } else {
                unreachable!()
            }
        }
    }
}

fn apply(
    f: &Function,
    args: &[AstNode],
    ctx: Rc<RefCell<SymbolTable>>,
) -> Result<Option<Constraint>> {
    if let FunctionClass::SpecialForm(sf) = f.class {
        apply_form(sf, args, ctx)
    } else {
        let mut traversed_args: Vec<Constraint> = vec![];
        for arg in args.iter() {
            let traversed = reduce(arg, ctx.clone())?;
            if let Some(traversed) = traversed {
                traversed_args.push(traversed);
            }
        }

        match &f.class {
            FunctionClass::Builtin(b) => {
                let traversed_args = b
                    .validate_args(traversed_args)
                    .with_context(|| eyre!("validating call to `{}`", f.name))?;
                match b {
                    Builtin::Begin => Ok(Some(Constraint::List(traversed_args))),
                    Builtin::BranchIfZero => {
                        let cond = traversed_args[0].clone();
                        if let Constraint::List(then) = &traversed_args[1] {
                            Ok(Some(Constraint::List(
                                then.iter()
                                    .map(|a| Constraint::Funcall {
                                        func: Builtin::IfZero,
                                        args: vec![cond.clone(), a.clone()],
                                    })
                                    .collect(),
                            )))
                        } else {
                            unreachable!()
                        }
                    }
                    Builtin::BranchIfZeroElse => {
                        let cond = traversed_args[0].clone();
                        if let (Constraint::List(tthen), Constraint::List(eelse)) =
                            (&traversed_args[1], &traversed_args[2])
                        {
                            Ok(Some(Constraint::List(
                                tthen
                                    .iter()
                                    .cloned()
                                    .flat_map(|c: Constraint| {
                                        c.flat_fold(&|x| Constraint::Funcall {
                                            func: Builtin::IfZero,
                                            args: vec![cond.clone(), x.clone()],
                                        })
                                    })
                                    .into_iter()
                                    .chain(eelse.iter().cloned().flat_map(|c: Constraint| {
                                        c.flat_fold(&|x| Constraint::Funcall {
                                            func: Builtin::Mul,
                                            args: vec![cond.clone(), x.clone()],
                                        })
                                    }))
                                    .collect(),
                            )))
                        } else {
                            unreachable!()
                        }
                    }
                    Builtin::BranchIfNotZero => {
                        let cond = traversed_args[0].clone();
                        if let Constraint::List(then) = &traversed_args[1] {
                            Ok(Some(Constraint::List(
                                then.iter()
                                    .map(|a| Constraint::Funcall {
                                        func: Builtin::Mul,
                                        args: vec![cond.clone(), a.clone()],
                                    })
                                    .collect(),
                            )))
                        } else {
                            unreachable!()
                        }
                    }
                    Builtin::BranchIfNotZeroElse => {
                        let cond = traversed_args[0].clone();
                        if let (Constraint::List(tthen), Constraint::List(eelse)) =
                            (&traversed_args[1], &traversed_args[2])
                        {
                            Ok(Some(Constraint::List(
                                tthen
                                    .iter()
                                    .cloned()
                                    .flat_map(|c: Constraint| {
                                        c.flat_fold(&|x| Constraint::Funcall {
                                            func: Builtin::Mul,
                                            args: vec![cond.clone(), x.clone()],
                                        })
                                    })
                                    .into_iter()
                                    .chain(eelse.iter().cloned().flat_map(|c: Constraint| {
                                        c.flat_fold(&|x| Constraint::Funcall {
                                            func: Builtin::IfZero,
                                            args: vec![cond.clone(), x.clone()],
                                        })
                                    }))
                                    .collect(),
                            )))
                        } else {
                            unreachable!()
                        }
                    }
                    Builtin::Nth => {
                        if let (Constraint::ArrayColumn(cname, ..), Constraint::Const(x)) =
                            (&traversed_args[0], &traversed_args[1])
                        {
                            let x = *x as usize;
                            match &ctx.borrow().resolve_symbol(cname)? {
                                array @ Constraint::ArrayColumn(name, range) => {
                                    if range.contains(&x) {
                                        Ok(Some(Constraint::ArrayColumnElement(name.to_owned(), x)))
                                    } else {
                                        Err(eyre!("tried to access `{:?}` at index {}", array, x))
                                    }
                                }
                                _ => unimplemented!(),
                            }
                        } else {
                            unreachable!()
                        }
                    }

                    b => Ok(Some(Constraint::Funcall {
                        func: *b,
                        args: traversed_args,
                    })),
                }
            }

            FunctionClass::UserDefined(b @ Defined { args: f_args, body }) => {
                let traversed_args = b
                    .validate_args(traversed_args)
                    .with_context(|| eyre!("validating call to `{}`", f.name))?;
                let new_ctx = SymbolTable::derived(ctx);
                for (i, f_arg) in f_args.iter().enumerate() {
                    new_ctx
                        .borrow_mut()
                        .insert_symbol(f_arg, traversed_args[i].clone())?;
                }

                reduce(body, new_ctx)
            }
            _ => unimplemented!("{:?}", f),
        }
    }
}

fn reduce(e: &AstNode, ctx: Rc<RefCell<SymbolTable>>) -> Result<Option<Constraint>> {
    match &e.class {
        Token::Ignore => Ok(None),
        Token::Value(x) => Ok(Some(Constraint::Const(*x))),
        Token::Symbol(name) => Ok(Some(ctx.borrow_mut().resolve_symbol(name)?)),
        Token::Form(args) => {
            if args.is_empty() {
                Ok(Some(Constraint::List(vec![])))
            } else if let Token::Symbol(verb) = &args[0].class {
                let func = ctx
                    .borrow()
                    .resolve_function(verb)
                    .with_context(|| eyre!("resolving function `{}`", verb))?;

                apply(&func, &args[1..], ctx)
            } else {
                Err(eyre!("Not a function: {:?}", args[0]))
            }
        }
        Token::DefConstraint(name, expr) => Ok(Some(Constraint::TopLevel {
            name: name.into(),
            expr: Box::new(reduce(expr, ctx)?.unwrap()), // the parser ensures that the body is never empty
        })),

        Token::Range(_) => Ok(None),
        Token::DefColumns(_) => Ok(None),
        Token::DefColumn(_) => Ok(None),
        Token::DefArrayColumn(..) => Ok(None),
        Token::DefAliases(_) => Ok(None),
        Token::DefAlias(..) => Ok(None),
        Token::DefunAlias(..) => Ok(None),
        Token::DefConst(..) => Ok(None),
        Token::Defun(..) => Ok(None),
    }
    .with_context(|| format!("at line {}, col.{}: \"{}\"", e.lc.0, e.lc.1, e.src))
}

pub fn pass(ast: &Ast, ctx: Rc<RefCell<SymbolTable>>) -> Result<Vec<Constraint>> {
    let mut r = vec![];

    for exp in ast.exprs.iter().cloned() {
        if let Some(c) = reduce(&exp, ctx.clone())
            .with_context(|| format!("at line {}, col.{}: \"{}\"", exp.lc.0, exp.lc.1, exp.src))?
        {
            r.push(c)
        }
    }
    Ok(r)
}
