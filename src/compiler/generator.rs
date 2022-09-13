use eyre::*;
use num_bigint::BigInt;
use num_traits::cast::ToPrimitive;
use num_traits::{One, Zero};
use std::cell::RefCell;
use std::fmt::{Debug, Formatter};
use std::rc::Rc;

use super::common::*;
use crate::column::ColumnSet;
use crate::compiler::definitions::SymbolTable;
use crate::compiler::parser::*;

#[derive(Debug)]
pub enum Constraint {
    Vanishes {
        name: String,
        domain: Option<Vec<isize>>,
        expr: Box<Expression>,
    },
    Plookup(Vec<Expression>, Vec<Expression>),
}

#[derive(Clone)]
pub enum Expression {
    Funcall {
        func: Builtin,
        args: Vec<Expression>,
    },
    Const(BigInt),
    Column(String, Type, Kind<Expression>),
    ArrayColumn(String, Vec<usize>, Type),
    ArrayColumnElement(String, usize, Type),
    List(Vec<Expression>),
}
impl Expression {
    pub fn flat_fold<T>(&self, f: &dyn Fn(&Expression) -> T) -> Vec<T> {
        let mut ax = vec![];
        match self {
            Expression::List(xs) => {
                for x in xs {
                    ax.push(f(x));
                }
            }
            x => ax.push(f(x)),
        }
        ax
    }
    pub fn len(&self) -> usize {
        match self {
            Expression::List(exps) => exps.len(),
            _ => 1,
        }
    }
}
impl Debug for Expression {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        fn format_list(cs: &[Expression]) -> String {
            cs.iter()
                .map(|c| format!("{:?}", c))
                .collect::<Vec<_>>()
                .join(" ")
        }

        match self {
            Expression::Const(x) => write!(f, "{}:CONST", x),
            Expression::Column(name, t, k) => write!(f, "{}:{{{:?}}}:{:?}", name, t, k),
            Expression::ArrayColumn(name, range, t) => {
                write!(
                    f,
                    "{}[{}:{}]:ARRAYCOLUMN{{{:?}}}",
                    name,
                    range.first().unwrap(),
                    range.last().unwrap(),
                    t
                )
            }
            Expression::ArrayColumnElement(name, i, t) => {
                write!(f, "{}[{}]:COLUMN{{{:?}}}", name, i, t)
            }
            Expression::List(cs) => write!(f, "'({})", format_list(cs)),
            Self::Funcall { func, args } => {
                write!(f, "({:?} {})", func, format_list(args))
            }
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Builtin {
    Add,
    Sub,
    Mul,
    Shift,
    Neg,
    Inv,
    Nth,

    Begin,

    IfZero,
    IfNotZero,
}
impl Builtin {
    fn typing(&self, argtype: &[Type]) -> Type {
        match self {
            Builtin::Add | Builtin::Sub | Builtin::Neg | Builtin::Inv => Type::Numeric,
            Builtin::Mul => {
                if argtype.iter().all(|t| matches!(t, Type::Boolean)) {
                    Type::Boolean
                } else {
                    Type::Numeric
                }
            }
            Builtin::IfZero | Builtin::IfNotZero => {
                std::cmp::min(argtype[1], *argtype.get(2).unwrap_or(&Type::Boolean))
            }
            Builtin::Begin => *argtype.iter().max().unwrap(),
            Builtin::Shift | Builtin::Nth => argtype[0],
        }
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub class: FunctionClass,
}
#[derive(Debug, Clone)]
pub enum FunctionClass {
    UserDefined(Defined),
    SpecialForm(Form),
    Builtin(Builtin),
    Alias(String),
}

#[derive(Debug, Clone)]
pub struct Defined {
    pub args: Vec<String>,
    pub body: AstNode,
}
impl FuncVerifier<Expression> for Defined {
    fn arity(&self) -> Arity {
        Arity::Exactly(self.args.len())
    }

    fn validate_types(&self, _args: &[Expression]) -> Result<()> {
        Ok(())
    }
}

impl FuncVerifier<Expression> for Builtin {
    fn arity(&self) -> Arity {
        match self {
            Builtin::Add => Arity::AtLeast(2),
            Builtin::Sub => Arity::AtLeast(2),
            Builtin::Mul => Arity::AtLeast(2),
            Builtin::Neg => Arity::Monadic,
            Builtin::Inv => Arity::Monadic,
            Builtin::Shift => Arity::Dyadic,
            Builtin::Begin => Arity::AtLeast(1),
            Builtin::IfZero => Arity::Between(2, 3),
            Builtin::IfNotZero => Arity::Between(2, 3),
            Builtin::Nth => Arity::Dyadic,
        }
    }
    fn validate_types(&self, args: &[Expression]) -> Result<()> {
        match self {
            f @ (Builtin::Add | Builtin::Sub | Builtin::Mul) => {
                if args.iter().all(|a| !matches!(a, Expression::List(_))) {
                    Ok(())
                } else {
                    Err(eyre!(
                        "`{:?}` expects scalar arguments but received a list",
                        f,
                    ))
                }
            }
            Builtin::Neg | Builtin::Inv => {
                if args.iter().all(|a| !matches!(a, Expression::List(_))) {
                    Ok(())
                } else {
                    Err(eyre!(
                        "`{:?}` expects a scalar argument but received a list",
                        self
                    ))
                }
            }
            Builtin::Shift => {
                if matches!(args[0], Expression::Column(_, _, _))
                    && matches!(&args[1], Expression::Const(x) if !Zero::is_zero(x))
                {
                    Ok(())
                } else {
                    Err(eyre!(
                        "`{:?}` expects a COLUMN and a non-null INTEGER but received {:?}",
                        self,
                        args
                    ))
                }
            }
            Builtin::Nth => {
                if matches!(args[0], Expression::ArrayColumn(..))
                    && matches!(&args[1], Expression::Const(x) if x.sign() != num_bigint::Sign::Minus)
                {
                    Ok(())
                } else {
                    Err(eyre!(
                        "`{:?}` expects [SYMBOL CONST] but received {:?}",
                        self,
                        args
                    ))
                }
            }
            Builtin::IfZero | Builtin::IfNotZero => {
                if !matches!(args[0], Expression::List(_)) {
                    Ok(())
                } else {
                    Err(eyre!("`{:?}` expects an expression as its condition", self))
                }
            }
            Builtin::Begin => Ok(()),
        }
    }
}

#[derive(Default, Debug)]
pub struct ConstraintsSet {
    pub columns: ColumnSet<u32>,
    pub constraints: Vec<Constraint>,
}

// Compared to a function, a form do not evaluate all of its arguments by default
fn apply_form(
    f: Form,
    args: &[AstNode],
    ctx: Rc<RefCell<SymbolTable>>,
    module: &mut String,
) -> Result<Option<(Expression, Type)>> {
    let args = f
        .validate_args(args.to_vec())
        .with_context(|| eyre!("evaluating call to {:?}", f))?;

    match f {
        Form::For => {
            if let (Token::Symbol(i_name), Token::Range(is), body) =
                (&args[0].class, &args[1].class, &args[2])
            {
                let mut l = vec![];
                let mut t = Type::Boolean;
                for i in is {
                    let new_ctx = SymbolTable::derived(ctx.clone());
                    new_ctx.borrow_mut().insert_symbol(
                        &module,
                        i_name,
                        Expression::Const(BigInt::from(*i)),
                    )?;

                    let (r, to) = reduce(&body.clone(), new_ctx, module)?.unwrap();
                    l.push(r);
                    t = t.max(to)
                }

                Ok(Some((Expression::List(l), t)))
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
    module: &mut String,
) -> Result<Option<(Expression, Type)>> {
    if let FunctionClass::SpecialForm(sf) = f.class {
        apply_form(sf, args, ctx, module)
    } else {
        let mut traversed_args = vec![];
        let mut traversed_args_t = vec![];
        for arg in args.iter() {
            let traversed = reduce(arg, ctx.clone(), module)?;
            if let Some((traversed, t)) = traversed {
                traversed_args.push(traversed);
                traversed_args_t.push(t);
            }
        }

        match &f.class {
            FunctionClass::Builtin(b) => {
                let traversed_args = b
                    .validate_args(traversed_args)
                    .with_context(|| eyre!("validating call to `{}`", f.name))?;
                let cond = traversed_args[0].clone();
                match b {
                    Builtin::Begin => Ok(Some((
                        Expression::List(traversed_args),
                        *traversed_args_t.iter().max().unwrap(),
                    ))),

                    b @ (Builtin::IfZero | Builtin::IfNotZero) => {
                        let conds = {
                            let cond_not_zero = cond.clone();
                            // If the condition is binary, cond_not_zero = 1 - x...
                            let cond_zero = if matches!(traversed_args_t[0], Type::Boolean) {
                                Expression::Funcall {
                                    func: Builtin::Sub,
                                    args: vec![
                                        Expression::Const(One::one()),
                                        cond_not_zero.clone(),
                                    ],
                                }
                            } else {
                                // ...otherwise, cond_zero = 1 - x.INV(x)
                                Expression::Funcall {
                                    func: Builtin::Sub,
                                    args: vec![
                                        Expression::Const(One::one()),
                                        Expression::Funcall {
                                            func: Builtin::Mul,
                                            args: vec![
                                                cond.clone(),
                                                Expression::Funcall {
                                                    func: Builtin::Inv,
                                                    args: vec![cond],
                                                },
                                            ],
                                        },
                                    ],
                                }
                            };
                            match b {
                                Builtin::IfZero => [cond_zero, cond_not_zero],
                                Builtin::IfNotZero => [cond_not_zero, cond_zero],
                                _ => unreachable!(),
                            }
                        };

                        // Order the then/else blocks
                        let t = traversed_args_t.iter().max().unwrap();
                        let then_else = vec![traversed_args.get(1), traversed_args.get(2)]
                            .into_iter()
                            .enumerate()
                            // Only keep the non-empty branches
                            .filter_map(|(i, ex)| ex.map(|ex| (i, ex)))
                            // Ensure branches are wrapped in in lists
                            .map(|(i, ex)| {
                                (
                                    i,
                                    match ex {
                                        Expression::List(_) => ex.clone(),
                                        ex => Expression::List(vec![ex.clone()]),
                                    },
                                )
                            })
                            // Map the corresponding then/else operations on the branches
                            .flat_map(|(i, exs)| {
                                if let Expression::List(exs) = exs {
                                    exs.into_iter()
                                        .map(|ex: Expression| {
                                            ex.flat_fold(&|ex| Expression::Funcall {
                                                func: Builtin::Mul,
                                                args: vec![conds[i].clone(), ex.clone()],
                                            })
                                        })
                                        .collect::<Vec<_>>()
                                } else {
                                    unreachable!()
                                }
                            })
                            .flatten()
                            .collect::<Vec<_>>();
                        if then_else.len() == 1 {
                            Ok(Some((then_else[0].clone(), *t)))
                        } else {
                            Ok(Some((Expression::List(then_else), *t)))
                        }
                    }

                    Builtin::Nth => {
                        if let (Expression::ArrayColumn(cname, ..), Expression::Const(x)) =
                            (&traversed_args[0], &traversed_args[1])
                        {
                            let x = x.to_usize().unwrap();
                            match &ctx.borrow_mut().resolve_symbol(module, cname)? {
                                array @ (Expression::ArrayColumn(name, range, t), _) => {
                                    if range.contains(&x) {
                                        Ok(Some((
                                            Expression::ArrayColumnElement(name.to_owned(), x, *t),
                                            *t,
                                        )))
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

                    b => Ok(Some((
                        Expression::Funcall {
                            func: *b,
                            args: traversed_args,
                        },
                        b.typing(&traversed_args_t),
                    ))),
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
                        .insert_symbol(module, f_arg, traversed_args[i].clone())?;
                }

                reduce(body, new_ctx, module)
            }
            _ => unimplemented!("{:?}", f),
        }
    }
}

fn reduce(
    e: &AstNode,
    ctx: Rc<RefCell<SymbolTable>>,
    module: &mut String,
) -> Result<Option<(Expression, Type)>> {
    match &e.class {
        Token::Type(_) | Token::Range(_) => Ok(None),
        Token::Value(x) => Ok(Some((
            Expression::Const(x.clone()),
            if *x >= Zero::zero() && *x <= One::one() {
                Type::Boolean
            } else {
                Type::Numeric
            },
        ))),
        Token::Symbol(name) => Ok(Some(ctx.borrow_mut().resolve_symbol(&module, name)?)),
        Token::Form(args) => {
            if args.is_empty() {
                Ok(Some((Expression::List(vec![]), Type::Void)))
            } else if let Token::Symbol(verb) = &args[0].class {
                let func = ctx
                    .borrow()
                    .resolve_function(verb)
                    .with_context(|| eyre!("resolving function `{}`", verb))?;

                apply(&func, &args[1..], ctx, module)
            } else {
                Err(eyre!("Not a function: {:?}", args[0]))
            }
        }

        Token::DefColumn(name, _, k) => match k {
            Kind::Composite(e) => {
                let e = reduce(e, ctx.clone(), module)?.unwrap();
                ctx.borrow_mut().edit_symbol(module, name, &|x| {
                    if let Expression::Column(_, _, kind) = x {
                        *kind = Kind::Composite(Box::new(e.0.clone()))
                    }
                })?;
                Ok(None)
            }
            _ => Ok(None),
        },
        Token::DefColumns(_)
        | Token::DefConstraint(..)
        | Token::DefArrayColumn(..)
        | Token::DefModule(_)
        | Token::DefAliases(_)
        | Token::DefAlias(..)
        | Token::DefunAlias(..)
        | Token::DefConst(..)
        | Token::Defun(..)
        | Token::DefPlookup(..) => unreachable!(),
    }
    .with_context(|| format!("at line {}, col.{}: \"{}\"", e.lc.0, e.lc.1, e.src))
}

fn reduce_toplevel(
    e: &AstNode,
    ctx: Rc<RefCell<SymbolTable>>,
    module: &mut String,
) -> Result<Option<Constraint>> {
    match &e.class {
        Token::DefConstraint(name, domain, expr) => Ok(Some(Constraint::Vanishes {
            name: name.into(),
            domain: domain.to_owned(),
            expr: Box::new(reduce(expr, ctx, module)?.unwrap().0), // the parser ensures that the body is never empty
        })),
        Token::DefPlookup(parent, child) => {
            let parents = parent
                .iter()
                .map(|e| reduce(e, ctx.clone(), module))
                .collect::<Result<Vec<_>>>()?
                .into_iter()
                .map(|e| e.unwrap().0)
                .collect::<Vec<_>>();
            let children = child
                .iter()
                .map(|e| reduce(e, ctx.clone(), module))
                .collect::<Result<Vec<_>>>()?
                .into_iter()
                .map(|e| e.unwrap().0)
                .collect::<Vec<_>>();
            Ok(Some(Constraint::Plookup(parents, children)))
        }
        Token::DefColumns(columns) => {
            for c in columns {
                reduce(c, ctx.clone(), module)?;
            }
            Ok(None)
        }
        Token::DefModule(name) => {
            *module = String::from(name);
            Ok(None)
        }

        Token::Value(_) | Token::Symbol(_) | Token::Form(_) | Token::Range(_) => {
            Err(eyre!("Unexpected top-level form: {:?}", e))
        }

        _ => Ok(None),
    }
}

pub fn pass(ast: &Ast, ctx: Rc<RefCell<SymbolTable>>) -> Result<Vec<Constraint>> {
    let mut r = vec![];

    let mut module = String::from(super::MAIN_MODULE);
    for exp in ast.exprs.iter().cloned() {
        if let Some(c) = reduce_toplevel(&exp, ctx.clone(), &mut module)
            .with_context(|| format!("at line {}, col.{}: \"{}\"", exp.lc.0, exp.lc.1, exp.src))?
        {
            r.push(c)
        }
    }
    Ok(r)
}
