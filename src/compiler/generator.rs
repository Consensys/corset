use eyre::*;
use std::cell::RefCell;
use std::fmt::Debug;
use std::rc::Rc;

use crate::compiler::define;
use crate::compiler::define::SymbolTable;
use crate::compiler::parser::*;
use crate::utils::*;

trait FuncVerifier<T> {
    fn arity(&self) -> Arity;

    fn validate_types(&self, args: &[T]) -> Result<()>;

    fn validate_arity(&self, args: &[T]) -> Result<()> {
        self.arity().validate(args.len())
    }

    fn validate_args(&self, args: Vec<T>) -> Result<Vec<T>> {
        self.validate_arity(&args)
            .and_then(|_| self.validate_types(&args))
            .and(Ok(args))
    }
}

enum Arity {
    AtLeast(usize),
    AtMost(usize),
    Even,
    Odd,
    Monadic,
    Dyadic,
    Exactly(usize),
}
impl Arity {
    fn make_error(&self, l: usize) -> String {
        fn arg_count(x: usize) -> String {
            format!("{} argument{}", x, if x > 1 { "s" } else { "" })
        }
        match self {
            Arity::AtLeast(x) => format!("expected at least {}, but received {}", arg_count(*x), l),
            Arity::AtMost(x) => format!("expected at most {}, but received {}", arg_count(*x), l),
            Arity::Even => format!("expected an even numer of arguments, but received {}", l),
            Arity::Odd => format!("expected an odd numer of arguments, but received {}", l),
            Arity::Monadic => format!("expected {}, but received {}", arg_count(1), l),
            Arity::Dyadic => format!("expected {}, but received {}", arg_count(2), l),
            Arity::Exactly(x) => format!("expected {}, but received {}", arg_count(*x), l),
        }
    }

    fn validate(&self, l: usize) -> Result<()> {
        match self {
            Arity::AtLeast(x) => l >= *x,
            Arity::AtMost(x) => l <= *x,
            Arity::Even => l % 2 == 0,
            Arity::Odd => l % 2 == 1,
            Arity::Monadic => l == 1,
            Arity::Dyadic => l == 2,
            Arity::Exactly(x) => l == *x,
        }
        .then(|| ())
        .ok_or_else(|| eyre!(self.make_error(l)))
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Form {
    Defconstraint,
    Defun,
    Defalias,
    Defunalias,
    Defconst,
    For,
}
impl FuncVerifier<AstNode> for Form {
    fn arity(&self) -> Arity {
        match self {
            Form::Defun => Arity::Exactly(2),
            Form::Defalias => Arity::Even,
            Form::Defunalias => Arity::Exactly(2),
            Form::Defconst => Arity::Even,
            Form::Defconstraint => Arity::Dyadic,
            Form::For => Arity::Exactly(3),
        }
    }
    fn validate_types(&self, args: &[AstNode]) -> Result<()> {
        match self {
            Form::Defun => {
                if matches!(args[0].class, Token::Form { .. }) {
                    Ok(())
                } else {
                    Err(eyre!("invalid DEFUN syntax; received: {:?}", args))
                }
            }
            Form::Defalias | Form::Defunalias => {
                if args.iter().all(|a| matches!(a.class, Token::Symbol(_))) {
                    Ok(())
                } else {
                    Err(eyre!("DEFCOLUMNS expects only symbols"))
                }
            }
            Form::Defconst => {
                if args.chunks(2).all(|p| {
                    matches!(p[0].class, Token::Symbol(_)) && matches!(p[1].class, Token::Value(_))
                }) {
                    Ok(())
                } else {
                    Err(eyre!("DEFCONST expects alternating symbols and values"))
                }
            }
            Form::Defconstraint => {
                if matches!(args[0].class, Token::Symbol(_))
                    && matches!(args[1].class, Token::Form { .. })
                {
                    Ok(())
                } else {
                    Err(eyre!(
                        "DEFCONSTRAINT expects [NAME CONSTRAINT], received: {:?}",
                        args
                    ))
                }
            }
            Form::For => {
                if matches!(args[0].class, Token::Symbol(_))
                    && matches!(args[1].class, Token::Range(_))
                    && matches!(args[2].class, Token::Form { .. })
                {
                    Ok(())
                } else {
                    Err(eyre!(
                        "`{:?}` expects [SYMBOL VALUE] but received {:?}",
                        self,
                        args
                    ))
                }
            }
        }
    }
}

impl FuncVerifier<Constraint> for Builtin {
    fn arity(&self) -> Arity {
        match self {
            Builtin::Add => Arity::AtLeast(2),
            Builtin::Sub => Arity::AtLeast(2),
            Builtin::Mul => Arity::AtLeast(2),
            Builtin::Neg => Arity::Monadic,
            Builtin::Inv => Arity::Monadic,
            Builtin::IfZero => Arity::Dyadic,
            Builtin::Shift => Arity::Dyadic,
            Builtin::Begin => Arity::AtLeast(1),
            Builtin::BranchIfZero => Arity::Dyadic,
            Builtin::BranchIfZeroElse => Arity::Exactly(3),
            Builtin::BranchIfNotZero => Arity::Dyadic,
            Builtin::BranchIfNotZeroElse => Arity::Exactly(3),
            Builtin::Nth => Arity::Dyadic,
        }
    }
    fn validate_types(&self, args: &[Constraint]) -> Result<()> {
        match self {
            f @ (Builtin::Add | Builtin::Sub | Builtin::Mul | Builtin::IfZero) => {
                if args.iter().all(|a| !matches!(a, Constraint::List(_))) {
                    Ok(())
                } else {
                    Err(eyre!(
                        "`{:?}` expects scalar arguments but received a list",
                        f,
                    ))
                }
            }
            Builtin::Neg | Builtin::Inv => {
                if args.iter().all(|a| !matches!(a, Constraint::List(_))) {
                    Ok(())
                } else {
                    Err(eyre!(
                        "`{:?}` expects a scalar argument but received a list",
                        self
                    ))
                }
            }
            Builtin::Shift => {
                if matches!(args[0], Constraint::Column(_))
                    && matches!(args[1], Constraint::Const(x) if x != 0)
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
                if matches!(args[0], Constraint::ArrayColumn(..))
                    && matches!(args[1], Constraint::Const(x) if x >= 0)
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
            Builtin::BranchIfZero | Builtin::BranchIfNotZero => {
                if !matches!(args[0], Constraint::List(_)) && matches!(args[1], Constraint::List(_))
                {
                    Ok(())
                } else {
                    Err(eyre!(
                        "`{:?}` expects scalar arguments but received a list",
                        self
                    ))
                }
            }
            Builtin::BranchIfZeroElse | Builtin::BranchIfNotZeroElse => {
                if !matches!(args[0], Constraint::List(_))
                    && matches!(args[1], Constraint::List(_))
                    && matches!(args[2], Constraint::List(_))
                {
                    Ok(())
                } else {
                    Err(eyre!(
                        "`{:?}` expects (SCALAR, LIST, LIST) but received {:?}",
                        self,
                        args
                    ))
                }
            }
            Builtin::Begin => Ok(()),
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
    args: Vec<String>,
    body: AstNode,
}
impl FuncVerifier<Constraint> for Defined {
    fn arity(&self) -> Arity {
        Arity::Exactly(self.args.len())
    }

    fn validate_types(&self, _args: &[Constraint]) -> Result<()> {
        Ok(())
    }
}

// Compared to a function, a form do not evaluate all of its arguments by default
fn apply_form(
    f: Form,
    args: &[AstNode],
    ctx: Rc<RefCell<SymbolTable>>,
    pass: Pass,
) -> Result<Option<Constraint>> {
    let args = f
        .validate_args(args.to_vec())
        .with_context(|| eyre!("evaluating call to {:?}", f))?;

    match (f, pass) {
        (Form::Defconst, Pass::Definition) => {
            for p in args.chunks(2) {
                if let (Token::Symbol(name), Token::Value(x)) = (&p[0].class, &p[1].class) {
                    ctx.borrow_mut().insert_constant(name, *x)?
                }
            }

            Ok(None)
        }
        (Form::Defconst, Pass::Compilation) => Ok(None),

        (Form::Defalias, Pass::Definition) => {
            for p in args.chunks(2) {
                if let (Token::Symbol(from), Token::Symbol(to)) = (&p[0].class, &p[1].class) {
                    ctx.borrow_mut().insert_alias(from, to)?
                }
            }

            Ok(None)
        }
        (Form::Defalias, Pass::Compilation) => Ok(None),

        (Form::Defunalias, Pass::Definition) => {
            for p in args.chunks(2) {
                if let (Token::Symbol(from), Token::Symbol(to)) = (&p[0].class, &p[1].class) {
                    ctx.borrow_mut().insert_funalias(from, to)?
                }
            }

            Ok(None)
        }
        (Form::Defunalias, Pass::Compilation) => Ok(None),

        (Form::Defun, Pass::Definition) => {
            let header = &args[0];
            let body = &args[1];

            if let Token::Form {
                args: ref inner_args,
            } = header.class
            {
                if let Token::Symbol(fname) = &inner_args[0].class {
                    let arg_names = inner_args
                        .iter()
                        .map(|a| {
                            if let Token::Symbol(ref n) = a.class {
                                Ok(n.to_owned())
                            } else {
                                Err(eyre!("{:?} is not a valid argument", a))
                            }
                        })
                        .collect::<Result<Vec<_>>>()
                        .with_context(|| format!("parsing function {}", fname))?;

                    ctx.borrow_mut().insert_func({
                        Function {
                            name: arg_names[0].to_owned(),
                            class: FunctionClass::UserDefined(Defined {
                                args: arg_names[1..].to_vec(),
                                body: body.to_owned(),
                            }),
                        }
                    })
                } else {
                    unreachable!()
                }
            } else {
                unreachable!()
            }?;
            Ok(None)
        }
        (Form::Defun, Pass::Compilation) => Ok(None),

        (Form::Defconstraint, Pass::Compilation) => {
            ctx.borrow_mut().insert_constraint(&args[0].src)?;
            Ok(Some(Constraint::TopLevel {
                name: args[0].src.to_string(),
                expr: Box::new(reduce(&args[1], ctx, pass)?.unwrap()),
            }))
        }
        (Form::Defconstraint, Pass::Definition) => Ok(None),

        (Form::For, Pass::Definition) => Ok(None),
        (Form::For, Pass::Compilation) => {
            if let (Token::Symbol(i_name), Token::Range(is), body) =
                (&args[0].class, &args[1].class, &args[2])
            {
                let mut l = vec![];
                for i in is {
                    let new_ctx = SymbolTable::derived(ctx.clone());
                    new_ctx
                        .borrow_mut()
                        .insert_symbol(i_name, Constraint::Const(*i as i32))?;

                    let r = reduce(&body.clone(), new_ctx, pass)?.unwrap();
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
    pass: Pass,
) -> Result<Option<Constraint>> {
    if let FunctionClass::SpecialForm(sf) = f.class {
        apply_form(sf, args, ctx, pass)
    } else if matches!(pass, Pass::Compilation) {
        let mut traversed_args: Vec<Constraint> = vec![];
        for arg in args.iter() {
            let traversed = reduce(arg, ctx.clone(), pass)?;
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

                reduce(body, new_ctx, pass)
            }
            _ => unimplemented!("{:?}", f),
        }
    } else {
        Ok(None)
    }
}

fn reduce(e: &AstNode, ctx: Rc<RefCell<SymbolTable>>, pass: Pass) -> Result<Option<Constraint>> {
    match (&e.class, pass) {
        (Token::Ignore, _) => Ok(None),
        (Token::Value(x), _) => Ok(Some(Constraint::Const(*x))),
        (Token::Symbol(name), _) => Ok(Some(ctx.borrow_mut().resolve_symbol(name)?)),
        (Token::TopLevelForm { args }, _) => {
            if let Token::Symbol(verb) = &args[0].class {
                let func = ctx
                    .borrow()
                    .resolve_function(verb)
                    .with_context(|| eyre!("resolving form `{}`", verb))?;

                Ok(apply(&func, &args[1..], ctx, pass)?)
            } else {
                unimplemented!("{:?}", args)
            }
        }
        (Token::Form { args }, Pass::Compilation) => {
            if let Token::Symbol(verb) = &args[0].class {
                let func = ctx
                    .borrow()
                    .resolve_function(verb)
                    .with_context(|| eyre!("resolving function `{}`", verb))?;

                apply(&func, &args[1..], ctx, pass)
            } else {
                Err(eyre!("Not a function: {:?}", args[0]))
            }
        }
        (Token::Form { .. }, Pass::Definition) => Ok(None),
        (Token::Range(_), _) => Ok(None),

        (Token::DefColumns(cols), Pass::Definition) => {
            cols.iter()
                .map(|c| reduce(c, ctx.clone(), pass))
                .collect::<Result<Vec<_>>>()?;
            Ok(None)
        }
        (Token::DefColumns(_), Pass::Compilation) => Ok(None),

        (Token::DefColumn(name), Pass::Definition) => {
            ctx.borrow_mut()
                .insert_symbol(name, Constraint::Column(name.into()))?;
            Ok(None)
        }
        (Token::DefColumn(_), Pass::Compilation) => Ok(None),

        (Token::DefArrayColumn(name, range), Pass::Definition) => {
            ctx.borrow_mut()
                .insert_symbol(name, Constraint::ArrayColumn(name.into(), range.clone()))?;
            Ok(None)
        }
        (Token::DefArrayColumn(..), Pass::Compilation) => Ok(None),
        x => unimplemented!("{:?}", x),
    }
    .with_context(|| format!("at line {}, col.{}: \"{}\"", e.lc.0, e.lc.1, e.src))
}

fn build_constraints(
    ast: &ParsingAst,
    ctx: Rc<RefCell<SymbolTable>>,
    pass: Pass,
) -> Result<Vec<Constraint>> {
    let mut r = vec![];

    for exp in ast.exprs.iter().cloned() {
        if let Some(c) = reduce(&exp, ctx.clone(), pass)
            .with_context(|| format!("at line {}, col.{}: \"{}\"", exp.lc.0, exp.lc.1, exp.src))?
        {
            r.push(c)
        }
    }
    Ok(r)
}

#[derive(Debug, Clone, Copy)]
enum Pass {
    Definition,
    Compilation,
}
pub fn compile(sources: &[(&str, &str)]) -> Result<ConstraintsSet> {
    let mut asts = vec![];
    let ctx = Rc::new(RefCell::new(SymbolTable::new_root()));

    for (name, content) in sources.iter() {
        let ast = parse(content).with_context(|| eyre!("parsing `{}`", name))?;
        define::parse(ctx.clone())
            .with_context(|| eyre!("assembling definitions in `{}`", name))?;
        let _ = build_constraints(&ast, ctx.clone(), Pass::Definition);
        asts.push((name, ast));
    }

    let constraints = asts
        .into_iter()
        .map(|(name, ast)| {
            build_constraints(&ast, ctx.clone(), Pass::Compilation)
                .with_context(|| eyre!("compiling constraints in `{}`", name))
        })
        .collect::<Result<Vec<_>>>()?
        .into_iter()
        .flatten()
        .collect();
    Ok(ConstraintsSet { constraints })
}
