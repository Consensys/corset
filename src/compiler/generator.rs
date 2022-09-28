use eyre::*;
use log::*;
use num_bigint::BigInt;
use num_traits::cast::ToPrimitive;
use num_traits::{One, Zero};
use pairing_ce::bn256::Fr;
use pairing_ce::ff::{Field, PrimeField};
use std::cell::RefCell;
use std::cmp::Ordering;
use std::collections::{HashMap, HashSet};
use std::fmt::{Debug, Display, Formatter};
use std::rc::Rc;

use super::common::*;
use crate::column::{Column, ColumnSet};
use crate::compiler::definitions::SymbolTable;
use crate::compiler::parser::*;
use crate::utils::*;

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
    Column(String, String, Type, Kind<Expression>), // Module Name Type Kind
    ArrayColumn(String, String, Vec<usize>, Type),
    Permutation(Vec<String>, Vec<String>),
    List(Vec<Expression>),
    Void,
}
impl Expression {
    pub fn t(&self) -> Type {
        match self {
            Expression::Funcall { func, args } => {
                func.typing(&args.iter().map(|a| a.t()).collect::<Vec<_>>())
            }
            Expression::Const(ref x) => {
                if Zero::is_zero(x) || One::is_one(x) {
                    Type::Boolean
                } else {
                    Type::Numeric
                }
            }
            Expression::Column(_, _, t, _) => *t,
            Expression::ArrayColumn(_, _, _, t) => *t,
            Expression::List(xs) => xs.iter().map(|x| x.t()).max().unwrap(),
            Expression::Void => Type::Void,
            Expression::Permutation(..) => Type::Void,
        }
    }

    pub fn dependencies(&self) -> HashSet<(String, String)> {
        self.leaves()
            .into_iter()
            .filter_map(|e| match e {
                Expression::Column(module, name, ..) => Some((module.to_owned(), name.to_owned())),
                _ => None,
            })
            .collect()
    }

    pub fn eval(
        &self,
        i: isize,
        get: &mut dyn FnMut(&str, &str, isize, bool) -> Option<Fr>,
        trace: bool,
        depth: usize,
        wrap: bool,
    ) -> Option<Fr> {
        let r = match self {
            Expression::Funcall { func, args } => match func {
                Builtin::Add => {
                    let args = args
                        .iter()
                        .map(|x| x.eval(i, get, trace, depth + 1, wrap))
                        .collect::<Option<Vec<_>>>()?;
                    Some(args.iter().fold(Fr::zero(), |mut ax, x| {
                        ax.add_assign(x);
                        ax
                    }))
                }
                Builtin::Sub => {
                    let args = args
                        .iter()
                        .map(|x| x.eval(i, get, trace, depth + 1, wrap))
                        .collect::<Option<Vec<_>>>()?;
                    let mut ax = args[0];
                    for x in args[1..].iter() {
                        ax.sub_assign(x)
                    }
                    Some(ax)
                }
                Builtin::Mul => {
                    let args = args
                        .iter()
                        .map(|x| x.eval(i, get, trace, depth + 1, wrap))
                        .collect::<Option<Vec<_>>>()?;
                    Some(args.iter().fold(Fr::one(), |mut ax, x| {
                        ax.mul_assign(x);
                        ax
                    }))
                }
                Builtin::Shift => {
                    if let Expression::Const(ii) = &args[1] {
                        args[0].eval(i + ii.to_isize().unwrap(), get, false, depth + 1, false)
                    } else {
                        unreachable!()
                    }
                }
                Builtin::Neg => args[0].eval(i, get, trace, depth + 1, true).map(|mut x| {
                    x.negate();
                    x
                }),
                Builtin::Inv => args[0]
                    .eval(i, get, trace, depth + 1, true)
                    .and_then(|x| x.inverse()),
                Builtin::Nth => {
                    if let (
                        Expression::ArrayColumn(module, name, range, _),
                        Expression::Const(idx),
                    ) = (&args[0], &args[1])
                    {
                        let idx = idx.to_usize().unwrap();
                        if !range.contains(&idx) {
                            panic!("trying to access `{}.{}` ad index `{}`", module, name, idx);
                        }
                        get(module, &format!("{}_{}", name, idx), i, wrap)
                    } else {
                        unreachable!()
                    }
                }
                Builtin::Begin => unreachable!(),
                Builtin::IfZero => unreachable!(),
                Builtin::IfNotZero => unreachable!(),
                Builtin::InRange => unreachable!(),
                Builtin::ByteDecomposition => unreachable!(),
            },
            Expression::Const(x) => Fr::from_str(&x.to_string()),
            Expression::Column(module, name, ..) => get(module, name, i, wrap),
            _ => unreachable!(),
        };
        if trace && !matches!(self, Expression::Const(_)) {
            eprintln!(
                "{:70} <- {}",
                r.as_ref().map(Pretty::pretty).unwrap_or("nil".to_owned()),
                format!("{}[{}]", self, i),
            );
        }
        r
    }

    pub fn leaves(&self) -> Vec<Expression> {
        fn _flatten(e: &Expression, ax: &mut Vec<Expression>) {
            match e {
                Expression::Funcall { args, .. } => {
                    for a in args {
                        _flatten(a, ax);
                    }
                }
                Expression::Const(_) => ax.push(e.clone()),
                Expression::Column(_, _, _, _) => ax.push(e.clone()),
                Expression::ArrayColumn(_, _, _, _) => {}
                Expression::List(args) => {
                    for a in args {
                        _flatten(a, ax);
                    }
                }
                Expression::Permutation(..) | Expression::Void => (),
            }
        }

        let mut r = vec![];
        _flatten(self, &mut r);
        r
    }
    // pub fn fold<T: Clone>(&self, ax: T, f: &dyn Fn(T, &Expression) -> T) -> T {
    //     match self {
    //         Expression::List(xs) => {
    //             let mut ax = ax.clone();
    //             for x in xs {
    //                 ax = x.fold(ax, f);
    //             }
    //             ax
    //         }
    //         x => f(ax, x.fold(ax, f)),
    //     }
    // }
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
}
impl Display for Expression {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        fn format_list(cs: &[Expression]) -> String {
            cs.iter()
                .map(|c| format!("{}", c))
                .collect::<Vec<_>>()
                .join(" ")
        }

        match self {
            Expression::Const(x) => write!(f, "{}", x),
            Expression::Column(module, name, _t, _k) => {
                write!(f, "{}.{}", module, name)
            }
            Expression::ArrayColumn(_module, name, range, _t) => {
                write!(
                    f,
                    "{}[{}:{}]",
                    name,
                    range.first().unwrap(),
                    range.last().unwrap(),
                )
            }
            Expression::List(cs) => write!(f, "{{{}}}", format_list(cs)),
            Self::Funcall { func, args } => {
                write!(f, "({:?} {})", func, format_list(args))
            }
            Expression::Void => write!(f, "nil"),
            Expression::Permutation(froms, tos) => write!(f, "{:?}<=>{:?}", froms, tos),
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
            Expression::Const(x) => write!(f, "{}", x),
            Expression::Column(module, name, t, _k) => {
                write!(f, "{}/{}:{:?}", module, name, t)
            }
            Expression::ArrayColumn(module, name, range, t) => {
                write!(
                    f,
                    "{}/{}:{:?}[{}:{}]",
                    module,
                    name,
                    t,
                    range.first().unwrap(),
                    range.last().unwrap(),
                )
            }
            Expression::List(cs) => write!(f, "'({})", format_list(cs)),
            Self::Funcall { func, args } => {
                write!(f, "({:?} {})", func, format_list(args))
            }
            Expression::Void => write!(f, "nil"),
            Expression::Permutation(froms, tos) => write!(f, "{:?}<=>{:?}", froms, tos),
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
    InRange,

    IfZero,
    IfNotZero,

    ByteDecomposition,
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
                std::cmp::max(argtype[1], *argtype.get(2).unwrap_or(&Type::Boolean))
            }
            Builtin::Begin => *argtype.iter().max().unwrap(),
            Builtin::Shift | Builtin::Nth => argtype[0],
            Builtin::InRange => Type::Void,
            Builtin::ByteDecomposition => Type::Void,
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
            Builtin::InRange => Arity::Exactly(2),
            Builtin::ByteDecomposition => Arity::Exactly(3),
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
                if matches!(&args[0], Expression::Column(..))
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
            Builtin::InRange => {
                if matches!(args[0], Expression::Column(..))
                    && matches!(args[1], Expression::Const(_))
                {
                    Ok(())
                } else {
                    Err(eyre!(
                        "`{:?}` expects a column and a number but received {:?}",
                        self,
                        args
                    ))
                }
            }
            Builtin::ByteDecomposition => {
                if matches!(args[0], Expression::Column(..))
                    && matches!(args[1], Expression::Const(_))
                    && matches!(args[2], Expression::Const(_))
                {
                    Ok(())
                } else {
                    Err(eyre!(
                        "`{:?}` expects COLUMN ELEM_SIZE CHUNK_COUT but received {:?}",
                        self,
                        args
                    ))
                }
            }
        }
    }
}

#[derive(Default, Debug)]
pub struct ConstraintSet {
    pub columns: ColumnSet<Fr>,
    pub constraints: Vec<Constraint>,
    pub constants: HashMap<String, i64>,
}
impl ConstraintSet {
    fn get(&self, module: &str, name: &str) -> Result<&Column<Fr>> {
        self.columns.get(module, name)
    }

    fn get_mut(&mut self, module: &str, name: &str) -> Result<&mut Column<Fr>> {
        self.columns.get_mut(module, name)
    }

    fn compute_interleaved(&mut self, module: &str, name: &str) -> Result<()> {
        let col = self.get(module, name)?;
        let froms = if let Column::Interleaved { froms: from, .. } = col {
            from.clone()
        } else {
            unreachable!()
        };

        for from in froms.iter() {
            self.compute_column(module, from)?;
        }

        if !froms
            .iter()
            .map(|c| self.get(module, c).unwrap().len().unwrap())
            .collect::<Vec<_>>()
            .windows(2)
            .all(|w| w[0] == w[1])
        {
            return Err(eyre!("interleaving columns of incoherent lengths"));
        }
        let len = self.get(module, &froms[0])?.len().unwrap();

        let mut values = Vec::new();
        for i in 0..len as isize {
            for from in froms.iter() {
                values.push(
                    self.get(module, from)
                        .unwrap()
                        .get(i, false)
                        .unwrap()
                        .clone(),
                );
            }
        }

        self.get_mut(module, name).unwrap().set_values(values);

        Ok(())
    }

    fn compute_sorted(&mut self, module: &str, name: &str) -> Result<()> {
        let col = self.get(module, name)?;
        let froms = if let Column::Sorted { froms, .. } = col {
            froms.clone()
        } else {
            unreachable!()
        };

        for from in froms.iter() {
            self.compute_column(module, from)?;
        }
        let from_cols = froms
            .iter()
            .map(|c| self.get(module, c).unwrap())
            .cloned()
            .collect::<Vec<_>>();

        if !from_cols.windows(2).all(|w| w[0].len() == w[1].len()) {
            return Err(eyre!("sorted columns of incoherent lengths"));
        }
        let len = from_cols[0].len().unwrap();

        let mut sorted_is = (0..len).collect::<Vec<_>>();
        sorted_is.sort_by(|i, j| {
            for t in 0..from_cols.len() {
                let i_t = from_cols[*i].get(t as isize, false).unwrap();
                let j_t = from_cols[*j].get(t as isize, false).unwrap();
                if i_t > j_t {
                    return Ordering::Greater;
                } else if i_t < j_t {
                    return Ordering::Less;
                }
            }
            Ordering::Equal
        });

        for k in 0..froms.len() {
            let mut value = Vec::with_capacity(len);
            value.resize_with(len, || Fr::zero());
            for i in &sorted_is {
                value[*i] = from_cols[k]
                    .get((*i).try_into().unwrap(), false)
                    .unwrap()
                    .clone();
            }
            if self.get_mut(module, name).unwrap().is_computed() {
                warn!(
                    "{}/{} already filled; dropping computing value",
                    module, name
                )
            } else {
                self.get_mut(module, name).unwrap().set_values(value);
            }
        }

        Ok(())
    }

    fn compute_composite(&mut self, module: &str, name: &str) -> Result<()> {
        let col = self.columns.get(module, name).unwrap();
        let (exp, cols_in_expr) = if let Column::Composite { exp, .. } = col {
            (exp.clone(), exp.dependencies())
        } else {
            unreachable!()
        };

        let length = *cols_in_expr
            .iter()
            .map(|(module, name)| {
                self.compute_column(module, name)?;
                Ok(self.get(module, name).unwrap().len().unwrap().to_owned())
            })
            .collect::<Result<Vec<_>>>()?
            .iter()
            .max()
            .unwrap();

        let values = (0..length as isize)
            .map(|i| {
                exp.eval(
                    i,
                    &mut |module, name, i, _| {
                        let col = self.get(module, name).unwrap();
                        if col.is_computed() {
                            col.get(i, false).cloned()
                        } else {
                            self.compute_column(module, name);
                            self.columns
                                .get(module, name)
                                .unwrap()
                                .get(i, false)
                                .cloned()
                        }
                    },
                    false,
                    0,
                    false,
                )
                .unwrap_or_else(Fr::zero)
            })
            .collect::<Vec<_>>();

        self.columns
            .get_mut(module, name)
            .unwrap()
            .set_values(values);

        Ok(())
    }

    fn compute_column(&mut self, module: &str, name: &str) -> Result<()> {
        let col = self.get(module, name).unwrap();
        if col.is_computed() {
            Ok(())
        } else {
            info!("Computing {}/{}", module, name);
            match col {
                Column::Composite { .. } => self.compute_composite(module, name),
                Column::Interleaved { .. } => self.compute_interleaved(module, name),
                Column::Sorted { .. } => self.compute_sorted(module, name),
                _ => unreachable!("{:?}", col),
            }
        }
    }

    pub fn compute(&mut self) -> Result<()> {
        let handles = self
            .columns
            .cols
            .iter()
            .flat_map(|(module, cols)| {
                cols.keys()
                    .map(|colname| (module.to_owned(), colname.to_owned()))
            })
            .collect::<Vec<_>>();

        for (module, colname) in handles.iter() {
            self.compute_column(module, colname)
                .with_context(|| format!("computing {}/{}", module, colname))?;
        }

        Ok(())
    }
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
                        module,
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
                        Expression::List(traversed_args.into_iter().fold(
                            vec![],
                            |mut ax, e| match e {
                                Expression::List(mut es) => {
                                    ax.append(&mut es);
                                    ax
                                }
                                _ => {
                                    ax.push(e);
                                    ax
                                }
                            },
                        )),
                        *traversed_args_t.iter().max().unwrap(),
                    ))),

                    b @ (Builtin::IfZero | Builtin::IfNotZero) => {
                        let conds = {
                            let cond_not_zero = cond.clone();
                            // If the condition is binary, cond_zero = 1 - x...
                            let cond_zero = if matches!(traversed_args_t[0], Type::Boolean) {
                                Expression::Funcall {
                                    func: Builtin::Sub,
                                    args: vec![Expression::Const(One::one()), cond.clone()],
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
                        if let (Expression::ArrayColumn(module, cname, ..), Expression::Const(i)) =
                            (&traversed_args[0], &traversed_args[1])
                        {
                            let x = i.to_usize().unwrap();
                            match &ctx.borrow_mut().resolve_symbol(module, cname)? {
                                array @ (Expression::ArrayColumn(_, _, range, t), _) => {
                                    if range.contains(&x) {
                                        Ok(Some((
                                            Expression::Column(
                                                module.to_owned(),
                                                format!("{}_{}", cname, i),
                                                *t,
                                                Kind::Atomic,
                                            ),
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

                    Builtin::InRange => {
                        warn!("INRANGE constraints not yet implemented");
                        Ok(None)
                    }

                    Builtin::ByteDecomposition => {
                        warn!("BYTEDECOMPOSITION constraints not yet implemented");
                        Ok(None)
                    }

                    b @ (Builtin::Add
                    | Builtin::Sub
                    | Builtin::Mul
                    | Builtin::Neg
                    | Builtin::Inv
                    | Builtin::Shift) => Ok(Some((
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
        Token::Keyword(_) | Token::Type(_) | Token::Range(_) => Ok(None),
        Token::Value(x) => Ok(Some((
            Expression::Const(x.clone()),
            if *x >= Zero::zero() && *x <= One::one() {
                Type::Boolean
            } else {
                Type::Numeric
            },
        ))),
        Token::Symbol(name) => Ok(Some(ctx.borrow_mut().resolve_symbol(module, name)?)),
        Token::List(args) => {
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
                    if let Expression::Column(_, _, _, kind) = x {
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
        | Token::DefConsts(..)
        | Token::Defun(..)
        | Token::DefSort(..)
        | Token::DefPlookup(..) => Ok(None),
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
            expr: Box::new(
                reduce(expr, ctx, module)?
                    .unwrap_or((Expression::Void, Type::Void))
                    .0,
            ), // the parser ensures that the body is never empty
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

        Token::Value(_) | Token::Symbol(_) | Token::List(_) | Token::Range(_) => {
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
