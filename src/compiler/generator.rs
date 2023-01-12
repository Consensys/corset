use anyhow::*;
use cached::Cached;
use colored::Colorize;
use log::*;
use num_bigint::BigInt;
use num_traits::cast::ToPrimitive;
use num_traits::{One, Zero};
use once_cell::sync::OnceCell;
use pairing_ce::bn256::Fr;
use pairing_ce::ff::{Field, PrimeField};
use rayon::prelude::*;
use serde::{Deserialize, Serialize};
use std::cell::RefCell;
use std::cmp::Ordering;
use std::collections::{HashMap, HashSet};
use std::fmt::{Debug, Display, Formatter};
use std::io::Write;
use std::rc::Rc;
use std::sync::atomic::AtomicUsize;

use super::definitions::ComputationTable;
use super::{common::*, CompileSettings, Handle};
use crate::column::{Column, ColumnSet, Computation};
use crate::compiler::definitions::SymbolTable;
use crate::compiler::parser::*;
use crate::pretty::Pretty;

static COUNTER: OnceCell<AtomicUsize> = OnceCell::new();

#[derive(Debug, Serialize, Deserialize, Clone)]
pub enum Constraint {
    Vanishes {
        handle: Handle,
        domain: Option<Vec<isize>>,
        expr: Box<Node>,
    },
    Plookup(Handle, Vec<Node>, Vec<Node>),
    Permutation(Handle, Vec<Handle>, Vec<Handle>),
    InRange(Handle, Node, usize),
}
impl Constraint {
    pub fn name(&self) -> String {
        match self {
            Constraint::Vanishes { handle: name, .. } => name.to_string(),
            Constraint::Plookup(handle, ..) => handle.to_string(),
            Constraint::Permutation(handle, ..) => handle.to_string(),
            Constraint::InRange(handle, ..) => handle.to_string(),
        }
    }

    pub fn add_id_to_handles(&mut self, set_id: &dyn Fn(&mut Handle)) {
        match self {
            Constraint::Vanishes { expr, .. } => expr.add_id_to_handles(set_id),
            Constraint::Plookup(_, xs, ys) => xs
                .iter_mut()
                .chain(ys.iter_mut())
                .for_each(|e| e.add_id_to_handles(set_id)),
            Constraint::Permutation(_, hs1, hs2) => {
                hs1.iter_mut().chain(hs2.iter_mut()).for_each(|h| set_id(h))
            }
            Constraint::InRange(_, _, _) => {}
        }
    }

    pub(crate) fn size(&self) -> usize {
        match self {
            Constraint::Vanishes { expr, .. } => expr.size(),
            Constraint::Plookup(_, _, _) => 1,
            Constraint::Permutation(_, _, _) => 1,
            Constraint::InRange(_, _, _) => 1,
        }
    }
}

pub struct EvalSettings {
    pub trace: bool,
    pub wrap: bool,
}
impl Default for EvalSettings {
    fn default() -> Self {
        EvalSettings {
            trace: false,
            wrap: true,
        }
    }
}
impl EvalSettings {
    pub fn new() -> Self {
        Default::default()
    }
    pub fn set_trace(self, trace: bool) -> Self {
        EvalSettings { trace, ..self }
    }
}

#[derive(Clone, Serialize, Deserialize)]
pub enum Expression {
    Funcall { func: Builtin, args: Vec<Node> },
    Const(BigInt, Option<Fr>),
    Column(Handle, Kind<Box<Node>>),
    ArrayColumn(Handle, Vec<usize>),
    List(Vec<Node>),
    Void,
}

#[derive(Clone, Serialize, Deserialize)]
pub struct Node {
    pub _e: Expression,
    pub _t: Option<Type>,
}
impl From<Expression> for Node {
    fn from(e: Expression) -> Self {
        Node::from_expr(e)
    }
}
impl Node {
    pub fn from_expr(e: Expression) -> Node {
        Node { _e: e, _t: None }
    }
    pub fn one() -> Node {
        Self::from_expr(Expression::Const(One::one(), Some(Fr::one())))
    }
    pub fn zero() -> Node {
        Self::from_expr(Expression::Const(Zero::zero(), Some(Fr::zero())))
    }
    pub fn e(&self) -> &Expression {
        &self._e
    }
    pub fn e_mut(&mut self) -> &mut Expression {
        &mut self._e
    }
    pub fn t(&self) -> Type {
        self._t.unwrap_or_else(|| match self.e() {
            Expression::Funcall { func, args } => {
                func.typing(&args.iter().map(|a| a.t()).collect::<Vec<_>>())
            }
            Expression::Const(ref x, _) => {
                if Zero::is_zero(x) || One::is_one(x) {
                    Type::Scalar(Magma::Boolean)
                } else {
                    Type::Scalar(Magma::Integer)
                }
            }
            Expression::Column(..) => unreachable!(),
            Expression::ArrayColumn(..) => unreachable!(),
            Expression::List(xs) => Type::List(
                xs.iter()
                    .map(Node::t)
                    .fold(Type::INFIMUM, |a, b| a.max(&b))
                    .magma(),
            ),
            Expression::Void => Type::Void,
        })
    }

    pub fn size(&self) -> usize {
        match self.e() {
            Expression::Funcall { args, .. } => 1 + args.iter().map(Node::size).sum::<usize>(),
            Expression::Const(..) => 0,
            Expression::Column(..) => 1,
            Expression::ArrayColumn(..) => 0,
            Expression::List(xs) => xs.iter().map(Node::size).sum::<usize>(),
            Expression::Void => 0,
        }
    }

    /// Compute the maximum future-shifting in the node
    pub fn future_span(&self) -> usize {
        fn _future_span(e: &Node, ax: &mut isize) {
            match e.e() {
                Expression::Funcall { func, args } => {
                    if let Builtin::Shift = func {
                        let arg_big = args[1].pure_eval().expect(args[1].to_string().as_str());
                        let arg = arg_big.to_isize().expect(arg_big.to_string().as_str());
                        *ax = (*ax).max(*ax + arg);
                    }
                    args.iter().for_each(|e| _future_span(e, ax))
                }
                Expression::List(es) => es.iter().for_each(|e| _future_span(e, ax)),
                _ => {}
            }
        }

        let mut span = 0;
        _future_span(self, &mut span);
        span.max(0) as usize
    }

    pub fn add_id_to_handles(&mut self, set_id: &dyn Fn(&mut Handle)) {
        match self.e_mut() {
            Expression::Funcall { args, .. } => {
                args.iter_mut().for_each(|e| e.add_id_to_handles(set_id))
            }

            Expression::Column(handle, ..) => set_id(handle),
            Expression::List(xs) => xs.iter_mut().for_each(|x| x.add_id_to_handles(set_id)),

            Expression::ArrayColumn(..) | Expression::Const(_, _) | Expression::Void => {}
        }
    }

    pub fn dependencies(&self) -> HashSet<Handle> {
        self.leaves()
            .into_iter()
            .filter_map(|e| match e.e() {
                Expression::Column(handle, ..) => Some(handle.clone()),
                _ => None,
            })
            .collect()
    }

    pub fn module(&self) -> Option<String> {
        let modules = self
            .dependencies()
            .into_iter()
            .map(|h| h.module)
            .collect::<HashSet<_>>();
        if modules.len() != 1 {
            None
        } else {
            modules.into_iter().next()
        }
    }

    /// Evaluate a compile-time known value
    pub fn pure_eval(&self) -> Result<BigInt> {
        match self.e() {
            Expression::Funcall { func, args } => match func {
                Builtin::Add => {
                    let args = args
                        .iter()
                        .map(|x| x.pure_eval())
                        .collect::<Result<Vec<_>>>()?;
                    Ok(args.iter().fold(BigInt::zero(), |ax, x| ax + x))
                }
                Builtin::Sub => {
                    let args = args
                        .iter()
                        .map(|x| x.pure_eval())
                        .collect::<Result<Vec<_>>>()?;
                    let mut ax = args[0].to_owned();
                    for x in args[1..].iter() {
                        ax -= x
                    }
                    Ok(ax)
                }
                Builtin::Mul => {
                    let args = args
                        .iter()
                        .map(|x| x.pure_eval())
                        .collect::<Result<Vec<_>>>()?;
                    Ok(args.iter().fold(BigInt::one(), |ax, x| ax * x))
                }
                Builtin::Neg => Ok(-args[0].pure_eval()?),
                x => Err(anyhow!(
                    "{} is not known at compile-time",
                    x.to_string().red()
                )),
            },
            Expression::Const(v, _) => Ok(v.to_owned()),
            _ => Err(anyhow!(
                "{} is not known at compile-time",
                self.to_string().red()
            )),
        }
    }

    pub fn eval(
        &self,
        i: isize,
        get: &mut dyn FnMut(&Handle, isize, bool) -> Option<Fr>,
        cache: &mut Option<cached::SizedCache<Fr, Fr>>,
        settings: &EvalSettings,
    ) -> Option<Fr> {
        let r = match self.e() {
            Expression::Funcall { func, args } => match func {
                Builtin::Add => {
                    let mut ax = Fr::zero();
                    for arg in args.iter() {
                        ax.add_assign(&arg.eval(i, get, cache, settings)?)
                    }
                    Some(ax)
                }
                Builtin::Sub => {
                    let mut ax = args[0].eval(i, get, cache, settings)?;
                    for arg in args.iter().skip(1) {
                        ax.sub_assign(&arg.eval(i, get, cache, settings)?)
                    }
                    Some(ax)
                }
                Builtin::Mul => {
                    let mut ax = Fr::one();
                    for arg in args.iter() {
                        ax.mul_assign(&arg.eval(i, get, cache, settings)?)
                    }
                    Some(ax)
                }
                Builtin::Exp => {
                    let mut ax = Fr::one();
                    let mantissa = args[0].eval(i, get, cache, settings)?;
                    let exp = args[1].pure_eval().unwrap().to_usize().unwrap();
                    for _ in 0..exp {
                        ax.mul_assign(&mantissa);
                    }
                    Some(ax)
                }
                Builtin::Shift => {
                    let shift = args[1].pure_eval().unwrap().to_isize().unwrap();
                    args[0].eval(
                        i + shift,
                        get,
                        cache,
                        &EvalSettings {
                            wrap: false,
                            ..*settings
                        },
                    )
                }
                Builtin::Eq => {
                    let (x, y) = (
                        args[0].eval(i, get, cache, settings)?,
                        args[1].eval(i, get, cache, settings)?,
                    );
                    if args[0].t().is_bool() && args[1].t().is_bool() {
                        Some(if x.eq(&y) { Fr::zero() } else { Fr::one() })
                    } else {
                        let mut ax = x;
                        ax.sub_assign(&y);
                        Some(ax)
                    }
                }
                Builtin::Neg => args[0].eval(i, get, cache, settings).map(|mut x| {
                    x.negate();
                    x
                }),
                Builtin::Inv => {
                    let x = args[0].eval(i, get, cache, settings);
                    if let Some(ref mut rcache) = cache {
                        x.map(|x| {
                            rcache
                                .cache_get_or_set_with(x, || x.inverse().unwrap_or_else(Fr::zero))
                                .to_owned()
                        })
                    } else {
                        x.and_then(|x| x.inverse()).or_else(|| Some(Fr::zero()))
                    }
                }
                Builtin::Not => {
                    let mut r = Fr::one();
                    if let Some(x) = args[0].eval(i, get, cache, settings) {
                        r.sub_assign(&x);
                        Some(r)
                    } else {
                        None
                    }
                }
                Builtin::Nth => {
                    if let (Expression::ArrayColumn(h, range), Expression::Const(idx, _)) =
                        (&args[0].e(), &args[1].e())
                    {
                        let idx = idx.to_usize().unwrap();
                        if !range.contains(&idx) {
                            panic!("trying to access `{}` ad index `{}`", h, idx);
                        }
                        get(&h.ith(idx), i, settings.wrap)
                    } else {
                        unreachable!()
                    }
                }
                Builtin::Begin => unreachable!(),
                Builtin::IfZero => {
                    if args[0].eval(i, get, cache, settings)?.is_zero() {
                        args[1].eval(i, get, cache, settings)
                    } else {
                        args.get(2)
                            .map(|x| x.eval(i, get, cache, settings))
                            .unwrap_or_else(|| Some(Fr::zero()))
                    }
                }
                Builtin::IfNotZero => {
                    if !args[0].eval(i, get, cache, settings)?.is_zero() {
                        args[1].eval(i, get, cache, settings)
                    } else {
                        args.get(2)
                            .map(|x| x.eval(i, get, cache, settings))
                            .unwrap_or_else(|| Some(Fr::zero()))
                    }
                }
                Builtin::ByteDecomposition => unreachable!(),
            },
            Expression::Const(v, x) => {
                Some(x.unwrap_or_else(|| panic!("{} is not an Fr element.", v)))
            }
            Expression::Column(handle, ..) => get(handle, i, settings.wrap),
            Expression::List(xs) => xs
                .iter()
                .filter_map(|x| x.eval(i, get, cache, settings))
                .find(|x| !x.is_zero())
                .or_else(|| Some(Fr::zero())),
            _ => unreachable!("{:?}", self),
        };
        if settings.trace && !matches!(self.e(), Expression::Const(..)) {
            eprintln!(
                "{:70} <- {}[{}]",
                r.as_ref()
                    .map(Pretty::pretty)
                    .unwrap_or_else(|| "nil".to_owned()),
                self,
                i
            );
        }
        r
    }

    pub fn leaves(&self) -> Vec<Node> {
        fn _flatten(e: &Node, ax: &mut Vec<Node>) {
            match e.e() {
                Expression::Funcall { args, .. } => {
                    for a in args {
                        _flatten(a, ax);
                    }
                }
                Expression::Const(..) => ax.push(e.clone()),
                Expression::Column(..) => ax.push(e.clone()),
                Expression::ArrayColumn(..) => {}
                Expression::List(args) => {
                    for a in args {
                        _flatten(a, ax);
                    }
                }
                Expression::Void => (),
            }
        }

        let mut r = vec![];
        _flatten(self, &mut r);
        r
    }

    pub fn flat_map<T>(&self, f: &dyn Fn(&Node) -> T) -> Vec<T> {
        let mut ax = vec![];
        match self.e() {
            Expression::List(xs) => {
                for x in xs {
                    ax.push(f(x));
                }
            }
            _ => ax.push(f(self)),
        }
        ax
    }
}
impl Display for Node {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        fn format_list(cs: &[Node]) -> String {
            cs.iter()
                .map(|c| format!("{}", c))
                .collect::<Vec<_>>()
                .join(" ")
        }

        match self.e() {
            Expression::Const(x, _) => write!(f, "{}", x),
            Expression::Column(handle, ..) => {
                write!(f, "{}", handle)
            }
            Expression::ArrayColumn(handle, range, ..) => {
                write!(
                    f,
                    "{}[{}:{}]",
                    handle,
                    range.first().unwrap(),
                    range.last().unwrap(),
                )
            }
            Expression::List(cs) => write!(f, "{{{}}}", format_list(cs)),
            Expression::Funcall { func, args } => {
                write!(f, "({} {})", func, format_list(args))
            }
            Expression::Void => write!(f, "nil"),
            // Expression::Permutation(froms, tos) => write!(f, "{:?}<=>{:?}", froms, tos),
        }
    }
}
impl Debug for Node {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        fn format_list(cs: &[Node]) -> String {
            cs.iter()
                .map(|c| format!("{:?}", c))
                .collect::<Vec<_>>()
                .join(" ")
        }

        match self.e() {
            Expression::Const(x, _) => write!(f, "{}", x),
            Expression::Column(handle, ..) => {
                write!(f, "{:?}:{:?}", handle, self.t())
            }
            Expression::ArrayColumn(handle, range, ..) => {
                write!(
                    f,
                    "{}:{:?}[{}:{}]",
                    handle,
                    self.t(),
                    range.first().unwrap(),
                    range.last().unwrap(),
                )
            }
            Expression::List(cs) => write!(f, "'({})", format_list(cs)),
            Expression::Funcall { func, args } => {
                write!(f, "({:?} {})", func, format_list(args))
            }
            Expression::Void => write!(f, "nil"),
            // Expression::Permutation(froms, tos) => write!(f, "{:?}<=>{:?}", froms, tos),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Serialize, Deserialize)]
pub enum Builtin {
    Add,
    Sub,
    Mul,
    Exp,
    Shift,
    Neg,
    Inv,
    Not,

    Nth,
    Eq,
    Begin,

    IfZero,
    IfNotZero,

    ByteDecomposition,
}
impl Builtin {
    pub fn call(self, args: &[Node]) -> Node {
        Node::from_expr(self.raw_call(args))
    }

    pub fn raw_call(self, args: &[Node]) -> Expression {
        Expression::Funcall {
            func: self,
            args: args.to_owned(),
        }
    }

    fn typing(&self, argtype: &[Type]) -> Type {
        match self {
            Builtin::Add | Builtin::Sub | Builtin::Neg | Builtin::Inv => {
                // Boolean is a corner case, as it is not stable under these operations
                match argtype.iter().fold(Type::INFIMUM, |a, b| a.max(b)) {
                    Type::Scalar(Magma::Boolean) => Type::Scalar(Magma::Integer),
                    Type::Column(Magma::Boolean) => Type::Column(Magma::Integer),
                    x => x,
                }
            }
            Builtin::Exp => argtype[0],
            Builtin::Eq => argtype.iter().fold(Type::INFIMUM, |a, b| a.max(b)),
            Builtin::Not => argtype
                .iter()
                .fold(Type::INFIMUM, |a, b| a.max(b))
                .same_scale(Magma::Boolean),
            Builtin::Mul => argtype.iter().fold(Type::INFIMUM, |a, b| a.max(b)),
            Builtin::IfZero | Builtin::IfNotZero => {
                argtype[1].max(argtype.get(2).unwrap_or(&Type::INFIMUM))
            }
            Builtin::Begin => {
                Type::List(argtype.iter().fold(Type::INFIMUM, |a, b| a.max(b)).magma())
            }
            Builtin::Shift | Builtin::Nth => argtype[0],
            Builtin::ByteDecomposition => Type::Void,
        }
    }
}
impl std::fmt::Display for Builtin {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Builtin::Eq => "eq",
                Builtin::Add => "+",
                Builtin::Sub => "-",
                Builtin::Mul => "*",
                Builtin::Exp => "^",
                Builtin::Shift => "shift",
                Builtin::Neg => "-",
                Builtin::Inv => "INV",
                Builtin::Not => "not",
                Builtin::Nth => "nth",
                Builtin::Begin => "begin",
                Builtin::IfZero => "if-zero",
                Builtin::IfNotZero => "if-not-zero",
                Builtin::ByteDecomposition => "make-byte-decomposition",
            }
        )
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    pub handle: Handle,
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
    pub pure: bool,
    pub args: Vec<String>,
    pub body: AstNode,
}
impl FuncVerifier<Node> for Defined {
    fn arity(&self) -> Arity {
        Arity::Exactly(self.args.len())
    }

    fn validate_types(&self, _args: &[Node]) -> Result<()> {
        Ok(())
    }
}

impl FuncVerifier<Node> for Builtin {
    fn arity(&self) -> Arity {
        match self {
            Builtin::Add => Arity::AtLeast(2),
            Builtin::Sub => Arity::AtLeast(2),
            Builtin::Mul => Arity::AtLeast(2),
            Builtin::Exp => Arity::Exactly(2),
            Builtin::Eq => Arity::Exactly(2),
            Builtin::Neg => Arity::Monadic,
            Builtin::Inv => Arity::Monadic,
            Builtin::Not => Arity::Monadic,
            Builtin::Shift => Arity::Dyadic,
            Builtin::Begin => Arity::AtLeast(1),
            Builtin::IfZero => Arity::Between(2, 3),
            Builtin::IfNotZero => Arity::Between(2, 3),
            Builtin::Nth => Arity::Dyadic,
            Builtin::ByteDecomposition => Arity::Exactly(3),
        }
    }
    fn validate_types(&self, args: &[Node]) -> Result<()> {
        match self {
            f @ (Builtin::Add | Builtin::Sub | Builtin::Mul) => args.iter().try_for_each(|a| {
                if a.t().is_value() {
                    Ok(())
                } else {
                    Err(anyhow!(
                        "`{:?}` received unexepcted argument {} of type {:?}",
                        f,
                        a.pretty(),
                        a.t(),
                    ))
                }
            }),
            Builtin::Exp => {
                if args[0].t().is_value() && args[1].t().is_scalar() {
                    Ok(())
                } else {
                    bail!(
                        "`{:?}` expects a scalar exponent; found `{}` of type {:?}",
                        &self,
                        args[1],
                        args[1].t()
                    )
                }
            }
            Builtin::Eq => {
                if args.iter().all(|a| a.t().is_value()) {
                    Ok(())
                } else {
                    bail!("`{:?}` expects value arguments", Builtin::Eq)
                }
            }
            Builtin::Not => args[0].t().is_bool().then_some(()).ok_or_else(|| {
                anyhow!(
                    "`{:?}` expects a boolean; found `{}` of type {:?}",
                    &self,
                    args[0],
                    args[0].t()
                )
            }),
            Builtin::Neg | Builtin::Inv => {
                if args.iter().all(|a| a.t().is_value()) {
                    Ok(())
                } else {
                    Err(anyhow!(
                        "`{:?}` expects value arguments but received a list",
                        self
                    ))
                }
            }
            Builtin::Shift => {
                if args[0].t().is_column() && args[1].t().is_scalar() {
                    Ok(())
                } else {
                    Err(anyhow!(
                        "`{:?}` expects a COLUMN and a VALUE but received {:?}",
                        self,
                        args.iter().map(Node::t).collect::<Vec<_>>()
                    ))
                }
            }
            Builtin::Nth => {
                if matches!(args[0].e(), Expression::ArrayColumn(..))
                    && matches!(&args[1].e(), Expression::Const(x, _) if x.sign() != num_bigint::Sign::Minus)
                {
                    Ok(())
                } else {
                    Err(anyhow!(
                        "`{:?}` expects [SYMBOL CONST] but received {:?}",
                        self,
                        args
                    ))
                }
            }
            Builtin::IfZero | Builtin::IfNotZero => {
                if !matches!(args[0].e(), Expression::List(_)) {
                    Ok(())
                } else {
                    Err(anyhow!(
                        "`{:?}` expects an expression as its condition",
                        self
                    ))
                }
            }
            Builtin::Begin => Ok(()),
            Builtin::ByteDecomposition => {
                if matches!(args[0].e(), Expression::Column(..))
                    && matches!(args[1].e(), Expression::Const(..))
                    && matches!(args[2].e(), Expression::Const(..))
                {
                    Ok(())
                } else {
                    Err(anyhow!(
                        "`{:?}` expects COLUMN ELEM_SIZE ELEM_COUNT but received {:?}",
                        self,
                        args
                    ))
                }
            }
        }
    }
}

#[derive(Default, Debug, Serialize, Deserialize, Clone)]
pub struct ConstraintSet {
    pub modules: ColumnSet<Fr>,
    pub constraints: Vec<Constraint>,
    pub constants: HashMap<Handle, BigInt>,
    pub computations: ComputationTable,

    _spilling: HashMap<String, isize>, // module -> past-spilling
}
impl ConstraintSet {
    pub fn new(
        columns: ColumnSet<Fr>,
        constraints: Vec<Constraint>,
        constants: HashMap<Handle, BigInt>,
        computations: ComputationTable,
    ) -> Self {
        let mut r = ConstraintSet {
            constraints,
            modules: columns,
            constants,
            computations,

            _spilling: Default::default(),
        };
        r.update_ids();
        r
    }
    // pub fn update_futures_spilling(&self) {
    //     for self.computations.
    // }
    pub fn update_ids(&mut self) {
        let set_id = |h: &mut Handle| h.set_id(self.modules.id_of(h));
        self.constraints
            .iter_mut()
            .for_each(|x| x.add_id_to_handles(&set_id));
        self.computations.update_ids(&set_id)
    }

    fn get(&self, handle: &Handle) -> Result<&Column<Fr>> {
        self.modules.get(handle)
    }

    fn get_mut(&mut self, handle: &Handle) -> Result<&mut Column<Fr>> {
        self.modules.get_mut(handle)
    }

    fn compute_interleaved(&mut self, froms: &[Handle]) -> Result<Vec<Fr>> {
        for from in froms.iter() {
            self.compute_column(from)?;
        }

        if !froms
            .iter()
            .map(|h| self.get(h).unwrap().len().unwrap())
            .collect::<Vec<_>>()
            .windows(2)
            .all(|w| w[0] == w[1])
        {
            return Err(anyhow!("interleaving columns of incoherent lengths"));
        }

        let len = self.get(&froms[0])?.len().unwrap();
        let count = froms.len();
        let values = (0..(len * count))
            .into_par_iter()
            .map(|k| {
                let i = k / count;
                let j = k % count;
                *self.get(&froms[j]).unwrap().get(i as isize, false).unwrap()
            })
            .collect::<Vec<_>>();

        Ok(values)
    }

    fn compute_sorted(&mut self, froms: &[Handle], tos: &[Handle]) -> Result<()> {
        for from in froms.iter() {
            self.compute_column(from)?;
        }

        let from_cols = froms
            .iter()
            .map(|c| self.get(c).unwrap())
            .collect::<Vec<_>>();

        if !from_cols.windows(2).all(|w| w[0].len() == w[1].len()) {
            return Err(anyhow!("sorted columns of incoherent lengths"));
        }
        let len = from_cols[0].len().unwrap();

        let mut sorted_is = (0..len).collect::<Vec<_>>();
        sorted_is.sort_by(|i, j| {
            for from in from_cols.iter() {
                let x_i = from.get(*i as isize, false).unwrap();
                let x_j = from.get(*j as isize, false).unwrap();
                if let x @ (Ordering::Greater | Ordering::Less) = x_i.cmp(x_j) {
                    return x;
                }
            }
            Ordering::Equal
        });

        for (k, from) in froms.iter().enumerate() {
            let value = sorted_is
                .iter()
                .map(|i| {
                    *self
                        .get(from)
                        .unwrap()
                        .get((*i).try_into().unwrap(), false)
                        .unwrap()
                })
                .collect();
            self.get_mut(&tos[k]).unwrap().set_value(value);
        }

        Ok(())
    }

    pub fn compute_composite(&mut self, exp: &Node) -> Result<Vec<Fr>> {
        let cols_in_expr = exp.dependencies();
        for c in &cols_in_expr {
            self.compute_column(c)?
        }
        let length = *cols_in_expr
            .iter()
            .map(|handle| Ok(self.get(handle).unwrap().len().unwrap().to_owned()))
            .collect::<Result<Vec<_>>>()?
            .iter()
            .max()
            .unwrap();

        let values = (0..length as isize)
            .into_par_iter()
            .map(|i| {
                exp.eval(
                    i,
                    &mut |handle, i, _| {
                        // All the columns are guaranteed to have been computed
                        // at the beginning of the function
                        self.modules._cols[handle.id.unwrap()]
                            .get(i, false)
                            .cloned()
                    },
                    &mut None,
                    &EvalSettings {
                        trace: false,
                        wrap: false,
                    },
                )
                .unwrap_or_else(Fr::zero)
            })
            .collect::<Vec<_>>();

        Ok(values)
    }

    pub fn compute_composite_static(&self, exp: &Node) -> Result<Vec<Fr>> {
        let cols_in_expr = exp.dependencies();
        for c in &cols_in_expr {
            if !self.get(c)?.is_computed() {
                return Err(anyhow!("column {} not yet computed", c.to_string().red()));
            }
        }

        let length = *cols_in_expr
            .iter()
            .map(|handle| {
                Ok(self
                    .get(handle)
                    .with_context(|| anyhow!("while reading {}", handle.to_string().red().bold()))?
                    .len()
                    .ok_or_else(|| anyhow!("{} has no len", handle.to_string().red().bold()))?
                    .to_owned())
            })
            .collect::<Result<Vec<_>>>()?
            .iter()
            .max()
            .unwrap();

        let values = (0..length as isize)
            .into_par_iter()
            .map(|i| {
                exp.eval(
                    i,
                    &mut |handle, i, _| {
                        // All the columns are guaranteed to have been computed
                        // at the begiinning of the function
                        self.modules._cols[handle.id.unwrap()]
                            .get(i, false)
                            .cloned()
                    },
                    &mut None,
                    &EvalSettings {
                        trace: false,
                        wrap: false,
                    },
                )
                .unwrap_or_else(Fr::zero)
            })
            .collect::<Vec<_>>();

        Ok(values)
    }

    fn compute_column(&mut self, target: &Handle) -> Result<()> {
        if self.get(target).unwrap().is_computed() {
            Ok(())
        } else {
            self.compute(
                self.computations
                    .dep(target)
                    .ok_or_else(|| anyhow!("No computations found for `{}`", target))?,
            )
        }
    }

    fn compute(&mut self, i: usize) -> Result<()> {
        let comp = self.computations.get(i).unwrap().clone();
        debug!("Computing `{}`", comp.target());

        match &comp {
            Computation::Composite { target, exp } => {
                if !self.modules.get(target)?.is_computed() {
                    let r = self.compute_composite(exp)?;
                    self.modules.get_mut(target).unwrap().set_value(r);
                }
                Ok(())
            }
            Computation::Interleaved { target, froms } => {
                if !self.modules.get(target)?.is_computed() {
                    let r = self.compute_interleaved(froms)?;
                    self.get_mut(target)?.set_value(r);
                }
                Ok(())
            }
            Computation::Sorted { froms, tos } => self.compute_sorted(froms, tos),
        }
    }

    pub fn compute_all(&mut self) -> Result<()> {
        for i in 0..self.computations.iter().count() {
            if let Err(e) = self.compute(i) {
                warn!("{:?}", e);
            }
        }

        Ok(())
    }

    pub fn padding_for(&self, h: &Handle) -> Option<&Vec<Fr>> {
        self.modules.by_handle(h)?.padding.as_ref()
    }

    pub fn pad_columns(&mut self) {
        for h in self.modules.handles().iter() {
            self.pad_column(h);
        }
    }
    // The padding value is 0 for atomic columns.
    // However, it has to be computed for computed columns.
    fn pad_column(&mut self, h: &Handle) {
        if self.get(h).unwrap().padding.is_some() {
            return;
        }
        let spilling = self
            ._spilling
            .entry(h.module.to_owned())
            .or_insert_with(|| {
                self.computations
                    .iter()
                    .filter_map(|c| match c {
                        Computation::Composite { target, exp } => {
                            if target.module == h.module {
                                Some(exp.future_span() as isize)
                            } else {
                                None
                            }
                        }
                        _ => None,
                    })
                    .max()
                    .unwrap_or(0)
            });

        let r = (0..=*spilling)
            .map(|k| {
                match &self.get(h).unwrap().kind {
                    Kind::Atomic | Kind::Interleaved(_) | Kind::Phantom => {
                        if *h == Handle::new("binary", "NOT") {
                            Fr::from_str("255").unwrap()
                        } else {
                            Fr::zero()
                        }
                    }
                    Kind::Composite(_) => {
                        let comp = self.computations.computation_for(h).cloned();
                        if let Some(comp) = comp {
                            match comp {
                                Computation::Composite { exp, .. } => {
                                    for h in exp.dependencies() {
                                        self.pad_column(&h);
                                    }
                                    exp.eval(
                                        k,
                                        &mut |target, j, wrap| {
                                            // if i == 0 {
                                            //     let r = self.padding_value_for(target).unwrap();
                                            //     Some(r)
                                            // } else {
                                            self.modules
                                                .get(target)
                                                .ok()
                                                .and_then(|c| c.get(j, wrap))
                                                .cloned()
                                            // }
                                        },
                                        &mut None,
                                        &EvalSettings {
                                            trace: false,
                                            wrap: false,
                                        },
                                    )
                                    .unwrap()
                                }
                                _ => unreachable!(),
                            }
                        } else {
                            unreachable!()
                        }
                    }
                }
            })
            .collect::<Vec<_>>();
        self.get_mut(h).unwrap().padding = Some(r);
    }

    pub fn length_multiplier(&self, h: &Handle) -> usize {
        self.computations
            .computation_for(h)
            .map(|comp| match comp {
                Computation::Composite { exp, .. } => {
                    self.length_multiplier(exp.dependencies().iter().next().unwrap())
                }
                Computation::Interleaved { froms, .. } => {
                    self.length_multiplier(&froms[0]) * froms.len()
                }
                Computation::Sorted { froms, .. } => self.length_multiplier(&froms[0]),
            })
            .unwrap_or(1)
    }

    // pub fn pad_trace(&mut self, s: PaddingStrategy) -> Result<()> {
    //     match s {
    //         PaddingStrategy::OneLine => {
    //             self.modules.handles().iter().for_each(|h| {
    //                 let padding = self.padding_for(h).unwrap();
    //                 let x = self.modules.by_handle_mut(h).unwrap();
    //                 if let Some(xs) = x.value_mut() {
    //                     x.set_value(padding.into_iter().chain(xs.iter()).cloned().collect());
    //                 } else {
    //                     x.set_value(padding.to_vec());
    //                 }
    //             });
    //             Ok(())
    //         }
    //         PaddingStrategy::Full => todo!(),
    //     }
    // }

    pub fn write(&mut self, out: &mut impl Write) -> Result<()> {
        // TODO encode the padding strategy behavior
        // serde_json::to_writer(out, self).with_context(|| "while serializing to JSON")

        out.write_all("{\"columns\":{\n".as_bytes())?;

        for (i, (module, columns)) in self.modules.cols.iter().enumerate() {
            info!("Exporting {}", &module);
            if i > 0 {
                out.write_all(b",")?;
            }

            let empty_vec = Vec::new();
            let mut current_col = columns.iter().peekable();
            while let Some((name, &i)) = current_col.next() {
                trace!("Writing {}/{}", module, name);
                let column = &self.modules._cols[i];
                let handle = Handle::new(&module, &name);
                let padding = self
                    .padding_for(&handle)
                    .ok_or_else(|| anyhow!("column {handle} has no padding value"))?;
                let value = column.value().unwrap_or(&empty_vec);
                out.write_all(format!("\"{}\":{{\n", handle).as_bytes())?;
                out.write_all("\"values\":[".as_bytes())?;

                out.write_all(
                    padding
                        .iter()
                        .chain(value.iter())
                        .map(|x| {
                            format!(
                                "\"0x0{}\"",
                                x.into_repr().to_string()[2..].trim_start_matches('0')
                            )
                        })
                        .collect::<Vec<_>>()
                        .join(",")
                        .as_bytes(),
                )?;

                out.write_all(b"],\n")?;
                out.write_all(
                    format!(
                        "\"padding_strategy\": {{\"action\": \"prepend\", \"value\": \"{}\"}}",
                        padding[0].pretty()
                    )
                    .as_bytes(),
                )?;
                out.write_all(b"\n}\n")?;
                if current_col.peek().is_some() {
                    out.write_all(b",")?;
                }
            }
        }
        out.write_all("}}".as_bytes())?;

        Ok(())
    }
}

// Compared to a function, a form do not evaluate all of its arguments by default
fn apply_form(
    f: Form,
    args: &[AstNode],
    root_ctx: Rc<RefCell<SymbolTable>>,
    ctx: &mut Rc<RefCell<SymbolTable>>,
    settings: &CompileSettings,
) -> Result<Option<Node>> {
    let args = f
        .validate_args(args.to_vec())
        .with_context(|| anyhow!("evaluating call to {:?}", f))?;

    match f {
        Form::For => {
            if let (Token::Symbol(i_name), Token::Range(is), body) =
                (&args[0].class, &args[1].class, &args[2])
            {
                let mut l = vec![];
                let mut t = Type::INFIMUM;
                for i in is {
                    let mut for_ctx = SymbolTable::derived(
                        ctx.clone(),
                        &format!(
                            "for-{}-{}",
                            COUNTER
                                .get_or_init(|| AtomicUsize::new(0))
                                .fetch_add(1, std::sync::atomic::Ordering::Relaxed),
                            i
                        ),
                        &ctx.borrow().pretty_name.clone(),
                        false,
                    );
                    for_ctx.borrow_mut().insert_symbol(
                        i_name,
                        Expression::Const(BigInt::from(*i), Fr::from_str(&i.to_string())).into(),
                    )?;

                    let r =
                        reduce(&body.clone(), root_ctx.clone(), &mut for_ctx, settings)?.unwrap();
                    t = t.max(&r.t());
                    l.push(r);
                }

                Ok(Some(Node {
                    _e: Expression::List(l),
                    _t: Some(t),
                }))
            } else {
                unreachable!()
            }
        }
        Form::Debug => {
            if !settings.debug {
                Ok(None)
            } else {
                let reduced = args
                    .iter()
                    .map(|e| reduce(e, root_ctx.clone(), ctx, settings))
                    .collect::<Result<Vec<_>>>()?;
                match reduced.len() {
                    0 => Ok(None),
                    1 => Ok(reduced[0].to_owned()),
                    _ => Ok(Some(
                        Builtin::Begin.call(
                            &reduced
                                .into_iter()
                                .map(|e| e.unwrap_or_else(|| Expression::Void.into()))
                                .collect::<Vec<_>>(),
                        ),
                    )),
                }
            }
        }
    }
}

fn apply(
    f: &Function,
    args: &[AstNode],
    root_ctx: Rc<RefCell<SymbolTable>>,
    ctx: &mut Rc<RefCell<SymbolTable>>,
    settings: &CompileSettings,
) -> Result<Option<Node>> {
    if let FunctionClass::SpecialForm(sf) = f.class {
        apply_form(sf, args, root_ctx, ctx, settings)
    } else {
        let mut traversed_args = vec![];
        let mut traversed_args_t = vec![];
        for arg in args.iter() {
            let traversed = reduce(arg, root_ctx.clone(), ctx, settings)?;
            if let Some(traversed) = traversed {
                traversed_args_t.push(traversed.t());
                traversed_args.push(traversed);
            }
        }

        match &f.class {
            FunctionClass::Builtin(b) => {
                let traversed_args = b.validate_args(traversed_args).with_context(|| {
                    anyhow!("validating arguments to {}", f.handle.to_string().blue())
                })?;
                match b {
                    Builtin::Begin => Ok(Some(Node {
                        _e: Expression::List(traversed_args.into_iter().fold(
                            vec![],
                            |mut ax, mut e| match e.e_mut() {
                                Expression::List(ref mut es) => {
                                    ax.append(es);
                                    ax
                                }
                                _ => {
                                    ax.push(e);
                                    ax
                                }
                            },
                        )),
                        _t: Some(traversed_args_t.iter().fold(Type::INFIMUM, |a, b| a.max(b))),
                    })),

                    b @ (Builtin::IfZero | Builtin::IfNotZero) => Ok(Some(b.call(&traversed_args))),

                    Builtin::Nth => {
                        if let (Expression::ArrayColumn(handle, ..), Expression::Const(i, _)) =
                            (&traversed_args[0].e(), &traversed_args[1].e())
                        {
                            let x = i.to_usize().unwrap();
                            let column = ctx.borrow_mut().resolve_symbol(&handle.name)?;
                            match column.e() {
                                Expression::ArrayColumn(handle, range) => {
                                    if range.contains(&x) {
                                        Ok(Some(Node {
                                            _e: Expression::Column(
                                                Handle::new(
                                                    &handle.module,
                                                    format!("{}_{}", handle.name, i),
                                                ),
                                                Kind::Atomic,
                                            ),
                                            _t: Some(column.t().as_scalar()),
                                        }))
                                    } else {
                                        Err(anyhow!(
                                            "tried to access `{:?}` at index {}",
                                            column,
                                            x
                                        ))
                                    }
                                }
                                _ => unimplemented!(),
                            }
                        } else {
                            unreachable!()
                        }
                    }

                    Builtin::ByteDecomposition => {
                        warn!("BYTEDECOMPOSITION constraints not yet implemented");
                        Ok(None)
                    }

                    Builtin::Not => Ok(Some(
                        Builtin::Sub.call(&[Node::one(), traversed_args[0].to_owned()]),
                    )),

                    Builtin::Eq => {
                        let x = &traversed_args[0];
                        let y = &traversed_args[1];
                        if traversed_args_t[0].is_bool() && traversed_args_t[1].is_bool() {
                            Ok(Some(Node {
                                _e: Builtin::Mul.raw_call(&[
                                    Builtin::Sub.call(&[x.clone(), y.clone()]),
                                    Builtin::Sub.call(&[x.clone(), y.clone()]),
                                ]),
                                // NOTE in this very specific case, we are sure that (x - y)² is boolean
                                _t: Some(x.t().same_scale(Magma::Boolean)),
                            }))
                        } else {
                            Ok(Some(Builtin::Sub.call(&[
                                traversed_args[0].to_owned(),
                                traversed_args[1].to_owned(),
                            ])))
                        }
                    }

                    b @ (Builtin::Add
                    | Builtin::Sub
                    | Builtin::Mul
                    | Builtin::Exp
                    | Builtin::Neg
                    | Builtin::Inv
                    | Builtin::Shift) => Ok(Some(b.call(&traversed_args))),
                }
            }

            FunctionClass::UserDefined(
                b @ Defined {
                    args: f_args,
                    body,
                    pure,
                },
            ) => {
                let f_mangle = format!(
                    "fn-{}-{}",
                    f.handle,
                    COUNTER
                        .get_or_init(|| AtomicUsize::new(0))
                        .fetch_add(1, std::sync::atomic::Ordering::Relaxed)
                );
                let traversed_args = b
                    .validate_args(traversed_args)
                    .with_context(|| anyhow!("validating call to `{}`", f.handle))?;
                let mut f_ctx =
                    SymbolTable::derived(ctx.clone(), &f_mangle, &f.handle.to_string(), *pure);
                for (i, f_arg) in f_args.iter().enumerate() {
                    f_ctx
                        .borrow_mut()
                        .insert_symbol(f_arg, traversed_args[i].clone())?;
                }
                reduce(body, root_ctx, &mut f_ctx, settings)
            }
            _ => unimplemented!("{:?}", f),
        }
    }
}

pub fn reduce(
    e: &AstNode,
    root_ctx: Rc<RefCell<SymbolTable>>,
    ctx: &mut Rc<RefCell<SymbolTable>>,
    settings: &CompileSettings,
) -> Result<Option<Node>> {
    match &e.class {
        Token::Keyword(_) | Token::Type(_) | Token::Range(_) => Ok(None),
        Token::Value(x) => Ok(Some(Node {
            _e: Expression::Const(x.clone(), Fr::from_str(&x.to_string())),
            _t: Some(if *x >= Zero::zero() && *x <= One::one() {
                Type::Scalar(Magma::Boolean)
            } else {
                Type::Scalar(Magma::Integer)
            }),
        })),
        Token::Symbol(name) => {
            let r = ctx
                .borrow_mut()
                .resolve_symbol(name)
                .with_context(|| make_ast_error(e))?;
            Ok(Some(r))
        }

        Token::List(args) => {
            if args.is_empty() {
                Ok(Some(Expression::List(vec![]).into()))
            } else if let Token::Symbol(verb) = &args[0].class {
                let func = ctx
                    .borrow()
                    .resolve_function(verb)
                    .with_context(|| make_ast_error(e))?;

                apply(&func, &args[1..], root_ctx, ctx, settings)
            } else {
                Err(anyhow!("not a function: `{:?}`", args[0])).with_context(|| make_ast_error(e))
            }
        }

        Token::DefColumn(name, _, k) => match k {
            Kind::Composite(e) => {
                let n = reduce(e, root_ctx, ctx, settings)?.unwrap();
                ctx.borrow_mut().edit_symbol(name, &|x| {
                    if let Expression::Column(_, kind) = x {
                        *kind = Kind::Composite(Box::new(n.clone()))
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
        | Token::Defpurefun(..)
        | Token::DefPermutation(..)
        | Token::DefPlookup(..)
        | Token::DefInrange(..) => Ok(None),
    }
    .with_context(|| make_ast_error(e))
}

fn reduce_toplevel(
    e: &AstNode,
    root_ctx: Rc<RefCell<SymbolTable>>,
    ctx: &mut Rc<RefCell<SymbolTable>>,
    settings: &CompileSettings,
) -> Result<Option<Constraint>> {
    match &e.class {
        Token::DefConstraint(name, domain, expr) => {
            let handle = Handle::new(&ctx.borrow().name, name);
            Ok(Some(Constraint::Vanishes {
                handle,
                domain: domain.to_owned(),
                expr: Box::new(
                    reduce(expr, root_ctx, ctx, settings)?
                        .unwrap_or_else(|| Expression::Void.into()),
                ),
            }))
        }
        Token::DefPlookup(name, parent, child) => {
            let handle = Handle::new(&ctx.borrow().name, name);
            let parents = parent
                .iter()
                .map(|e| reduce(e, root_ctx.clone(), ctx, settings).map(Option::unwrap))
                .collect::<Result<Vec<_>>>()?;
            let children = child
                .iter()
                .map(|e| reduce(e, root_ctx.clone(), ctx, settings).map(Option::unwrap))
                .collect::<Result<Vec<_>>>()?;
            if parents.len() != children.len() {
                Err(anyhow!(
                    "in {}, parents and children have different lengths: {} and {}",
                    name.red(),
                    parents.len(),
                    children.len()
                ))
            } else {
                Ok(Some(Constraint::Plookup(handle, parents, children)))
            }
        }
        Token::DefInrange(e, range) => {
            let handle = Handle::new(
                &ctx.borrow().name,
                names::Generator::default().next().unwrap(),
            );
            Ok(Some(Constraint::InRange(
                handle,
                reduce(e, root_ctx, ctx, settings)?.unwrap(),
                *range,
            )))
        }
        Token::DefColumns(columns) => {
            for _ in columns {
                reduce(e, root_ctx.clone(), ctx, settings)?;
            }
            Ok(None)
        }
        Token::DefModule(name) => {
            *ctx = SymbolTable::derived(root_ctx, name, name, false);
            Ok(None)
        }
        Token::Value(_) | Token::Symbol(_) | Token::List(_) | Token::Range(_) => {
            Err(anyhow!("Unexpected top-level form: {:?}", e))
        }
        Token::Defun(..)
        | Token::Defpurefun(..)
        | Token::DefAliases(_)
        | Token::DefunAlias(..)
        | Token::DefConsts(..) => Ok(None),
        Token::DefPermutation(to, from) => {
            // This silly piece of code ensures that columns involved in permutations
            // are marked as "used" in the symbol table
            from.iter()
                .map(|f| ctx.borrow_mut().resolve_symbol(&f.as_symbol().unwrap()))
                .collect::<Result<Vec<_>>>()
                .with_context(|| anyhow!("while defining permutation"))?;
            to.iter()
                .map(|f| ctx.borrow_mut().resolve_symbol(&f.as_symbol().unwrap()))
                .collect::<Result<Vec<_>>>()
                .with_context(|| anyhow!("while defining permutation"))?;
            Ok(Some(Constraint::Permutation(
                Handle::new(
                    &ctx.borrow().name,
                    names::Generator::default().next().unwrap(),
                ),
                from.iter()
                    .map(|f| Handle::new(&ctx.borrow().name, f.as_symbol().unwrap()))
                    .collect::<Vec<_>>(),
                to.iter()
                    .map(|f| Handle::new(&ctx.borrow().name, f.as_symbol().unwrap()))
                    .collect::<Vec<_>>(),
            )))
        }
        _ => unreachable!("{:?}", e.src),
    }
}

pub fn make_ast_error(exp: &AstNode) -> String {
    make_src_error(&exp.src, exp.lc)
}

pub fn pass(
    ast: &Ast,
    ctx: Rc<RefCell<SymbolTable>>,
    settings: &CompileSettings,
) -> Result<Vec<Constraint>> {
    let mut r = vec![];

    let mut module = ctx.clone();
    for exp in ast.exprs.iter() {
        if let Some(c) = reduce_toplevel(exp, ctx.clone(), &mut module, settings)? {
            r.push(c)
        }
    }
    Ok(r)
}
