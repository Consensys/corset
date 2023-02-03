use anyhow::*;
use cached::Cached;
use colored::Colorize;
use num_bigint::BigInt;
use num_traits::{FromPrimitive, One, ToPrimitive, Zero};
use pairing_ce::ff::Field;
use pairing_ce::{bn256::Fr, ff::PrimeField};
use serde::{Deserialize, Serialize};
use std::{
    collections::{HashMap, HashSet},
    fmt::{Debug, Display, Formatter},
};

use crate::compiler::codetyper::Tty;
use crate::pretty::Pretty;

use super::{Builtin, EvalSettings, Handle, Kind, Magma, Type};

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
    pub fn from_const(x: isize) -> Node {
        Node {
            _e: Expression::Const(BigInt::from_isize(x).unwrap(), Fr::from_str(&x.to_string())),
            _t: Some(Type::Scalar(match x {
                0 | 1 => Magma::Boolean,
                _ => Magma::Integer,
            })),
        }
    }
    pub fn from_bigint(x: BigInt) -> Node {
        Node {
            _e: Expression::Const(x.to_owned(), Fr::from_str(&x.to_string())),
            _t: Some(Type::Scalar(if x.is_one() || x.is_zero() {
                Magma::Boolean
            } else {
                Magma::Integer
            })),
        }
    }
    pub fn from_handle(x: &Handle) -> Node {
        Node {
            _e: Expression::Column(x.clone(), Kind::Phantom),
            _t: None,
        }
    }
    pub fn with_type(self, t: Type) -> Self {
        Node {
            _t: Some(t),
            ..self
        }
    }
    pub fn from_typed_handle(x: &Handle, t: Type) -> Node {
        Self::from_handle(x).with_type(t)
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
            Expression::Column(..) => Type::Void,
            Expression::ArrayColumn(..) => Type::Void,
            Expression::List(xs) => Type::List(
                xs.iter()
                    .map(Node::t)
                    .fold(Type::INFIMUM, |a, b| a.max(&b))
                    .magma(),
            ),
            Expression::Void => Type::Void,
        })
    }

    pub fn debug(&self, f: &dyn Fn(&Node) -> Option<Fr>, unclutter: bool, dim: bool) -> String {
        fn _debug(
            n: &Node,
            tty: &mut Tty,
            f: &dyn Fn(&Node) -> Option<Fr>,
            faulty: &Fr, // the non-zero value of the constraint
            unclutter: bool,
            dim: bool,          // whether the user enabled --debug-dim
            zero_context: bool, // whether we are in a zero-path
        ) {
            let colors = [
                colored::Color::Red,
                colored::Color::Green,
                colored::Color::Yellow,
                colored::Color::Blue,
                colored::Color::Magenta,
                colored::Color::Cyan,
                colored::Color::BrightRed,
                colored::Color::BrightGreen,
                colored::Color::BrightYellow,
                colored::Color::BrightBlue,
                colored::Color::BrightMagenta,
                colored::Color::BrightCyan,
            ];
            let c = if dim && zero_context {
                colored::Color::BrightBlack
            } else {
                colors[tty.depth() % colors.len()]
            };

            match n.e() {
                Expression::Funcall { func, args } => {
                    let v = f(n).unwrap_or_default();
                    if v.is_zero() && unclutter {
                        tty.write("...".color(colored::Color::BrightBlack).to_string());
                        return;
                    }
                    let fname = func.to_string();
                    let c = if v.is_zero() && zero_context {
                        colored::Color::BrightBlack
                    } else {
                        c
                    };
                    let c_v = if dim && (zero_context || v.is_zero()) {
                        colored::Color::BrightBlack
                    } else if v.eq(faulty) {
                        colored::Color::Red
                    } else {
                        colored::Color::White
                    };

                    tty.write(format!("({fname} ").color(c).to_string());
                    tty.shift(fname.len() + 2);
                    if let Some(a) = args.get(0) {
                        tty.latch_indent();
                        _debug(
                            a,
                            tty,
                            f,
                            faulty,
                            unclutter,
                            dim,
                            v.is_zero() || zero_context,
                        );
                    }
                    tty.cr();
                    for a in args.iter().skip(1) {
                        _debug(
                            a,
                            tty,
                            f,
                            faulty,
                            unclutter,
                            dim,
                            v.is_zero() || zero_context,
                        );
                        tty.cr();
                    }
                    tty.unshift();
                    tty.write(")".color(c).to_string());
                    tty.append(format!("[{}]", v.pretty()).color(c_v).to_string())
                }
                Expression::Const(x, _) => {
                    let c = if dim && zero_context {
                        colored::Color::BrightBlack
                    } else {
                        colored::Color::White
                    };
                    tty.write(x.to_string().color(c).bold().to_string());
                }
                Expression::Column(h, _) => {
                    let v = f(n).unwrap_or_default();
                    let c = if dim && zero_context {
                        colored::Color::BrightBlack
                    } else if v.eq(faulty) {
                        colored::Color::Red
                    } else {
                        colored::Color::BrightWhite
                    };

                    tty.write(
                        format!("{}[{}]", h.name, v.pretty())
                            .color(c)
                            .bold()
                            .to_string(),
                    );
                }
                Expression::ArrayColumn(h, _) => tty.write(h.to_string()),
                Expression::List(ns) => {
                    let v = f(n).unwrap();
                    let c = if v.is_zero() && dim {
                        colored::Color::BrightBlack
                    } else {
                        c
                    };
                    if v.is_zero() && unclutter {
                        tty.write("...".color(c).to_string());
                        return;
                    }
                    tty.write("{begin".color(c).to_string());
                    tty.cr();
                    tty.shift(3);
                    for a in ns.iter() {
                        _debug(
                            a,
                            tty,
                            f,
                            faulty,
                            unclutter,
                            dim,
                            v.is_zero() || zero_context,
                        );
                        tty.cr();
                    }
                    tty.unshift();
                    tty.write("}".color(c).to_string());
                }
                Expression::Void => tty.write("∅"),
            };
        }

        let mut tty = Tty::new();
        let faulty = f(self).unwrap_or_default();
        _debug(self, &mut tty, f, &faulty, unclutter, dim, false);
        tty.page_feed()
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
    pub fn past_spill(&self) -> isize {
        fn _past_span(e: &Node, ax: &mut isize) {
            match e.e() {
                Expression::Funcall { func, args } => {
                    if let Builtin::Shift = func {
                        let arg_big = args[1]
                            .pure_eval()
                            .unwrap_or_else(|_| panic!("{}", args[1].to_string().as_str()));
                        let arg = arg_big
                            .to_isize()
                            .unwrap_or_else(|| panic!("{}", arg_big.to_string().as_str()));
                        *ax = (*ax).min(*ax + arg);
                    }
                    args.iter().for_each(|e| _past_span(e, ax))
                }
                Expression::List(es) => es.iter().for_each(|e| _past_span(e, ax)),
                _ => {}
            }
        }

        let mut span = 0;
        _past_span(self, &mut span);
        span.min(0) as isize
    }

    /// Compute the maximum future-shifting in the node
    pub fn future_spill(&self) -> usize {
        fn _future_span(e: &Node, ax: &mut isize) {
            match e.e() {
                Expression::Funcall { func, args } => {
                    if let Builtin::Shift = func {
                        let arg_big = args[1]
                            .pure_eval()
                            .unwrap_or_else(|_| panic!("{}", args[1].to_string().as_str()));
                        let arg = arg_big
                            .to_isize()
                            .unwrap_or_else(|| panic!("{}", arg_big.to_string().as_str()));
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
                x => bail!("{} is not known at compile-time", x.to_string().red()),
            },
            Expression::Const(v, _) => Ok(v.to_owned()),
            _ => bail!("{} is not known at compile-time", self.to_string().red()),
        }
    }

    pub fn eval_trace(
        &self,
        i: isize,
        get: &mut dyn FnMut(&Handle, isize, bool) -> Option<Fr>,
        cache: &mut Option<cached::SizedCache<Fr, Fr>>,
        settings: &EvalSettings,
    ) -> (Option<Fr>, HashMap<String, Option<Fr>>) {
        let mut trace = HashMap::new();
        let r = self.eval_fold(i, get, cache, settings, &mut |n, v| {
            if !matches!(n.e(), Expression::List(_) | Expression::Const(..)) {
                trace.insert(n.to_string(), *v);
            }
        });
        (r, trace)
    }

    pub fn eval(
        &self,
        i: isize,
        get: &mut dyn FnMut(&Handle, isize, bool) -> Option<Fr>,
        cache: &mut Option<cached::SizedCache<Fr, Fr>>,
        settings: &EvalSettings,
    ) -> Option<Fr> {
        self.eval_fold(i, get, cache, settings, &mut |_, _| {})
    }

    pub fn eval_fold(
        &self,
        i: isize,
        get: &mut dyn FnMut(&Handle, isize, bool) -> Option<Fr>,
        cache: &mut Option<cached::SizedCache<Fr, Fr>>,
        settings: &EvalSettings,
        f: &mut dyn FnMut(&Node, &Option<Fr>),
    ) -> Option<Fr> {
        let r = match self.e() {
            Expression::Funcall { func, args } => match func {
                Builtin::Add => {
                    let mut ax = Fr::zero();
                    for arg in args.iter() {
                        ax.add_assign(&arg.eval_fold(i, get, cache, settings, f)?)
                    }
                    Some(ax)
                }
                Builtin::Sub => {
                    let mut ax = args[0].eval_fold(i, get, cache, settings, f)?;
                    for arg in args.iter().skip(1) {
                        ax.sub_assign(&arg.eval_fold(i, get, cache, settings, f)?)
                    }
                    Some(ax)
                }
                Builtin::Mul => {
                    let mut ax = Fr::one();
                    for arg in args.iter() {
                        ax.mul_assign(&arg.eval_fold(i, get, cache, settings, f)?)
                    }
                    Some(ax)
                }
                Builtin::Exp => {
                    let mut ax = Fr::one();
                    let mantissa = args[0].eval_fold(i, get, cache, settings, f)?;
                    let exp = args[1].pure_eval().unwrap().to_usize().unwrap();
                    for _ in 0..exp {
                        ax.mul_assign(&mantissa);
                    }
                    Some(ax)
                }
                Builtin::Shift => {
                    let shift = args[1].pure_eval().unwrap().to_isize().unwrap();
                    args[0].eval_fold(i + shift, get, cache, &EvalSettings { wrap: false }, f)
                }
                Builtin::Eq => {
                    let (x, y) = (
                        args[0].eval_fold(i, get, cache, settings, f)?,
                        args[1].eval_fold(i, get, cache, settings, f)?,
                    );
                    if args[0].t().is_bool() && args[1].t().is_bool() {
                        Some(if x.eq(&y) { Fr::zero() } else { Fr::one() })
                    } else {
                        let mut ax = x;
                        ax.sub_assign(&y);
                        Some(ax)
                    }
                }
                Builtin::Neg => args[0].eval_fold(i, get, cache, settings, f).map(|mut x| {
                    x.negate();
                    x
                }),
                Builtin::Inv => {
                    let x = args[0].eval_fold(i, get, cache, settings, f);
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
                    if let Some(x) = args[0].eval_fold(i, get, cache, settings, f) {
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
                    if args[0].eval_fold(i, get, cache, settings, f)?.is_zero() {
                        args[1].eval_fold(i, get, cache, settings, f)
                    } else {
                        args.get(2)
                            .map(|x| x.eval_fold(i, get, cache, settings, f))
                            .unwrap_or_else(|| Some(Fr::zero()))
                    }
                }
                Builtin::IfNotZero => {
                    if !args[0].eval_fold(i, get, cache, settings, f)?.is_zero() {
                        args[1].eval_fold(i, get, cache, settings, f)
                    } else {
                        args.get(2)
                            .map(|x| x.eval_fold(i, get, cache, settings, f))
                            .unwrap_or_else(|| Some(Fr::zero()))
                    }
                }
            },
            Expression::Const(v, x) => {
                Some(x.unwrap_or_else(|| panic!("{} is not an Fr element.", v)))
            }
            Expression::Column(handle, ..) => get(handle, i, settings.wrap),
            Expression::List(xs) => xs
                .iter()
                .filter_map(|x| x.eval_fold(i, get, cache, settings, f))
                .find(|x| !x.is_zero())
                .or_else(|| Some(Fr::zero())),
            _ => unreachable!("{:?}", self),
        };
        f(self, &r);
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
                write!(f, "{:?}:{:?}", handle, self._t)
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
