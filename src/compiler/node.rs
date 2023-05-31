use crate::compiler::ColumnID;
use anyhow::*;
use cached::Cached;
use colored::{ColoredString, Colorize};
use either::Either;
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
use crate::pretty::{Base, Pretty, COLORS};
use crate::structs::Handle;

use super::{ConstraintSet, EvalSettings, Intrinsic, Kind, Magma, Type};

#[derive(Clone, Serialize, Deserialize, Debug, Hash, Eq)]
pub struct ColumnRef(pub Either<Handle, ColumnID>);
impl ColumnRef {
    pub fn of_handle(h: Handle) -> ColumnRef {
        ColumnRef(Either::Left(h))
    }
    pub fn of_id(i: ColumnID) -> ColumnRef {
        ColumnRef(Either::Right(i))
    }
    pub fn as_handle(&self) -> &Handle {
        self.0.as_ref().left().unwrap()
    }
    pub fn as_id(&self) -> ColumnID {
        *self.0.as_ref().right().unwrap()
    }
    pub fn is_id(&self) -> bool {
        matches!(self.0, Either::Right(_))
    }
    pub fn is_handle(&self) -> bool {
        matches!(self.0, Either::Left(_))
    }
    pub fn set_id(&mut self, i: ColumnID) {
        if self.is_handle() {
            self.0 = Either::Right(i)
        }
    }
}
impl std::cmp::PartialEq for ColumnRef {
    fn eq(&self, other: &Self) -> bool {
        match (&self.0, &other.0) {
            (Either::Left(x), Either::Left(y)) => x.eq(y),
            (Either::Left(_), Either::Right(_)) => false,
            (Either::Right(_), Either::Left(_)) => false,
            (Either::Right(x), Either::Right(y)) => x.eq(y),
        }
    }
}
impl Display for ColumnRef {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self.0 {
            Either::Left(handle) => write!(f, "{}", handle),
            Either::Right(id) => write!(f, "{}", id),
        }
    }
}
impl From<&Handle> for ColumnRef {
    fn from(h: &Handle) -> ColumnRef {
        ColumnRef::of_handle(h.to_owned())
    }
}
impl From<Handle> for ColumnRef {
    fn from(h: Handle) -> ColumnRef {
        ColumnRef::of_handle(h)
    }
}
impl From<ColumnID> for ColumnRef {
    fn from(i: ColumnID) -> ColumnRef {
        ColumnRef::of_id(i)
    }
}

#[derive(Clone, Serialize, Deserialize, Debug)]
pub enum Expression {
    Funcall {
        func: Intrinsic,
        args: Vec<Node>,
    },
    Const(BigInt, Option<Fr>),
    Column {
        handle: ColumnRef,
        kind: Kind<Node>,
        padding_value: Option<i64>,
        base: Base,
        fetched: bool,
    },
    ArrayColumn {
        handle: Handle,
        domain: Vec<usize>,
        base: Base,
    },
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
#[buildstructor::buildstructor]
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
    pub fn phantom_column(x: &ColumnRef) -> Node {
        Node {
            _e: Expression::Column {
                handle: x.to_owned(),
                kind: Kind::Phantom,
                padding_value: None,
                base: Base::Hex,
                fetched: false,
            },
            _t: None,
        }
    }
    pub fn with_type(self, t: Type) -> Self {
        Node {
            _t: Some(t),
            ..self
        }
    }
    #[builder(entry = "column", exit = "build", visibility = "pub")]
    pub fn new_column(
        handle: ColumnRef,
        base: Option<Base>,
        kind: Kind<Node>,
        padding_value: Option<i64>,
        t: Option<Magma>,
    ) -> Node {
        Node {
            _e: Expression::Column {
                handle,
                kind,
                padding_value,
                base: base.unwrap_or(Base::Hex),
                fetched: false,
            },
            _t: Some(Type::Column(t.unwrap_or(Magma::Integer))),
        }
    }
    pub fn typed_phantom_column(x: &ColumnRef, t: Type) -> Node {
        Self::phantom_column(x).with_type(t)
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
        self._t.unwrap_or_else(|| match &self.e() {
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
            Expression::Column { .. } => Type::Void,
            Expression::ArrayColumn { .. } => Type::Void,
            Expression::List(xs) => Type::List(
                xs.iter()
                    .map(Node::t)
                    .max()
                    .unwrap_or(Type::INFIMUM)
                    .magma(),
            ),
            Expression::Void => Type::Void,
        })
    }

    pub fn pretty_with_handle(&self, cs: &ConstraintSet) -> String {
        fn rec_pretty(s: &Node, depth: usize, cs: &ConstraintSet) -> ColoredString {
            let c = &COLORS[depth % COLORS.len()];
            match s.e() {
                Expression::Const(x, _) => format!("{}", x).color(*c),
                Expression::Column { handle, .. } => cs.handle(handle).to_string().color(*c),
                Expression::ArrayColumn {
                    handle,
                    domain: range,
                    ..
                } => format!(
                    "{}[{}:{}]",
                    handle.name,
                    range.first().unwrap(),
                    range.last().unwrap(),
                )
                .color(*c),
                Expression::List(ns) => format!("{{{}}}", format_list(ns, depth + 1, cs)).color(*c),
                Expression::Funcall { func, args } => {
                    format!("({:?} {})", func, format_list(args, depth + 1, cs)).color(*c)
                }
                Expression::Void => "nil".color(*c),
            }
        }
        fn format_list(ns: &[Node], depth: usize, cs: &ConstraintSet) -> String {
            ns.iter()
                .map(|n| rec_pretty(n, depth, cs).to_string())
                .collect::<Vec<_>>()
                .join(" ")
        }

        format!("{}", rec_pretty(self, 0, cs))
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
                    let mut args = args.iter().skip(1).peekable();
                    while let Some(a) = args.next() {
                        _debug(
                            a,
                            tty,
                            f,
                            faulty,
                            unclutter,
                            dim,
                            v.is_zero() || zero_context,
                        );
                        if args.peek().is_some() {
                            tty.cr();
                        }
                    }
                    tty.unshift();
                    tty.cr();
                    tty.write(")".color(c).to_string());
                    tty.write(
                        format!("[{}]", v.pretty_with_base(Base::Hex))
                            .color(c_v)
                            .to_string(),
                    );
                }
                Expression::Const(x, _) => {
                    let c = if dim && zero_context {
                        colored::Color::BrightBlack
                    } else {
                        colored::Color::White
                    };
                    tty.write(x.to_string().color(c).bold().to_string());
                }
                Expression::Column { handle: h, .. } => {
                    let v = f(n).unwrap_or_default();
                    let c = if dim && zero_context {
                        colored::Color::BrightBlack
                    } else if v.eq(faulty) {
                        colored::Color::Red
                    } else {
                        colored::Color::BrightWhite
                    };

                    let h = h.as_handle();
                    tty.write(
                        format!("{}[{}]", h.name, v.pretty_with_base(Base::Hex))
                            .color(c)
                            .bold()
                            .to_string(),
                    );
                }
                Expression::ArrayColumn { handle, .. } => tty.write(handle.to_string()),
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
            Expression::Column { .. } => 1,
            Expression::ArrayColumn { .. } => 0,
            Expression::List(xs) => xs.iter().map(Node::size).sum::<usize>(),
            Expression::Void => 0,
        }
    }

    /// Compute the maximum future-shifting in the node
    pub fn past_spill(&self) -> isize {
        fn _past_span(e: &Expression, ax: isize) -> isize {
            match e {
                Expression::Funcall { func, args } => {
                    let mut mine = ax;
                    if let Intrinsic::Shift = func {
                        let arg_big = args[1]
                            .pure_eval()
                            .unwrap_or_else(|_| panic!("{}", args[1].to_string().as_str()));
                        let arg = arg_big
                            .to_isize()
                            .unwrap_or_else(|| panic!("{}", arg_big.to_string().as_str()));
                        mine = mine.min(mine + arg);
                    }
                    args.iter().map(|e| _past_span(e.e(), mine)).min().unwrap()
                }
                Expression::List(es) => es.iter().map(|e| _past_span(e.e(), ax)).min().unwrap(),
                _ => ax,
            }
        }

        _past_span(self.e(), 0).min(0)
    }

    /// Compute the maximum future-shifting in the node
    pub fn future_spill(&self) -> isize {
        fn _future_span(e: &Expression, ax: isize) -> isize {
            match e {
                Expression::Funcall { func, args } => {
                    let mut mine = ax;
                    if let Intrinsic::Shift = func {
                        let arg_big = args[1]
                            .pure_eval()
                            .unwrap_or_else(|_| panic!("{}", args[1].to_string().as_str()));
                        let arg = arg_big
                            .to_isize()
                            .unwrap_or_else(|| panic!("{}", arg_big.to_string().as_str()));
                        mine = mine.max(mine + arg);
                    }
                    args.iter()
                        .map(|e| _future_span(e.e(), mine))
                        .max()
                        .unwrap()
                }
                Expression::List(es) => es.iter().map(|e| _future_span(e.e(), ax)).max().unwrap(),
                _ => ax,
            }
        }

        _future_span(self.e(), 0).max(0)
    }

    pub fn add_id_to_handles(&mut self, set_id: &dyn Fn(&mut ColumnRef)) {
        match self.e_mut() {
            Expression::Funcall { args, .. } => {
                args.iter_mut().for_each(|e| e.add_id_to_handles(set_id))
            }

            Expression::Column { handle, .. } => set_id(handle),
            Expression::List(xs) => xs.iter_mut().for_each(|x| x.add_id_to_handles(set_id)),

            Expression::ArrayColumn { .. } | Expression::Const(_, _) | Expression::Void => {}
        }
    }

    pub fn dependencies(&self) -> HashSet<ColumnRef> {
        self.leaves()
            .into_iter()
            .filter_map(|e| match e.e() {
                Expression::Column { handle, .. } => Some(handle.clone()),
                _ => None,
            })
            .collect()
    }

    pub fn core_dependencies(&self) -> HashSet<ColumnRef> {
        self.leaves()
            .into_iter()
            .filter_map(|e| match e.e() {
                Expression::Column {
                    handle, fetched, ..
                } => {
                    if *fetched {
                        None
                    } else {
                        Some(handle.clone())
                    }
                }
                _ => None,
            })
            .collect()
    }

    /// Evaluate a compile-time known value
    pub fn pure_eval(&self) -> Result<BigInt> {
        match self.e() {
            Expression::Funcall { func, args } => match func {
                Intrinsic::Add => {
                    let args = args
                        .iter()
                        .map(|x| x.pure_eval())
                        .collect::<Result<Vec<_>>>()?;
                    Ok(args.iter().fold(BigInt::zero(), |ax, x| ax + x))
                }
                Intrinsic::Sub => {
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
                Intrinsic::Mul => {
                    let args = args
                        .iter()
                        .map(|x| x.pure_eval())
                        .collect::<Result<Vec<_>>>()?;
                    Ok(args.iter().fold(BigInt::one(), |ax, x| ax * x))
                }
                Intrinsic::Neg => Ok(-args[0].pure_eval()?),
                Intrinsic::Exp => {
                    let args = args
                        .iter()
                        .map(|x| x.pure_eval())
                        .collect::<Result<Vec<_>>>()?;
                    Ok(args[0].pow(args[1].to_u32().ok_or_else(|| {
                        anyhow!("exponent {} is not an u32", args[1].to_string())
                    })?))
                }
                x => bail!("{} is not known at compile-time", x.to_string().red()),
            },
            Expression::Const(v, _) => Ok(v.to_owned()),
            _ => bail!("{} is not known at compile-time", self.to_string().red()),
        }
    }

    #[allow(dead_code)]
    pub fn eval_trace(
        &self,
        i: isize,
        get: &mut dyn FnMut(&ColumnRef, isize, bool) -> Option<Fr>,
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
        get: &mut dyn FnMut(&ColumnRef, isize, bool) -> Option<Fr>,
        cache: &mut Option<cached::SizedCache<Fr, Fr>>,
        settings: &EvalSettings,
    ) -> Option<Fr> {
        self.eval_fold(i, get, cache, settings, &mut |_, _| {})
    }

    pub fn eval_fold(
        &self,
        i: isize,
        get: &mut dyn FnMut(&ColumnRef, isize, bool) -> Option<Fr>,
        cache: &mut Option<cached::SizedCache<Fr, Fr>>,
        settings: &EvalSettings,
        f: &mut dyn FnMut(&Node, &Option<Fr>),
    ) -> Option<Fr> {
        let r = match self.e() {
            Expression::Funcall { func, args } => match func {
                Intrinsic::Add => {
                    let mut ax = Fr::zero();
                    for arg in args.iter() {
                        ax.add_assign(&arg.eval_fold(i, get, cache, settings, f)?)
                    }
                    Some(ax)
                }
                Intrinsic::Sub => {
                    let mut ax = args[0].eval_fold(i, get, cache, settings, f)?;
                    for arg in args.iter().skip(1) {
                        ax.sub_assign(&arg.eval_fold(i, get, cache, settings, f)?)
                    }
                    Some(ax)
                }
                Intrinsic::Mul => {
                    let mut ax = Fr::one();
                    for arg in args.iter() {
                        ax.mul_assign(&arg.eval_fold(i, get, cache, settings, f)?)
                    }
                    Some(ax)
                }
                Intrinsic::Exp => {
                    let mut ax = Fr::one();
                    let mantissa = args[0].eval_fold(i, get, cache, settings, f)?;
                    let exp = args[1].pure_eval().unwrap().to_usize().unwrap();
                    for _ in 0..exp {
                        ax.mul_assign(&mantissa);
                    }
                    Some(ax)
                }
                Intrinsic::Shift => {
                    let shift = args[1].pure_eval().unwrap().to_isize().unwrap();
                    args[0].eval_fold(i + shift, get, cache, &EvalSettings { wrap: false }, f)
                }
                Intrinsic::Eq => {
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
                Intrinsic::Neg => args[0].eval_fold(i, get, cache, settings, f).map(|mut x| {
                    x.negate();
                    x
                }),
                Intrinsic::Inv => {
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
                Intrinsic::Not => {
                    let mut r = Fr::one();
                    if let Some(x) = args[0].eval_fold(i, get, cache, settings, f) {
                        r.sub_assign(&x);
                        Some(r)
                    } else {
                        None
                    }
                }
                Intrinsic::Nth => {
                    if let (
                        Expression::ArrayColumn {
                            handle: h,
                            domain: range,
                            base: _,
                        },
                        Expression::Const(idx, _),
                    ) = (&args[0].e(), &args[1].e())
                    {
                        let idx = idx.to_usize().unwrap();
                        if !range.contains(&idx) {
                            panic!("trying to access `{}` at index `{}`", h, idx);
                        }
                        get(&h.ith(idx).into(), i, settings.wrap)
                    } else {
                        unreachable!()
                    }
                }
                Intrinsic::Begin => unreachable!(),
                Intrinsic::IfZero => {
                    if args[0].eval_fold(i, get, cache, settings, f)?.is_zero() {
                        args[1].eval_fold(i, get, cache, settings, f)
                    } else {
                        args.get(2)
                            .map(|x| x.eval_fold(i, get, cache, settings, f))
                            .unwrap_or_else(|| Some(Fr::zero()))
                    }
                }
                Intrinsic::IfNotZero => {
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
            Expression::Column { handle, .. } => get(handle, i, settings.wrap),
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
                Expression::Column { .. } => ax.push(e.clone()),
                Expression::ArrayColumn { .. } => {}
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
            Expression::Column { handle, .. } => {
                write!(f, "{}", handle)
            }
            Expression::ArrayColumn { handle, domain, .. } => {
                write!(
                    f,
                    "{}[{}:{}]",
                    handle,
                    domain.first().unwrap(),
                    domain.last().unwrap(),
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
            Expression::Column {
                handle, fetched, ..
            } => {
                write!(
                    f,
                    "{}{:?}:{:?}",
                    if *fetched { "F:" } else { "" },
                    handle,
                    self._t
                )
            }
            Expression::ArrayColumn {
                handle,
                domain: range,
                ..
            } => {
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
