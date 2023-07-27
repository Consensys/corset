use crate::compiler::ColumnID;
use anyhow::*;
use cached::Cached;
use num_bigint::BigInt;
use num_traits::{FromPrimitive, One, ToPrimitive, Zero};
use owo_colors::{colored::Color, OwoColorize};
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

#[derive(Clone, Serialize, Deserialize, Debug, Eq)]
pub struct ColumnRef {
    h: Option<Handle>,
    id: Option<ColumnID>,
}
impl ColumnRef {
    pub fn from_handle(h: Handle) -> ColumnRef {
        ColumnRef {
            h: Some(h),
            id: None,
        }
    }
    pub fn from_id(i: ColumnID) -> ColumnRef {
        ColumnRef {
            h: None,
            id: Some(i),
        }
    }
    pub fn id(mut self, i: ColumnID) -> Self {
        if let Some(id) = self.id {
            if id != i {
                panic!("can not re-assign ID")
            }
        }

        self.id = Some(i);
        self
    }
    pub fn handle(mut self, h: Handle) -> Self {
        if let Some(ho) = self.h {
            if ho != h {
                panic!("can not re-assign handle")
            }
        }

        self.h = Some(h);
        self
    }
    pub fn as_handle(&self) -> &Handle {
        self.h.as_ref().unwrap()
    }
    pub fn as_id(&self) -> ColumnID {
        self.id.unwrap()
    }
    pub fn is_id(&self) -> bool {
        self.id.is_some()
    }
    pub fn is_handle(&self) -> bool {
        self.h.is_some()
    }
    pub fn set_id(&mut self, i: ColumnID) {
        if let Some(id) = self.id {
            if id != i {
                panic!("can not re-assign ID")
            }
        }

        self.id = Some(i);
    }
    pub fn map<T, F, G>(&self, f: F, g: G) -> T
    where
        F: FnOnce(ColumnID) -> T,
        G: FnOnce(&Handle) -> T,
    {
        if let Some(handle) = self.h.as_ref() {
            g(handle)
        } else if let Some(id) = self.id {
            f(id)
        } else {
            unreachable!()
        }
    }
}
impl std::cmp::PartialEq for ColumnRef {
    fn eq(&self, other: &Self) -> bool {
        self.id
            .zip(other.id)
            .map(|(x, y)| x.eq(&y))
            .or(self.h.as_ref().zip(other.h.as_ref()).map(|(x, y)| x.eq(y)))
            .unwrap_or(false)
    }
}
impl std::hash::Hash for ColumnRef {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        if self.is_id() {
            self.as_id().hash(state);
        } else {
            self.as_handle().hash(state);
        }
    }
}

impl Display for ColumnRef {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if self.is_handle() {
            write!(f, "{}", self.as_handle())
        } else if self.is_id() {
            write!(f, "col#{}", self.as_id())
        } else {
            unreachable!()
        }
    }
}
impl From<&Handle> for ColumnRef {
    fn from(h: &Handle) -> ColumnRef {
        ColumnRef::from_handle(h.to_owned())
    }
}
impl From<Handle> for ColumnRef {
    fn from(h: Handle) -> ColumnRef {
        ColumnRef::from_handle(h)
    }
}
impl From<ColumnID> for ColumnRef {
    fn from(i: ColumnID) -> ColumnRef {
        ColumnRef::from_id(i)
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
        handle: ColumnRef,
        domain: Vec<usize>,
        base: Base,
    },
    List(Vec<Node>),
    Void,
}

#[derive(Clone, Serialize, Deserialize)]
pub struct Node {
    _e: Expression,
    _t: Option<Type>,
    dbg: Option<String>,
}
impl From<Expression> for Node {
    fn from(e: Expression) -> Self {
        Node::from_expr(e)
    }
}
#[buildstructor::buildstructor]
impl Node {
    pub fn from_expr(e: Expression) -> Node {
        Node {
            /// the expresssion contained within the node
            _e: e,
            /// if set, the type of the node; it wil be computed on the fly
            /// otherwise
            _t: None,
            /// if set, a string containing the original code of the node for
            /// debugging purposes
            dbg: None,
        }
    }
    pub fn from_const(x: isize) -> Node {
        Node {
            _e: Expression::Const(BigInt::from_isize(x).unwrap(), Fr::from_str(&x.to_string())),
            _t: Some(Type::Scalar(match x {
                0 | 1 => Magma::Boolean,
                _ => Magma::Integer,
            })),
            dbg: None,
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
            dbg: None,
        }
    }
    pub fn with_type(self, t: Type) -> Self {
        Node {
            _t: Some(t),
            ..self
        }
    }
    pub fn with_debug(self, dbg: Option<String>) -> Self {
        Node { dbg, ..self }
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
            dbg: None,
        }
    }
    #[builder(entry = "array_column", exit = "build", visibility = "pub")]
    fn new_array_column(
        handle: ColumnRef,
        domain: Vec<usize>,
        base: Option<Base>,
        t: Option<Magma>,
    ) -> Node {
        Node {
            _e: Expression::ArrayColumn {
                handle,
                domain,
                base: base.unwrap_or(Base::Hex),
            },
            _t: Some(Type::ArrayColumn(t.unwrap_or(Magma::Integer))),
            dbg: None,
        }
    }
    pub fn phantom_column(x: &ColumnRef, m: Magma) -> Node {
        Node {
            _e: Expression::Column {
                handle: x.to_owned(),
                kind: Kind::Phantom,
                padding_value: None,
                base: Base::Hex,
                fetched: false,
            },
            _t: Some(Type::Column(m)),
            dbg: None,
        }
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
            Expression::Column { handle, .. } => {
                unreachable!("COLUMN {} SHOULD BE TYPED", handle.pretty())
            }
            Expression::ArrayColumn { .. } => unreachable!("ARRAYCOLUMN SHOULD BE TYPED"),
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
    pub fn dbg(&self) -> Option<&String> {
        self.dbg.as_ref()
    }

    pub fn pretty_with_handle(&self, cs: &ConstraintSet) -> String {
        fn rec_pretty(s: &Node, depth: usize, cs: &ConstraintSet) -> String {
            let c = &COLORS[depth % COLORS.len()];
            match s.e() {
                Expression::Const(x, _) => format!("{}", x).color(*c).to_string(),
                Expression::Column { handle, .. } => {
                    cs.handle(handle).to_string().color(*c).to_string()
                }
                Expression::ArrayColumn {
                    handle,
                    domain: range,
                    ..
                } => format!(
                    "{}[{}:{}]",
                    handle.as_handle().name,
                    range.first().unwrap(),
                    range.last().unwrap(),
                )
                .color(*c)
                .to_string(),
                Expression::List(ns) => format!("{{{}}}", format_list(ns, depth + 1, cs))
                    .color(*c)
                    .to_string(),
                Expression::Funcall { func, args } => {
                    format!("({:?} {})", func, format_list(args, depth + 1, cs))
                        .color(*c)
                        .to_string()
                }
                Expression::Void => "nil".color(*c).to_string(),
            }
        }
        fn format_list(ns: &[Node], depth: usize, cs: &ConstraintSet) -> String {
            ns.iter()
                .map(|n| rec_pretty(n, depth, cs))
                .collect::<Vec<_>>()
                .join(" ")
        }

        rec_pretty(self, 0, cs)
    }

    /// Compute the number of operations required to execute to fully compute the [`Expression`]
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

    /// Return whether this [`Expression`] is susceptible to overflow withtin the field
    pub fn may_overflow(&self) -> bool {
        match self.e() {
            Expression::Funcall { func, args } => match func {
                Intrinsic::Add => args.iter().any(|a| !a.t().is_bool()),
                // TODO: see with Olivier
                Intrinsic::Sub => false,
                Intrinsic::Mul => args.iter().any(|a| !a.t().is_bool()),
                // exponentiation are compile-time computed, hence cannot overflow
                Intrinsic::Exp => false,
                Intrinsic::Shift => false,
                Intrinsic::Neg => false,
                Intrinsic::Inv => false,
                Intrinsic::Begin => unreachable!(),
                Intrinsic::IfZero | Intrinsic::IfNotZero => {
                    args[1].may_overflow() || args.get(2).map(|a| a.may_overflow()).unwrap_or(false)
                }
            },
            Expression::Const(_, _) => false,
            Expression::Column {
                handle,
                kind,
                padding_value,
                base,
                fetched,
            } => false,
            Expression::ArrayColumn {
                handle,
                domain,
                base,
            } => false,
            Expression::List(ns) => ns.iter().any(Node::may_overflow),
            Expression::Void => false,
        }
    }

    /// Compute the depth of the tree representing [`Expression`]
    pub fn depth(&self) -> usize {
        match self.e() {
            Expression::Funcall { args, .. } => {
                1 + args.iter().map(Node::depth).max().unwrap_or_default()
            }
            Expression::List(xs) => 1 + xs.iter().map(Node::depth).max().unwrap_or_default(),
            Expression::Const(..)
            | Expression::Column { .. }
            | Expression::ArrayColumn { .. }
            | Expression::Void => 0,
        }
    }

    /// Compute the maximum past (negative) shift coefficient in the AST rooted at `self`
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

    /// Compute the maximum future (positive) shift coefficient in the AST rooted at `self`
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

    // TODO: replace with a generic map()
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

    /// Return all the leaves of the AST rooted at this `Node`
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

    /// Return all the columns appearing in the AST rooted at this `Node`
    pub fn dependencies(&self) -> HashSet<ColumnRef> {
        self.leaves()
            .into_iter()
            .filter_map(|e| match e.e() {
                Expression::Column { handle, .. } => Some(handle.clone()),
                _ => None,
            })
            .collect()
    }

    /// Try to evalaute a Node from compile-time information, return an `Err` otherwise
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

    pub fn debug(
        &self,
        f: &dyn Fn(&Node) -> Option<Fr>,
        unclutter: bool,
        dim: bool,
        src: bool,
    ) -> String {
        fn spacer(tty: &mut Tty, with_newlines: bool) {
            if with_newlines {
                tty.cr();
            } else {
                tty.write(" ");
            }
        }
        fn _debug(
            n: &Node,
            tty: &mut Tty,
            f: &dyn Fn(&Node) -> Option<Fr>,
            faulty: &Fr, // the non-zero value of the constraint
            unclutter: bool,
            dim: bool,           // whether the user enabled --debug-dim
            zero_context: bool,  // whether we are in a zero-path
            with_newlines: bool, // whether we want the expression to span several lines
            with_src: bool,
        ) {
            let colors = [
                Color::Red,
                Color::Green,
                Color::Yellow,
                Color::Blue,
                Color::Magenta,
                Color::Cyan,
                Color::BrightRed,
                Color::BrightGreen,
                Color::BrightYellow,
                Color::BrightBlue,
                Color::BrightMagenta,
                Color::BrightCyan,
            ];
            let c = if dim && zero_context {
                Color::BrightBlack
            } else {
                colors[tty.depth() % colors.len()]
            };

            match n.e() {
                Expression::Funcall { func, args } => {
                    let v = f(n).unwrap_or_default();
                    let zero_context = (v.is_zero() || zero_context) && dim;
                    if v.is_zero() && unclutter && n.depth() >= 1 {
                        tty.write("...".color(Color::BrightBlack).to_string());
                        return;
                    }
                    if with_src {
                        if let Some(dbg) = n.dbg.as_ref() {
                            tty.write(dbg.color(Color::BrightBlack).bold().to_string());
                            spacer(tty, with_newlines);
                        }
                    }
                    let fname = func.to_string();
                    let c = if zero_context { Color::BrightBlack } else { c };
                    let c_v = if dim && zero_context {
                        Color::BrightBlack
                    } else if v.eq(faulty) {
                        Color::Red
                    } else {
                        Color::White
                    };

                    if matches!(func, Intrinsic::Shift) {
                        let subponent = args[1].pure_eval().unwrap().to_i64().unwrap();
                        _debug(
                            &args[0],
                            tty,
                            f,
                            faulty,
                            unclutter,
                            dim,
                            zero_context,
                            false,
                            with_src,
                        );
                        if subponent > 0 {
                            tty.write("₊");
                        }
                        tty.write(crate::pretty::subscript(&subponent.to_string()));
                    } else {
                        tty.write(format!("({fname} ",).color(c).to_string());
                        if with_newlines {
                            tty.shift(fname.len() + 2);
                        }
                        if let Some(a) = args.get(0) {
                            _debug(
                                a,
                                tty,
                                f,
                                faulty,
                                unclutter
                                    && !matches!(func, Intrinsic::IfZero | Intrinsic::IfNotZero),
                                dim,
                                zero_context,
                                a.depth() > 2,
                                with_src,
                            );
                            spacer(tty, with_newlines);
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
                                zero_context,
                                a.depth() > 2,
                                with_src,
                            );
                            if args.peek().is_some() {
                                spacer(tty, with_newlines)
                            }
                        }
                        if with_newlines {
                            tty.unshift();
                            tty.cr();
                        }
                        tty.write(")".color(c).to_string());
                    }
                    tty.buffer_end(format!(
                        " → {}",
                        v.pretty_with_base(Base::Hex).color(c_v).bold()
                    ));
                }
                Expression::Const(x, _) => {
                    let c = if dim && zero_context {
                        Color::BrightBlack
                    } else {
                        Color::White
                    };
                    tty.write(x.to_string().color(c).bold().to_string());
                }
                Expression::Column { handle: h, .. } => {
                    let v = f(n).unwrap_or_default();
                    let c = if dim && zero_context {
                        Color::BrightBlack
                    } else if v.eq(faulty) {
                        Color::Red
                    } else {
                        Color::BrightWhite
                    };

                    tty.write(format!("{}", h));
                    tty.buffer_end(format!(
                        " → {}",
                        v.pretty_with_base(Base::Hex).color(c).bold()
                    ));
                }
                Expression::ArrayColumn { handle, .. } => tty.write(handle.to_string()),
                Expression::List(ns) => {
                    let v = f(n).unwrap();
                    let c = if v.is_zero() && dim {
                        Color::BrightBlack
                    } else {
                        c
                    };
                    if v.is_zero() && unclutter {
                        tty.write("...".color(c).to_string());
                        return;
                    }
                    tty.write("begin {".color(c).to_string());
                    tty.shift(3);
                    tty.cr();
                    let mut ns = ns.iter().peekable();
                    while let Some(n) = ns.next() {
                        _debug(
                            n,
                            tty,
                            f,
                            faulty,
                            unclutter,
                            dim,
                            v.is_zero() || zero_context,
                            n.depth() > 2,
                            with_src,
                        );
                        if ns.peek().is_some() {
                            spacer(tty, true);
                        }
                    }
                    tty.unshift();
                    tty.cr();
                    tty.write("}".color(c).to_string());
                }
                Expression::Void => tty.write("∅"),
            };
        }

        let mut tty = Tty::new();
        let faulty = f(self).unwrap_or_default();
        _debug(self, &mut tty, f, &faulty, unclutter, dim, false, true, src);
        tty.page_feed()
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
            Expression::Const(x, _) => write!(f, "{}", x)?,
            Expression::Column {
                handle, fetched, ..
            } => write!(f, "{}{}", if *fetched { "F:" } else { "" }, handle,)?,
            Expression::ArrayColumn {
                handle,
                domain: range,
                ..
            } => write!(
                f,
                "{}:{:?}[{}:{}]",
                handle,
                self.t(),
                range.first().unwrap(),
                range.last().unwrap(),
            )?,
            Expression::List(cs) => write!(f, "'({})", format_list(cs))?,
            Expression::Funcall { func, args } => write!(f, "({:?} {})", func, format_list(args))?,
            Expression::Void => write!(f, "nil")?,
            // Expression::Permutation(froms, tos) => write!(f, "{:?}<=>{:?}", froms, tos),
        };
        if let Some(t) = self._t {
            write!(f, ":{}", t)
        } else {
            write!(f, ":?")
        }
    }
}
