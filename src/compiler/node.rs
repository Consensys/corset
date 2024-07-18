use crate::column::{ColumnID, Value};
use anyhow::*;
use cached::Cached;
use num_bigint::BigInt;
use num_traits::{One, ToPrimitive, Zero};
use owo_colors::{colored::Color, OwoColorize};
use serde::de::Error;
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use std::write;
use std::{
    collections::HashSet,
    fmt::{Debug, Display, Formatter},
};

use crate::compiler::codetyper::Tty;
use crate::pretty::{Base, Pretty, COLORS};
use crate::structs::Handle;

use super::{ConstraintSet, Domain, EvalSettings, Intrinsic, Kind, Magma, Type};

#[derive(Clone, Debug, Eq)]
pub struct ColumnRef {
    h: Option<Handle>,
    id: Option<ColumnID>,
}
impl std::cmp::Ord for ColumnRef {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        if self.is_handle() && other.is_handle() {
            self.as_handle().cmp(other.as_handle())
        } else if self.is_id() && other.is_id() {
            self.as_id().cmp(&other.as_id())
        } else {
            panic!("uncomparable handles")
        }
    }
}
impl std::cmp::PartialOrd for ColumnRef {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
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
    #[allow(dead_code)]
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
    pub fn to_string_short(&self) -> String {
        self.map(|id| format!("col#{}", id), |handle| handle.name.to_owned())
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

impl Serialize for ColumnRef {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let fmt_str = match (&self.h, &self.id) {
            (None, None) => unreachable!(),
            (Some(h), None) => format!("{}", h.to_serialized_string()),
            (None, Some(id)) => format!("#{}", id),
            (Some(h), Some(id)) => format!("{}#{}", h.to_serialized_string(), id),
        };
        // Done
        serializer.serialize_str(&fmt_str)
    }
}

impl<'a> Deserialize<'a> for ColumnRef {
    fn deserialize<S: Deserializer<'a>>(deserializer: S) -> Result<Self, S::Error> {
        let st = String::deserialize(deserializer)?;
        // Split out column/register index
        let p1: Vec<&str> = st.split("#").collect();
        //
        match p1.len() {
            1 => {
                // Parse the handle itself
                let h = Handle::from_serialized_string(p1[0]).map_err(S::Error::custom)?;
                // Done
                std::result::Result::Ok(ColumnRef::from_handle(h))
            }
            2 => {
                // Parser the column ID (or report error)
                let id: ColumnID = p1[1].parse().map_err(S::Error::custom)?;
                // Parse the handle itself
                let h = Handle::from_serialized_string(p1[0]).map_err(S::Error::custom)?;
                // Done
                std::result::Result::Ok(ColumnRef::from_handle(h).id(id))
            }
            _ => {
                let msg = format!("invalid serialized ColumnRef: {}", st);
                Err(S::Error::custom(msg))
            }
        }
    }
}

#[derive(Clone, Serialize, Deserialize, Debug)]
pub enum Expression {
    Funcall {
        func: Intrinsic,
        args: Vec<Node>,
    },
    Const(Value),
    Column {
        handle: ColumnRef,
        shift: i16,
        kind: Kind<Box<Node>>,
        must_prove: bool,
        padding_value: Option<i64>,
        base: Base,
    },
    ArrayColumn {
        handle: ColumnRef,
        domain: Domain<isize>,
        base: Base,
    },
    ExoColumn {
        handle: ColumnRef,
        shift: i16,
        padding_value: Option<i64>,
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
            // the expresssion contained within the node
            _e: e,
            // if set, the type of the node; it will be computed on the fly
            // otherwise
            _t: None,
            // if set, a string containing the original code of the node for
            // debugging purposes
            dbg: None,
        }
    }
    pub fn from_isize(x: isize) -> Node {
        Node {
            _e: Expression::Const(Value::from(x)),
            _t: Some(Type::Scalar(match x {
                0 | 1 => Magma::binary(),
                _ => Magma::native(),
            })),
            dbg: None,
        }
    }
    pub fn from_bigint(x: BigInt) -> Node {
        let magma = if x.is_one() || x.is_zero() {
            Magma::binary()
        } else {
            Magma::native()
        };
        Node {
            _e: Expression::Const(Value::try_from(x).unwrap()),
            _t: Some(Type::Scalar(magma)),
            dbg: None,
        }
    }
    pub fn from_value(x: Value) -> Node {
        let magma = if x.is_one() || x.is_zero() {
            Magma::binary()
        } else {
            Magma::native()
        };
        Node {
            _e: Expression::Const(x),
            _t: Some(Type::Scalar(magma)),
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
        shift: Option<i16>,
        base: Option<Base>,
        kind: Option<Kind<Box<Node>>>,
        padding_value: Option<i64>,
        must_prove: Option<bool>,
        t: Option<Magma>,
    ) -> Node {
        let magma = t.unwrap_or(Magma::native());
        if magma.bit_size() > Magma::NATIVE.bit_size() {
            Node {
                _e: Expression::ExoColumn {
                    handle: handle.clone(),
                    shift: shift.unwrap_or(0),
                    padding_value,
                    base: base.unwrap_or_else(|| t.unwrap_or(Magma::native()).into()),
                },
                _t: Some(Type::Column(magma)),
                dbg: None,
            }
        } else {
            Node {
                _e: Expression::Column {
                    handle: handle.clone(),
                    shift: shift.unwrap_or(0),
                    kind: kind.unwrap_or(Kind::Computed),
                    must_prove: must_prove.unwrap_or(false),
                    padding_value,
                    base: base.unwrap_or_else(|| t.unwrap_or(Magma::native()).into()),
                },
                _t: Some(Type::Column(t.unwrap_or(Magma::native()))),
                dbg: None,
            }
        }
    }
    #[builder(entry = "array_column", exit = "build", visibility = "pub")]
    fn new_array_column(
        handle: ColumnRef,
        domain: Domain<isize>,
        base: Option<Base>,
        t: Option<Magma>,
    ) -> Node {
        Node {
            _e: Expression::ArrayColumn {
                handle,
                domain,
                base: base.unwrap_or(Base::Hex),
            },
            _t: Some(Type::ArrayColumn(t.unwrap_or(Magma::native()))),
            dbg: None,
        }
    }
    pub fn shift(mut self, i: i16) -> Self {
        match self.e_mut() {
            Expression::Funcall { args, .. } => {
                for a in args.iter_mut() {
                    *a = a.clone().shift(i);
                }
            }
            Expression::Column { shift, .. } | Expression::ExoColumn { shift, .. } => {
                *shift += i;
            }
            Expression::ArrayColumn { .. } => unreachable!(),
            Expression::List(ls) => {
                for l in ls.iter_mut() {
                    *l = l.clone().shift(i);
                }
            }
            Expression::Const(_) => {}
            Expression::Void => {}
        };
        self
    }
    pub fn one() -> Node {
        Self::from_expr(Expression::Const(Value::one()))
    }
    pub fn zero() -> Node {
        Self::from_expr(Expression::Const(Value::zero()))
    }
    pub fn is_constant(&self) -> bool {
        matches!(self.e(), Expression::Const(..))
    }
    pub fn is_exocolumn(&self) -> bool {
        matches!(self.e(), Expression::ExoColumn { .. })
    }
    /// Determine whether this expression is a list or not.
    pub fn is_list(&self) -> bool {
        match self.e() {
            // Obvious fails
            Expression::List(_) => true,
            Expression::Funcall { func, .. } => match func {
                Intrinsic::Begin => true,
                _ => false,
            },
            _ => false,
        }
    }
    /// Determines whether this expression is "atomic" or not.  An
    /// atomic expression is one which is never split into two or more
    /// expressions.  For example, an expression containing an
    /// if-then-else conditional is not atomic, as it will split into
    /// (at least) two expressions.  Likewise, an expression
    /// containing a list is not considered atomic.
    pub fn is_atomic(&self) -> bool {
        match self.e() {
            // Obvious fails
            Expression::List(_) => false,
            Expression::Void => false,
            // Obvious passes
            Expression::Const(_) => true,
            Expression::Column { .. }
            | Expression::ExoColumn { .. }
            | Expression::ArrayColumn { .. } => true,
            // Other
            Expression::Funcall { func, args } => match func {
                Intrinsic::Begin => false,
                Intrinsic::IfZero if args.len() > 2 => false,
                Intrinsic::IfNotZero if args.len() > 2 => false,
                _ => {
                    for arg in args {
                        if !arg.is_atomic() {
                            return false;
                        }
                    }
                    true
                }
            },
        }
    }
    pub fn e(&self) -> &Expression {
        &self._e
    }
    pub fn e_mut(&mut self) -> &mut Expression {
        &mut self._e
    }
    pub fn t(&self) -> Type {
        self._t
            .unwrap_or_else(|| match &self.e() {
                Expression::Funcall { func, args } => func
                    .typing(&args.iter().map(|a| a.t()).collect::<Vec<_>>())
                    .unwrap(),
                Expression::Const(ref x) => {
                    if x.is_zero() || x.is_one() {
                        Type::Scalar(Magma::binary())
                    } else {
                        Type::Scalar(Magma::native())
                    }
                }
                Expression::Column { handle, .. } => {
                    unreachable!("COLUMN {} SHOULD BE TYPED", handle.pretty())
                }
                Expression::ExoColumn { .. } => unreachable!("FIELDVALUES SHOULD BE TYPED"),
                Expression::ArrayColumn { .. } => unreachable!("ARRAYCOLUMN SHOULD BE TYPED"),
                Expression::List(xs) => {
                    let l_types = xs.iter().map(|x| x.t()).collect::<Vec<_>>();
                    super::max_type(l_types.iter()).unwrap()
                }
                Expression::Void => Type::Void,
            })
            .to_owned()
    }
    pub fn perspective(&self) -> Option<String> {
        match self.e() {
            Expression::Funcall { args: _, .. } | Expression::List(_) => todo!(),
            Expression::Const(_) => None,
            Expression::Column { handle, .. }
            | Expression::ExoColumn { handle, .. }
            | Expression::ArrayColumn { handle, .. } => handle.as_handle().perspective.to_owned(),
            Expression::Void => None,
        }
    }
    pub fn dbg(&self) -> Option<&String> {
        self.dbg.as_ref()
    }
    pub fn pretty_with_handle(&self, cs: &ConstraintSet) -> String {
        fn rec_pretty(s: &Node, depth: usize, cs: &ConstraintSet) -> String {
            let c = &COLORS[depth % COLORS.len()];
            match s.e() {
                Expression::Const(x) => format!("{}", x).color(*c).to_string(),
                Expression::Column { handle, .. } | Expression::ExoColumn { handle, .. } => {
                    cs.handle(handle).to_string().color(*c).to_string()
                }
                Expression::ArrayColumn { handle, domain, .. } => {
                    format!("{}{}", handle.as_handle().name, domain)
                        .color(*c)
                        .to_string()
                }
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
            Expression::Column { .. } => 0,
            Expression::ExoColumn { .. } => 0,
            Expression::ArrayColumn { .. } => 0,
            Expression::List(xs) => xs.iter().map(Node::size).sum::<usize>(),
            Expression::Void => 0,
        }
    }

    pub fn bit_size(&self) -> usize {
        self.t().m().bit_size()
    }

    /// Return whether this [`Expression`] is susceptible to overflow withtin the field
    pub fn may_overflow(&self) -> bool {
        // TODO: decide its future
        false
        // match self.e() {
        //     Expression::Funcall { func, args } => match func {
        //         Intrinsic::Add => args.iter().any(|a| !a.t().is_binary()),
        //         // TODO: see with Olivier
        //         Intrinsic::Sub => false,
        //         Intrinsic::Mul => args.iter().any(|a| !a.t().is_binary()),
        //         // exponentiation are compile-time computed, hence cannot overflow
        //         Intrinsic::Exp => false,
        //         Intrinsic::Neg => false,
        //         Intrinsic::Inv => false,
        //         Intrinsic::Normalize => false,
        //         Intrinsic::Begin => unreachable!(),
        //         Intrinsic::IfZero => {
        //             args[1].may_overflow() || args.get(2).map(|a| a.may_overflow()).unwrap_or(false)
        //         }
        //         _ => false,
        //     },
        //     Expression::Const(_) => false,
        //     Expression::Column { .. } => false,
        //     Expression::ExoColumn { .. } => false,
        //     Expression::ArrayColumn { .. } => false,
        //     Expression::List(ns) => ns.iter().any(Node::may_overflow),
        //     Expression::Void => false,
        // }
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
            | Expression::ExoColumn { .. }
            | Expression::ArrayColumn { .. }
            | Expression::Void => 0,
        }
    }

    /// Compute the maximum past (negative) shift coefficient in the AST rooted at `self`
    pub fn past_spill(&self) -> isize {
        self.leaves()
            .iter()
            .filter_map(|n| match n.e() {
                Expression::Column { shift, .. } | Expression::ExoColumn { shift, .. } => {
                    Some(*shift as isize)
                }
                Expression::ArrayColumn { .. } => unreachable!(),
                _ => None,
            })
            .filter(|x| *x < 0)
            .min()
            .unwrap_or(0)
    }

    /// Compute the maximum future (positive) shift coefficient in the AST rooted at `self`
    pub fn future_spill(&self) -> isize {
        self.leaves()
            .iter()
            .filter_map(|n| match n.e() {
                Expression::Column { shift, .. } | Expression::ExoColumn { shift, .. } => {
                    Some(*shift as isize)
                }
                Expression::ArrayColumn { .. } => unreachable!(),
                _ => None,
            })
            .filter(|x| *x > 0)
            .max()
            .unwrap_or(0)
            .max(0)
    }

    // TODO: replace with a generic map()
    pub fn add_id_to_handles(&mut self, set_id: &dyn Fn(&mut ColumnRef)) {
        match self.e_mut() {
            Expression::Funcall { args, .. } => {
                args.iter_mut().for_each(|e| e.add_id_to_handles(set_id))
            }

            Expression::Column { handle, .. } => set_id(handle),
            Expression::ExoColumn { handle, .. } => set_id(handle),
            Expression::List(xs) => xs.iter_mut().for_each(|x| x.add_id_to_handles(set_id)),

            Expression::ArrayColumn { .. } | Expression::Const(_) | Expression::Void => {}
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
                Expression::ExoColumn { .. } => ax.push(e.clone()),
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
                Expression::Column { handle, .. } | Expression::ExoColumn { handle, .. } => {
                    Some(handle.clone())
                }
                _ => None,
            })
            .collect()
    }

    /// Try to evalaute a Node from compile-time information, return an `Err` otherwise
    pub fn pure_eval(&self) -> Result<BigInt> {
        match self.e() {
            Expression::Funcall { func, args } => match func {
                // Here the vector operations behave like the normal ones, as we are sure to
                // be working with BigInt, as per the definition of pure_eval
                Intrinsic::Add | Intrinsic::VectorAdd => {
                    let args = args
                        .iter()
                        .map(|x| x.pure_eval())
                        .collect::<Result<Vec<_>>>()?;
                    Ok(args.iter().fold(BigInt::zero(), |ax, x| ax + x))
                }
                Intrinsic::Sub | Intrinsic::VectorSub => {
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
                Intrinsic::Mul | Intrinsic::VectorMul => {
                    let args = args
                        .iter()
                        .map(|x| x.pure_eval())
                        .collect::<Result<Vec<_>>>()?;
                    Ok(args.iter().fold(BigInt::one(), |ax, x| ax * x))
                }
                Intrinsic::Normalize => {
                    let x = args[0].pure_eval()?;
                    Ok(if x.is_zero() {
                        BigInt::zero()
                    } else {
                        BigInt::one()
                    })
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
            Expression::Const(v) => Ok(v.into()),
            _ => bail!("{} is not known at compile-time", self.to_string().red()),
        }
    }

    pub fn eval<F: Fn(&ColumnRef, isize, bool) -> Option<Value>>(
        &self,
        i: isize,
        get: F,
        cache: &mut Option<cached::SizedCache<Value, Value>>,
        settings: &EvalSettings,
    ) -> Option<Value> {
        self.eval_fold(i, &get, cache, settings, &mut |_, _| {})
    }

    pub fn eval_fold<F: Fn(&ColumnRef, isize, bool) -> Option<Value>>(
        &self,
        i: isize,
        get: &F,
        cache: &mut Option<cached::SizedCache<Value, Value>>,
        settings: &EvalSettings,
        f: &mut dyn FnMut(&Node, &Option<Value>),
    ) -> Option<Value> {
        let r = match self.e() {
            Expression::Funcall { func, args } => match func {
                Intrinsic::Add => {
                    let mut ax = args[0].eval_fold(i, get, cache, settings, f)?;
                    for arg in args.iter().skip(1) {
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
                    let mut ax = args[0].eval_fold(i, get, cache, settings, f)?;
                    for arg in args.iter().skip(1) {
                        if ax.is_zero() {
                            return Some(ax);
                        }
                        ax.mul_assign(&arg.eval_fold(i, get, cache, settings, f)?)
                    }
                    Some(ax)
                }
                Intrinsic::VectorAdd => {
                    let mut ax = args[0].eval_fold(i, get, cache, settings, f)?;
                    for arg in args.iter().skip(1) {
                        ax.vector_add_assign(&arg.eval_fold(i, get, cache, settings, f)?)
                    }
                    Some(ax)
                }
                Intrinsic::VectorSub => {
                    let mut ax = args[0].eval_fold(i, get, cache, settings, f)?;
                    for arg in args.iter().skip(1) {
                        ax.vector_sub_assign(&arg.eval_fold(i, get, cache, settings, f)?)
                    }
                    Some(ax)
                }
                Intrinsic::VectorMul => {
                    let mut ax = args[0].eval_fold(i, get, cache, settings, f)?;
                    for arg in args.iter().skip(1) {
                        ax.vector_mul_assign(&arg.eval_fold(i, get, cache, settings, f)?)
                    }
                    Some(ax)
                }
                Intrinsic::Exp => {
                    let mantissa = args[0].eval_fold(i, get, cache, settings, f)?;
                    let mut ax = mantissa.clone();
                    let exp = args[1].pure_eval().unwrap().to_usize().unwrap();
                    for _ in 1..exp {
                        ax.mul_assign(&mantissa);
                    }
                    Some(ax)
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
                                .cache_get_or_set_with(x.clone(), || x.inverse())
                                .to_owned()
                        })
                    } else {
                        x.map(|x| x.inverse())
                    }
                }
                Intrinsic::Normalize => args[0]
                    .eval_fold(i, get, cache, settings, f)
                    .map(|x| x.normalize()),
                Intrinsic::Begin => unreachable!(),
                Intrinsic::IfZero => {
                    if args[0].eval_fold(i, get, cache, settings, f)?.is_zero() {
                        args[1].eval_fold(i, get, cache, settings, f)
                    } else {
                        args.get(2)
                            .map(|x| x.eval_fold(i, get, cache, settings, f))
                            .unwrap_or_else(|| Some(Value::zero()))
                    }
                }
                Intrinsic::IfNotZero => {
                    if !args[0].eval_fold(i, get, cache, settings, f)?.is_zero() {
                        args[1].eval_fold(i, get, cache, settings, f)
                    } else {
                        args.get(2)
                            .map(|x| x.eval_fold(i, get, cache, settings, f))
                            .unwrap_or_else(|| Some(Value::zero()))
                    }
                }
            },
            Expression::Const(v) => Some(v.clone()),
            Expression::Column { handle, shift, .. } => {
                get(handle, i + (*shift as isize), settings.wrap)
            }
            Expression::ExoColumn { handle, shift, .. } => {
                get(handle, i + (*shift as isize), settings.wrap)
            }
            Expression::List(xs) => xs
                .iter()
                .filter_map(|x| x.eval_fold(i, get, cache, settings, f))
                .find(|x| !x.is_zero())
                .or_else(|| Some(Value::zero())),
            _ => unreachable!("{:?}", self),
        };
        f(self, &r);
        r
    }

    pub fn debug(
        &self,
        f: &dyn Fn(&Node) -> Option<Value>,
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
            f: &dyn Fn(&Node) -> Option<Value>,
            faulty: &Value, // the non-zero value of the constraint
            unclutter: bool,
            dim: bool,           // whether the user enabled --debug-dim
            zero_context: bool,  // whether we are in a zero-path
            with_newlines: bool, // whether we want the expression to span several lines
            with_src: bool,
            show_value: bool,
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
                    let v = f(n).unwrap_or_else(Value::bi_zero);
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

                    if with_newlines {
                        tty.within_styled(
                            &fname,
                            |s| s.color(c).to_string(),
                            Some(fname.len() + 2),
                            |tty| {
                                if let Some(a) = args.get(0) {
                                    _debug(
                                        a,
                                        tty,
                                        f,
                                        faulty,
                                        unclutter && !matches!(func, Intrinsic::IfZero),
                                        dim,
                                        zero_context,
                                        a.depth() > 2,
                                        with_src,
                                        show_value,
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
                                        show_value,
                                    );
                                    if args.peek().is_some() {
                                        spacer(tty, with_newlines)
                                    }
                                }
                            },
                        );
                    } else {
                        tty.write(format!("({fname} ",).color(c).to_string());
                        if let Some(a) = args.get(0) {
                            _debug(
                                a,
                                tty,
                                f,
                                faulty,
                                unclutter && !matches!(func, Intrinsic::IfZero),
                                dim,
                                zero_context,
                                a.depth() > 2,
                                with_src,
                                show_value,
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
                                show_value,
                            );
                            if args.peek().is_some() {
                                spacer(tty, with_newlines)
                            }
                        }
                        tty.write(")".color(c).to_string());
                    }
                    tty.annotate(format!(
                        "→ {}",
                        v.pretty_with_base(Base::Hex).color(c_v).bold()
                    ));
                }
                Expression::Const(x) => {
                    let c = if dim && zero_context {
                        Color::BrightBlack
                    } else {
                        Color::White
                    };
                    tty.write(x.to_string().color(c).bold().to_string());
                }
                Expression::Column {
                    handle: h,
                    shift,
                    base,
                    ..
                }
                | Expression::ExoColumn {
                    handle: h,
                    shift,
                    base,
                    ..
                } => {
                    let v = f(n).unwrap_or_else(Value::bi_zero);
                    let c = if dim && zero_context {
                        Color::BrightBlack
                    } else if v.eq(faulty) {
                        Color::Red
                    } else {
                        Color::White
                    };

                    tty.write(h.as_handle().name.color(c).bold().to_string());
                    if *shift != 0 {
                        if *shift > 0 {
                            tty.write("₊".color(c).to_string());
                        }
                        tty.write(
                            crate::pretty::subscript(&shift.to_string())
                                .color(c)
                                .to_string(),
                        );
                    }
                    if show_value {
                        tty.write(
                            format!("<{}>", v.pretty_with_base(*base))
                                .color(c)
                                .to_string(),
                        );
                    }
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
                    tty.within_styled(
                        "begin ",
                        |s| s.color(c).to_string(),
                        Some(3),
                        |tty| {
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
                                    show_value,
                                );
                                if ns.peek().is_some() {
                                    spacer(tty, true);
                                }
                            }
                        },
                    )
                }
                Expression::Void => tty.write("∅"),
            };
        }

        let mut tty = Tty::new().with_guides();
        let faulty = f(self).unwrap_or_else(Value::fr_zero);
        _debug(
            self, &mut tty, f, &faulty, unclutter, dim, false, true, src, true,
        );
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
            Expression::Const(x) => write!(f, "{}", x),
            Expression::Column { handle, shift, .. }
            | Expression::ExoColumn { handle, shift, .. } => {
                write!(
                    f,
                    "{}{}",
                    handle.to_string_short(),
                    if *shift > 0 {
                        format!("₊{}", crate::pretty::subscript(&shift.to_string()))
                    } else if *shift < 0 {
                        crate::pretty::subscript(&shift.to_string())
                    } else {
                        Default::default()
                    }
                )
            }
            Expression::ArrayColumn { handle, domain, .. } => {
                write!(f, "{}{}", handle.to_string_short(), domain)
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
            Expression::Const(x) => write!(f, "{}", x)?,
            Expression::Column { handle, .. } | Expression::ExoColumn { handle, .. } => {
                write!(f, "{}", handle,)?
            }
            Expression::ArrayColumn { handle, domain, .. } => {
                write!(f, "{}:{:?}{}", handle, self.t(), domain)?
            }
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
