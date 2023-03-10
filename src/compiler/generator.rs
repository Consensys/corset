use anyhow::*;
use cached::Cached;
use colored::Colorize;
use log::*;
use logging_timer::time;
use num_bigint::BigInt;
use num_traits::cast::ToPrimitive;
use num_traits::{One, Zero};
use once_cell::sync::OnceCell;
use pairing_ce::bn256::Fr;
use pairing_ce::ff::{Field, PrimeField};
use serde::{Deserialize, Serialize};
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::Debug;
use std::io::Write;
use std::rc::Rc;
use std::sync::atomic::AtomicUsize;

use super::tables::{ComputationTable, SymbolTable};
use super::{common::*, CompileSettings, Expression, Magma, Node, Type};
use crate::column::{Column, ColumnSet, Computation};
use crate::compiler::parser::*;
use crate::errors::{CompileError, RuntimeError};
use crate::pretty::Pretty;
use crate::structs::Handle;

static COUNTER: OnceCell<AtomicUsize> = OnceCell::new();

#[derive(Debug, Serialize, Deserialize, Clone)]
pub enum Constraint {
    Vanishes {
        handle: Handle,
        domain: Option<Vec<isize>>,
        expr: Box<Node>,
    },
    Plookup {
        handle: Handle,
        including: Vec<Node>,
        included: Vec<Node>,
    },
    Permutation {
        handle: Handle,
        from: Vec<Handle>,
        to: Vec<Handle>,
    },
    InRange {
        handle: Handle,
        exp: Node,
        max: Fr,
    },
}
impl Constraint {
    pub fn name(&self) -> String {
        match self {
            Constraint::Vanishes { handle, .. } => handle.to_string(),
            Constraint::Plookup { handle, .. } => handle.to_string(),
            Constraint::Permutation { handle, .. } => handle.to_string(),
            Constraint::InRange { handle, .. } => handle.to_string(),
        }
    }

    pub fn add_id_to_handles(&mut self, set_id: &dyn Fn(&mut Handle)) {
        match self {
            Constraint::Vanishes { expr, .. } => expr.add_id_to_handles(set_id),
            Constraint::Plookup {
                handle: _,
                including: xs,
                included: ys,
            } => xs
                .iter_mut()
                .chain(ys.iter_mut())
                .for_each(|e| e.add_id_to_handles(set_id)),
            Constraint::Permutation {
                handle: _,
                from: hs1,
                to: hs2,
            } => hs1.iter_mut().chain(hs2.iter_mut()).for_each(|h| set_id(h)),
            Constraint::InRange {
                handle: _,
                exp,
                max: _,
            } => exp.add_id_to_handles(set_id),
        }
    }

    pub(crate) fn size(&self) -> usize {
        match self {
            Constraint::Vanishes { expr, .. } => expr.size(),
            Constraint::Plookup { .. } => 1,
            Constraint::Permutation { .. } => 1,
            Constraint::InRange { .. } => 1,
        }
    }
}

/// Options used when evaluating an expression
pub struct EvalSettings {
    /// If true, negative indices will loop from the end of the column;
    /// otherwise, they will go up in the padding.
    pub wrap: bool,
}
impl Default for EvalSettings {
    fn default() -> Self {
        EvalSettings { wrap: true }
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    pub handle: Handle,
    pub class: FunctionClass,
}
#[derive(Debug, Clone)]
pub enum FunctionClass {
    /// A source-defined function
    UserDefined(Defined),
    /// A function acting on the AST
    Form(Form),
    /// A builtin function
    Builtin(Builtin),
    /// A field element function
    Intrinsic(Intrinsic),
    /// A name alias to any other function (including another alias)
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

impl FuncVerifier<Node> for Intrinsic {
    fn arity(&self) -> Arity {
        match self {
            Intrinsic::Add => Arity::AtLeast(1),
            Intrinsic::Sub => Arity::AtLeast(1),
            Intrinsic::Mul => Arity::AtLeast(1),
            Intrinsic::Exp => Arity::Dyadic,
            Intrinsic::Eq => Arity::Dyadic,
            Intrinsic::Neg => Arity::Monadic,
            Intrinsic::Inv => Arity::Monadic,
            Intrinsic::Not => Arity::Monadic,
            Intrinsic::Shift => Arity::Dyadic,
            Intrinsic::Begin => Arity::AtLeast(1),
            Intrinsic::IfZero => Arity::Between(2, 3),
            Intrinsic::IfNotZero => Arity::Between(2, 3),
            Intrinsic::Nth => Arity::Dyadic,
        }
    }
    fn validate_types(&self, args: &[Node]) -> Result<()> {
        let args_t = args.iter().map(|a| a.t()).collect::<Vec<_>>();
        let expected_t: &[&[Type]] = match self {
            Intrinsic::Add | Intrinsic::Sub | Intrinsic::Mul => {
                &[&[Type::Scalar(Magma::Any), Type::Column(Magma::Any)]]
            }
            Intrinsic::Exp => &[
                &[Type::Scalar(Magma::Any), Type::Column(Magma::Any)],
                &[Type::Scalar(Magma::Any)],
            ],
            Intrinsic::Eq => &[&[Type::Column(Magma::Any), Type::Scalar(Magma::Any)]],
            Intrinsic::Not => &[&[Type::Scalar(Magma::Boolean), Type::Column(Magma::Boolean)]],
            Intrinsic::Neg => &[&[Type::Scalar(Magma::Any), Type::Column(Magma::Any)]],
            Intrinsic::Inv => &[&[Type::Column(Magma::Any)]],
            Intrinsic::Shift => &[&[Type::Column(Magma::Any)], &[Type::Scalar(Magma::Any)]],
            Intrinsic::Nth => &[
                &[Type::ArrayColumn(Magma::Any)],
                &[Type::Scalar(Magma::Any)],
            ],
            Intrinsic::IfZero | Intrinsic::IfNotZero => &[
                &[
                    Type::Scalar(Magma::Any),
                    Type::Column(Magma::Any),
                    Type::List(Magma::Any),
                ],
                &[
                    Type::Scalar(Magma::Any),
                    Type::Column(Magma::Any),
                    Type::List(Magma::Any),
                ],
            ],
            Intrinsic::Begin => &[&[
                Type::Scalar(Magma::Any),
                Type::Column(Magma::Any),
                Type::List(Magma::Any),
            ]],
        };

        if super::compatible_with(expected_t, &args_t) {
            Ok(())
        } else {
            bail!(CompileError::TypeError(
                self.to_string(),
                expected_t,
                args_t
            ))
        }
    }
    fn validate_args(&self, args: &[Node]) -> Result<()> {
        FuncVerifier::validate_arity(self, args)
            .with_context(|| format!("while validating {}", self))
    }
}

#[derive(Default, Debug, Serialize, Deserialize, Clone)]
pub struct ConstraintSet {
    pub modules: ColumnSet,
    pub constraints: Vec<Constraint>,
    pub constants: HashMap<Handle, BigInt>,
    pub computations: ComputationTable,

    /// The spilling of a module is the maximum of the absolute values
    /// of the forward- and backward-shift observed in its constraints.
    /// The spilling is used to compute the padding of its columns.
    _spilling: HashMap<String, isize>, // module -> (past-spilling, future-spilling)
}
impl ConstraintSet {
    pub fn new(
        columns: ColumnSet,
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

    pub fn from_ptr<'a>(ptr: *const ConstraintSet) -> &'a Self {
        assert!(!ptr.is_null());
        unsafe { &*ptr }
    }

    pub fn mut_from_ptr<'a>(ptr: *mut ConstraintSet) -> &'a mut Self {
        assert!(!ptr.is_null());
        unsafe { &mut *ptr }
    }

    // TODO I hate this, see if we can automate it
    pub fn update_ids(&mut self) {
        let set_id = |h: &mut Handle| h.set_id(self.modules.id_of(h));
        self.constraints
            .iter_mut()
            .for_each(|x| x.add_id_to_handles(&set_id));
        self.computations.update_ids(&set_id)
    }

    pub fn get(&self, handle: &Handle) -> Result<&Column> {
        self.modules.get(handle)
    }

    pub fn get_mut(&mut self, handle: &Handle) -> Result<&mut Column> {
        self.modules.get_mut(handle)
    }

    pub fn raw_len_for_or_set(&mut self, m: &str, x: isize) -> isize {
        *self.modules.raw_len.entry(m.to_string()).or_insert(x)
    }

    pub fn spilling(&self, m: &str) -> Option<isize> {
        self._spilling.get(m).cloned()
    }

    pub fn spilling_or_insert(&mut self, m: &str) -> isize {
        *self._spilling.entry(m.to_string()).or_insert_with(|| {
            self.computations
                .iter()
                .filter_map(|c| match c {
                    Computation::Composite { target, exp } => {
                        if target.module == m {
                            Some(exp.past_spill() as isize)
                        } else {
                            None
                        }
                    }
                    _ => None,
                })
                .min()
                .unwrap_or(0)
                .abs()
                .max(
                    self.computations
                        .iter()
                        .filter_map(|c| match c {
                            Computation::Composite { target, exp } => {
                                if target.module == m {
                                    Some(exp.future_spill() as isize)
                                } else {
                                    None
                                }
                            }
                            _ => None,
                        })
                        .max()
                        .unwrap_or(0),
                )
        })
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
                Computation::Sorted { froms, .. } | Computation::CyclicFrom { froms, .. } => {
                    self.length_multiplier(&froms[0])
                }
                Computation::SortingConstraints { .. } => 1,
            })
            .unwrap_or(1)
            * self
                .modules
                .get(h)
                .unwrap()
                .intrinsic_size_factor
                .unwrap_or(1)
    }

    #[time("info", "Exporting expanded trace")]
    pub fn write(&mut self, out: &mut impl Write) -> Result<()> {
        let mut cache = cached::SizedCache::with_size(200000); // ~1.60MB cache

        out.write_all("{\"columns\":{\n".as_bytes())?;

        for (i, (module, columns)) in self.modules.cols.iter().enumerate() {
            debug!("Exporting {}", &module);
            if i > 0 {
                out.write_all(b",")?;
            }

            let empty_vec = Vec::new();
            let mut current_col = columns.iter().peekable();
            while let Some((name, &i)) = current_col.next() {
                trace!("Writing {}/{}", module, name);
                let column = &self.modules._cols[i];
                let handle = Handle::new(&module, &name);
                let value = column.value().unwrap_or(&empty_vec);
                let padding = if let Some(x) = column.padding_value {
                    Fr::from_str(&x.to_string()).unwrap()
                } else {
                    value.get(0).cloned().unwrap_or_else(|| {
                        self.computations
                            .computation_for(&handle)
                            .map(|c| match c {
                                Computation::Composite { exp, .. } => exp
                                    .eval(
                                        0,
                                        &mut |_, _, _| Some(Fr::zero()),
                                        &mut None,
                                        &EvalSettings::default(),
                                    )
                                    .unwrap_or_else(Fr::zero),
                                Computation::Interleaved { .. } => Fr::zero(),
                                Computation::Sorted { .. } => Fr::zero(),
                                Computation::CyclicFrom { .. } => Fr::zero(),
                                Computation::SortingConstraints { .. } => Fr::zero(),
                            })
                            .unwrap_or_else(Fr::zero)
                    })
                };

                out.write_all(format!("\"{}\":{{\n", handle).as_bytes())?;
                out.write_all("\"values\":[".as_bytes())?;

                let mut value = value.iter().peekable();
                while let Some(x) = value.next() {
                    out.write_all(
                        cache
                            .cache_get_or_set_with(x.to_owned(), || {
                                format!(
                                    "\"0x0{}\"",
                                    x.into_repr().to_string()[2..].trim_start_matches('0')
                                )
                            })
                            .as_bytes(),
                    )?;
                    if value.peek().is_some() {
                        out.write_all(b",")?;
                    }
                }
                out.write_all(b"],\n")?;
                out.write_all(
                    format!(
                        "\"padding_strategy\": {{\"action\": \"prepend\", \"value\": \"{}\"}}",
                        padding.pretty()
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
    ctx: &mut Rc<RefCell<SymbolTable>>,
    settings: &CompileSettings,
) -> Result<Option<Node>> {
    f.validate_args(args)
        .with_context(|| anyhow!("evaluating call to {:?}", f))?;

    match f {
        Form::For => {
            if let (Token::Symbol(i_name), Token::Range(is), body) =
                (&args[0].class, &args[1].class, &args[2])
            {
                let mut l = vec![];
                let mut t = Type::INFIMUM;
                for i in is {
                    let for_ctx_pretty_name = &ctx.borrow().pretty_name.clone();
                    let mut for_ctx = SymbolTable::derived(
                        ctx.clone(),
                        &format!(
                            "for-{}-{}",
                            COUNTER
                                .get_or_init(|| AtomicUsize::new(0))
                                .fetch_add(1, std::sync::atomic::Ordering::Relaxed),
                            i
                        ),
                        for_ctx_pretty_name,
                        false,
                        false,
                    );
                    for_ctx.borrow_mut().insert_symbol(
                        i_name,
                        Expression::Const(BigInt::from(*i), Fr::from_str(&i.to_string())).into(),
                    )?;

                    let r = reduce(&body.clone(), &mut for_ctx, settings)?.unwrap();
                    t = t.max(r.t());
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
                    .map(|e| reduce(e, ctx, settings))
                    .collect::<Result<Vec<_>>>()?;
                match reduced.len() {
                    0 => Ok(None),
                    1 => Ok(reduced[0].to_owned()),
                    _ => Ok(Some(
                        Intrinsic::Begin.call(
                            &reduced
                                .into_iter()
                                .map(|e| e.unwrap_or_else(|| Expression::Void.into()))
                                .collect::<Vec<_>>(),
                        )?,
                    )),
                }
            }
        }
        Form::Let => {
            let sub_ctx_name = format!("let-{}", ctx.borrow().name);
            let mut sub_ctx =
                SymbolTable::derived(ctx.clone(), &sub_ctx_name, &sub_ctx_name, false, false);
            for pair in args[0].as_list().unwrap().iter() {
                let pair = pair.as_list().unwrap();
                let name = pair[0].as_symbol().unwrap();
                let value = reduce(&pair[1], &mut sub_ctx, settings)?.unwrap();
                sub_ctx.borrow_mut().insert_symbol(name, value)?;
            }
            let body = reduce(&args[1], &mut sub_ctx, settings)?.unwrap();

            Ok(Some(body))
        }
        Form::Reduce => {
            let f_name = args[0].as_symbol().unwrap();
            let f = ctx.borrow().resolve_function(f_name)?;

            let mut body = reduce(&args[1], ctx, settings)?.unwrap();

            return match body.e_mut() {
                Expression::Column(_, _, _)
                | Expression::Void
                | Expression::ArrayColumn(_, _)
                | Expression::Funcall { .. }
                | Expression::Const(_, _) => panic!(),
                Expression::List(xs) => {
                    if xs.len() < 2 {
                        Ok(Some(body))
                    } else {
                        let mut r = apply_function(
                            &f,
                            vec![xs.pop().unwrap(), xs.pop().unwrap()],
                            ctx,
                            settings,
                        );
                        while let Some(x) = xs.pop() {
                            r = apply_function(&f, vec![x, r?.unwrap()], ctx, settings);
                        }
                        r
                    }
                }
            };
        }
    }
}

fn apply_defined(
    b: &Defined,
    h: &Handle,
    traversed_args: Vec<Node>,
    ctx: &mut Rc<RefCell<SymbolTable>>,
    settings: &CompileSettings,
) -> Result<Option<Node>> {
    let f_mangle = format!(
        "fn-{}-{}",
        h,
        COUNTER
            .get_or_init(|| AtomicUsize::new(0))
            .fetch_add(1, std::sync::atomic::Ordering::Relaxed)
    );
    b.validate_args(&traversed_args)
        .with_context(|| anyhow!("validating call to `{}`", h))?;
    let mut f_ctx = SymbolTable::derived(ctx.clone(), &f_mangle, &h.to_string(), b.pure, false);
    for (i, f_arg) in b.args.iter().enumerate() {
        f_ctx
            .borrow_mut()
            .insert_symbol(f_arg, traversed_args[i].clone())?;
    }
    reduce(&b.body, &mut f_ctx, settings)
}

fn apply_builtin(
    b: &Builtin,
    traversed_args: Vec<Node>,
    _ctx: &mut Rc<RefCell<SymbolTable>>,
    _settings: &CompileSettings,
) -> Result<Option<Node>> {
    b.validate_args(&traversed_args)?;

    match b {
        Builtin::ForceBool => {
            Ok(Some(traversed_args[0].clone().with_type(
                traversed_args[0].t().same_scale(Magma::Boolean),
            )))
        }
        Builtin::Len => {
            if let Expression::ArrayColumn(_, domain) = traversed_args[0].e() {
                Ok(Some(Node::from_const(domain.len().try_into().unwrap())))
            } else {
                bail!(RuntimeError::NotAnArray(traversed_args[0].e().clone()))
            }
        }
    }
}

fn apply_intrinsic(
    b: &Intrinsic,
    traversed_args: Vec<Node>,
    ctx: &mut Rc<RefCell<SymbolTable>>,
    _settings: &CompileSettings,
) -> Result<Option<Node>> {
    b.validate_args(&traversed_args)?;
    let traversed_args_t = traversed_args.iter().map(|a| a.t()).collect::<Vec<_>>();
    match b {
        // Begin flattens & concatenate any list argument
        Intrinsic::Begin => Ok(Some(Node {
            _e: Expression::List(traversed_args.into_iter().fold(vec![], |mut ax, mut e| {
                match e.e_mut() {
                    Expression::List(ref mut es) => {
                        ax.append(es);
                        ax
                    }
                    _ => {
                        ax.push(e.to_owned());
                        ax
                    }
                }
            })),
            _t: Some(
                traversed_args_t
                    .iter()
                    .fold(Type::INFIMUM, |a, b| a.max(*b)),
            ),
        })),

        b @ (Intrinsic::IfZero | Intrinsic::IfNotZero) => Ok(Some(b.call(&traversed_args)?)),

        Intrinsic::Nth => {
            if let Expression::ArrayColumn(handle, ..) = &traversed_args[0].e() {
                let i = traversed_args[1].pure_eval()?.to_usize().ok_or_else(|| {
                    anyhow!("{:?} is not a valid indice", traversed_args[1].pure_eval())
                })?;
                let array = ctx.borrow_mut().resolve_symbol(&handle.name)?;
                match array.e() {
                    Expression::ArrayColumn(handle, range) => {
                        if range.contains(&i) {
                            Ok(Some(Node {
                                _e: Expression::Column(
                                    Handle::new(&handle.module, format!("{}_{}", handle.name, i)),
                                    Kind::Atomic,
                                    None,
                                ),
                                _t: Some(Type::Column(array.t().magma())),
                            }))
                        } else {
                            bail!("tried to access `{:?}` at index {}", array, i)
                        }
                    }
                    _ => unimplemented!(),
                }
            } else {
                unreachable!()
            }
        }
        Intrinsic::Not => Ok(Some(
            Intrinsic::Sub
                .call(&[Node::one(), traversed_args[0].to_owned()])?
                .with_type(traversed_args[0].t().same_scale(Magma::Boolean)),
        )),

        Intrinsic::Eq => {
            let x = &traversed_args[0];
            let y = &traversed_args[1];
            if traversed_args_t[0].is_bool() && traversed_args_t[1].is_bool() {
                Ok(Some(Node {
                    _e: Intrinsic::Mul.raw_call(&[
                        Intrinsic::Sub.call(&[x.clone(), y.clone()])?,
                        Intrinsic::Sub.call(&[x.clone(), y.clone()])?,
                    ]),
                    // NOTE in this very specific case, we are sure that (x - y)² is boolean
                    _t: Some(x.t().same_scale(Magma::Boolean)),
                }))
            } else {
                Ok(Some(Intrinsic::Sub.call(&[
                    traversed_args[0].to_owned(),
                    traversed_args[1].to_owned(),
                ])?))
            }
        }

        b @ (Intrinsic::Add
        | Intrinsic::Sub
        | Intrinsic::Mul
        | Intrinsic::Exp
        | Intrinsic::Neg
        | Intrinsic::Inv
        | Intrinsic::Shift) => Ok(Some(b.call(&traversed_args)?)),
    }
}

fn apply_function(
    f: &Function,
    args: Vec<Node>,
    ctx: &mut Rc<RefCell<SymbolTable>>,
    settings: &CompileSettings,
) -> Result<Option<Node>> {
    match &f.class {
        FunctionClass::UserDefined(d) => apply_defined(d, &f.handle, args, ctx, settings),
        FunctionClass::Intrinsic(i) => apply_intrinsic(i, args, ctx, settings),
        FunctionClass::Builtin(b) => apply_builtin(b, args, ctx, settings),
        _ => unreachable!(),
    }
}

fn apply(
    f: &Function,
    args: &[AstNode],
    ctx: &mut Rc<RefCell<SymbolTable>>,
    settings: &CompileSettings,
) -> Result<Option<Node>> {
    match f.class {
        FunctionClass::Form(sf) => apply_form(sf, args, ctx, settings),
        FunctionClass::Intrinsic(_) | FunctionClass::UserDefined(_) | FunctionClass::Builtin(_) => {
            let mut traversed_args = vec![];
            for arg in args.iter() {
                let traversed = reduce(arg, ctx, settings)?;
                if let Some(traversed) = traversed {
                    traversed_args.push(traversed);
                }
            }

            apply_function(f, traversed_args, ctx, settings)
        }
        _ => unreachable!(),
    }
}

pub fn reduce(
    e: &AstNode,
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

                apply(&func, &args[1..], ctx, settings)
            } else {
                Err(anyhow!("not a function: `{:?}`", args[0])).with_context(|| make_ast_error(e))
            }
        }

        Token::DefColumn {
            name,
            t: _,
            kind: k,
            ..
        } => match k {
            Kind::Composite(e) => {
                let n = reduce(e, ctx, settings)?.unwrap();
                ctx.borrow_mut().edit_symbol(name, &|x| {
                    if let Expression::Column(_, kind, _) = x {
                        *kind = Kind::Composite(Box::new(n.clone()))
                    }
                })?;
                Ok(None)
            }
            Kind::Interleaved(froms, _) => {
                let from_handles = froms
                    .iter()
                    .map(|f| match reduce(f, ctx, settings)?.unwrap().e() {
                        Expression::Column(h, ..) => Ok(h.to_owned()),
                        x => Err(anyhow!("expected column, found {:?}", x)),
                    })
                    .collect::<Result<Vec<_>>>()
                    .with_context(|| anyhow!("while defining {}", name))?;

                ctx.borrow_mut().edit_symbol(name, &|x| {
                    if let Expression::Column(_, kind, _) = x {
                        *kind = Kind::Interleaved(vec![], Some(from_handles.to_vec()))
                    }
                })?;
                Ok(None)
            }
            _ => Ok(None),
        },
        Token::DefColumns(_)
        | Token::DefConstraint { .. }
        | Token::DefArrayColumn { .. }
        | Token::DefModule(_)
        | Token::DefAliases(_)
        | Token::DefAlias(..)
        | Token::DefunAlias(..)
        | Token::DefConsts(..)
        | Token::Defun { .. }
        | Token::Defpurefun { .. }
        | Token::DefPermutation { .. }
        | Token::DefPlookup { .. }
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
        Token::DefConstraint {
            name,
            domain,
            guard,
            body: expr,
        } => {
            let handle = Handle::new(&ctx.borrow().name, name);
            Ok(Some(Constraint::Vanishes {
                handle,
                domain: domain.to_owned(),
                expr: Box::new({
                    let expr =
                        reduce(expr, ctx, settings)?.unwrap_or_else(|| Expression::Void.into());
                    if let Some(guard) = guard {
                        let guard_expr = reduce(guard, ctx, settings)?
                            .with_context(|| anyhow!("guard `{:?}` is empty", guard))?;
                        Intrinsic::IfNotZero.call(&[guard_expr, expr])?
                    } else {
                        expr
                    }
                }),
            }))
        }
        Token::DefPlookup {
            name,
            including: parent,
            included: child,
        } => {
            let handle = Handle::new(&ctx.borrow().name, name);
            let parents = parent
                .iter()
                .map(|e| reduce(e, ctx, settings).map(Option::unwrap))
                .collect::<Result<Vec<_>>>()?;
            let children = child
                .iter()
                .map(|e| reduce(e, ctx, settings).map(Option::unwrap))
                .collect::<Result<Vec<_>>>()?;
            if parents.len() != children.len() {
                bail!(
                    "in {}, parents and children have different lengths: {} and {}",
                    name.red(),
                    parents.len(),
                    children.len()
                )
            } else {
                Ok(Some(Constraint::Plookup {
                    handle,
                    including: parents,
                    included: children,
                }))
            }
        }
        Token::DefInrange(e, range) => {
            let handle = Handle::new(
                &ctx.borrow().name,
                names::Generator::default().next().unwrap(),
            );
            Ok(Some(Constraint::InRange {
                handle,
                exp: reduce(e, ctx, settings)?.unwrap(),
                max: Fr::from_str(&range.to_string())
                    .ok_or_else(|| anyhow!("`{range}` is not representable in Fr"))?,
            }))
        }
        Token::DefColumns(columns) => {
            for c in columns {
                reduce(c, ctx, settings)?;
            }
            Ok(None)
        }
        Token::DefModule(name) => {
            *ctx = SymbolTable::derived(root_ctx, name, name, false, true);
            Ok(None)
        }
        Token::Value(_) | Token::Symbol(_) | Token::List(_) | Token::Range(_) => {
            bail!("Unexpected top-level form: {:?}", e)
        }
        Token::Defun { .. }
        | Token::Defpurefun { .. }
        | Token::DefAliases(_)
        | Token::DefunAlias(..)
        | Token::DefConsts(..) => Ok(None),
        Token::DefPermutation { from, to } => {
            // We look up the columns involved in the permutation just to ensure that they
            // are marked as "used" in the symbol table
            from.iter()
                .map(|f| ctx.borrow_mut().resolve_symbol(f))
                .collect::<Result<Vec<_>>>()
                .with_context(|| anyhow!("while defining permutation"))?;
            to.iter()
                .map(|f| ctx.borrow_mut().resolve_symbol(f))
                .collect::<Result<Vec<_>>>()
                .with_context(|| anyhow!("while defining permutation"))?;

            Ok(Some(Constraint::Permutation {
                handle: Handle::new(
                    &ctx.borrow().name,
                    names::Generator::default().next().unwrap(),
                ),
                from: from
                    .iter()
                    .map(|f| Handle::new(&ctx.borrow().name, f))
                    .collect::<Vec<_>>(),
                to: to
                    .iter()
                    .map(|f| Handle::new(&ctx.borrow().name, f))
                    .collect::<Vec<_>>(),
            }))
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
