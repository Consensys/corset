use anyhow::*;
use cached::Cached;
use itertools::Itertools;
use log::*;
use logging_timer::time;
use num_bigint::BigInt;
use num_traits::cast::ToPrimitive;
use num_traits::{One, Zero};
use owo_colors::OwoColorize;
use serde::{Deserialize, Serialize};
use std::sync::OnceLock;

use std::collections::{BTreeMap, HashMap, HashSet};
use std::fmt::Debug;
use std::io::Write;

use std::sync::atomic::AtomicUsize;

use super::node::ColumnRef;
use super::parser::{Ast, AstNode, Token};
use super::tables::{ComputationTable, Scope};
use super::{common::*, CompileSettings, Conditioning, Expression, Magma, Node, Type};
use crate::column::{Column, ColumnSet, Computation, RegisterID, Value, ValueBacking};
use crate::dag::ComputationDag;
use crate::errors::{self, CompileError, RuntimeError};
use crate::pretty::Pretty;
use crate::structs::Handle;
use crate::utils::hash_strings;

static COUNTER: OnceLock<AtomicUsize> = OnceLock::new();

fn uniquify(n: String) -> String {
    format!(
        "{}-{}",
        n,
        COUNTER
            .get_or_init(|| AtomicUsize::new(0))
            .fetch_add(1, std::sync::atomic::Ordering::Relaxed),
    )
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub enum Constraint {
    Vanishes {
        handle: Handle,
        domain: Option<Domain<isize>>,
        expr: Box<Node>,
    },
    Lookup {
        handle: Handle,
        including: Vec<Node>,
        included: Vec<Node>,
    },
    Permutation {
        handle: Handle,
        from: Vec<ColumnRef>,
        to: Vec<ColumnRef>,
    },
    InRange {
        handle: Handle,
        exp: Node,
        max: Value,
    },
    // Ensures that 1 = reference × invert
    Normalization {
        handle: Handle,
        reference: Node,
        inverted: ColumnRef,
    },
}
impl Constraint {
    pub fn name(&self) -> String {
        match self {
            Constraint::Vanishes { handle, .. } => handle.to_string(),
            Constraint::Lookup { handle, .. } => handle.to_string(),
            Constraint::Permutation { handle, .. } => handle.to_string(),
            Constraint::InRange { handle, .. } => handle.to_string(),
            Constraint::Normalization { handle, .. } => handle.to_string(),
        }
    }

    pub fn add_id_to_handles(&mut self, set_id: &dyn Fn(&mut ColumnRef)) {
        match self {
            Constraint::Vanishes { expr, .. } => expr.add_id_to_handles(set_id),
            Constraint::Lookup {
                including: xs,
                included: ys,
                ..
            } => xs
                .iter_mut()
                .chain(ys.iter_mut())
                .for_each(|e| e.add_id_to_handles(set_id)),
            Constraint::Permutation {
                from: hs1, to: hs2, ..
            } => hs1.iter_mut().chain(hs2.iter_mut()).for_each(set_id),
            Constraint::InRange { exp, .. } => exp.add_id_to_handles(set_id),
            Constraint::Normalization {
                reference,
                inverted,
                ..
            } => {
                reference.add_id_to_handles(set_id);
                set_id(inverted);
            }
        }
    }

    pub(crate) fn size(&self) -> usize {
        match self {
            Constraint::Vanishes { expr, .. } => expr.size(),
            Constraint::Lookup { .. } => 1,
            Constraint::Permutation { .. } => 1,
            Constraint::InRange { .. } => 1,
            Constraint::Normalization { .. } => 1,
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
impl EvalSettings {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn wrap(self, w: bool) -> Self {
        Self { wrap: w, ..self }
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

/// A user-defined function is defined by its specializations, of which
/// (hopefully) one must fit with the provided argument to validate the call.
#[derive(Debug, Clone)]
pub struct Defined {
    pub specializations: Vec<Specialization>,
}
impl Defined {
    pub(crate) fn add_specialization(&mut self, other: &Self) -> Result<()> {
        assert!(other.specializations.len() == 1);
        let new_specialization = &other.specializations[0];

        if self.specializations.iter().any(|current_specialization| {
            current_specialization.in_types == new_specialization.in_types
        }) {
            // TODO: better error
            bail!("specialization {} already defined", new_specialization)
        } else {
            self.specializations.push(new_specialization.clone());
        }

        Ok(())
    }

    pub(crate) fn get_specialization(&self, args_t: &[Type]) -> Result<&Specialization> {
        for s in self.specializations.iter() {
            if crate::compiler::compatible_with(&s.in_types, args_t) {
                return Ok(s);
            }
        }
        let mut msg = "available specializations:".to_string();
        for s in self.specializations.iter() {
            let (expected_str, found_str) =
                errors::compiler::type_comparison_message(&s.in_types, args_t);
            msg += &format!(
                "\nexpected {} mismatches with found {}",
                expected_str, found_str
            );
        }
        bail!(msg)
    }
}
// User-defined function do not need to implement [`FunctionVerifier`], because
// their lookup would fail in any case if it is incompatible with the given
// arguments
/// A Specialization defines one of the implementations of a (polymorphic) function.
/// A non-polymorphic function will have one and only one Specialization.
#[derive(Debug, Clone)]
pub struct Specialization {
    // whether the function can escape its symbol table and access global
    // symbols -- other than constants, which are always available
    pub pure: bool,
    // the names of the function arguments
    pub args: Vec<String>,
    // the types of the function arguments, which must uniquely identify an
    // (polymorphic) implementation of a function
    pub in_types: Vec<Type>,
    // the return type of this specialization of the function
    pub out_type: Option<Type>,
    // the body type of this specialization of the function
    pub body: AstNode,
    // if set, then suppress warnings if there is a mismatch between the defined
    // & found return types at a callsite of this specialization
    pub force: bool,
}
impl std::fmt::Display for Specialization {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.pure {
            write!(f, "(pure) ",)?;
        }

        for t in self.in_types.iter() {
            write!(f, "{} ", t)?;
        }

        if let Some(t) = self.out_type {
            write!(f, "-> {}", t)
        } else {
            write!(f, "-> ?")
        }
    }
}

impl FuncVerifier<Node> for Intrinsic {
    fn arity(&self) -> Arity {
        match self {
            Intrinsic::Add => Arity::AtLeast(1),
            Intrinsic::Sub => Arity::AtLeast(1),
            Intrinsic::Mul => Arity::AtLeast(1),
            Intrinsic::VectorAdd => Arity::AtLeast(1),
            Intrinsic::VectorSub => Arity::AtLeast(1),
            Intrinsic::VectorMul => Arity::AtLeast(1),
            Intrinsic::Exp => Arity::Dyadic,
            Intrinsic::Neg => Arity::Monadic,
            Intrinsic::Inv => Arity::Monadic,
            Intrinsic::Normalize => Arity::Monadic,
            Intrinsic::Begin => Arity::AtLeast(1),
            Intrinsic::IfZero | Intrinsic::IfNotZero => Arity::Between(2, 3),
        }
    }
    fn validate_types(&self, args: &[Node]) -> Result<()> {
        let args_t = args.iter().map(|a| a.t()).collect::<Vec<_>>();

        // Intrinsic-specific checks
        match self {
            Intrinsic::Add
            | Intrinsic::Sub
            | Intrinsic::Mul
            | Intrinsic::VectorAdd
            | Intrinsic::VectorSub
            | Intrinsic::VectorMul => {}
            Intrinsic::Begin => {
                // TODO: maybe?
                // if args_t
                //     .iter()
                //     .any(|t| args_t.get(0).map(|tt| tt.c() != t.c()).unwrap_or(false))
                // {
                //     bail!(
                //         "can not mix multiple conditionings in if branches: {}",
                //         args_t.iter().skip(1).map(|t| t.c().to_string()).join(" ")
                //     )
                // }
            }
            Intrinsic::IfZero => {
                if !args_t[0].is_conditioned() {
                    bail!(CompileError::ConditioningError(self.to_string(), args_t[0]))
                }

                // TODO: maybe?
                // if args_t
                //     .iter()
                //     .skip(1)
                //     .any(|t| args_t.get(1).map(|tt| tt.c() != t.c()).unwrap_or(false))
                // {
                //     bail!(
                //         "can not mix multiple conditionings: {}",
                //         args_t.iter().map(|t| t.to_string()).join(" ")
                //     )
                // }
            }
            _ => {}
        }

        // The typing of functions is represented as a list of list.
        //
        // The elements of the first-level list represent the acceptable typing
        // for the arguments of the function, in their order.
        //
        // Each nested second-level list represent the acceptable typings for
        // the argument they type.
        //
        // The first-level list last element is cycled as many times is needed
        // to validate all the arguments. Therefore, for function taking
        // homogeneous arguments (e.g. Add), a single second-level list is
        // enough.
        let expected_t: &[&[Type]] = match self {
            Intrinsic::Add
            | Intrinsic::Sub
            | Intrinsic::Mul
            | Intrinsic::VectorAdd
            | Intrinsic::VectorSub
            | Intrinsic::VectorMul => &[&[Type::Any(Magma::ANY)]],
            Intrinsic::Exp => &[&[Type::Any(Magma::ANY)], &[Type::Scalar(Magma::ANY)]],
            Intrinsic::Neg => &[&[Type::Scalar(Magma::ANY), Type::Column(Magma::ANY)]],
            Intrinsic::Inv | Intrinsic::Normalize => &[&[Type::Any(Magma::ANY)]],
            Intrinsic::IfZero | Intrinsic::IfNotZero => &[
                // condition type
                &[Type::Any(Magma::ANY)],
                // then/else arms typ
                &[Type::Any(Magma::ANY)],
            ],
            Intrinsic::Begin => &[&[Type::Any(Magma::ANY)]],
        };

        if super::compatible_with_repeating(expected_t, &args_t) {
            Ok(())
        } else {
            bail!(CompileError::TypeError(
                self.to_string(),
                expected_t,
                args_t
            ))
        }
    }
}

pub type PerspectiveTable = HashMap<String, HashMap<String, Node>>;

pub const ADDER_MODULE: &str = "#adder";
pub const MULER_MODULE: &str = "#muler";

#[derive(Default, Debug, Serialize, Deserialize)]
pub struct ConstraintSet {
    pub columns: ColumnSet,
    pub constraints: Vec<Constraint>,
    pub constants: HashMap<Handle, BigInt>,
    pub computations: ComputationTable,
    pub perspectives: PerspectiveTable,
    pub transformations: u32,
    pub auto_constraints: u32,
}
impl ConstraintSet {
    pub fn new(
        columns: ColumnSet,
        constraints: Vec<Constraint>,
        constants: HashMap<Handle, BigInt>,
        computations: ComputationTable,
        perspectives: PerspectiveTable,
    ) -> Result<Self> {
        let mut r = ConstraintSet {
            constraints,
            columns,
            constants,
            computations,
            perspectives,
            transformations: 0,
            auto_constraints: 0,
        };
        r.convert_refs_to_ids()?;
        r.allocate_registers();
        r.fill_perspectives()?;
        r.compute_spillings();
        r.validate()?;
        Ok(r)
    }

    fn allocate_registers(&mut self) {
        #[derive(Default, Debug)]
        struct ColumnPool {
            root: Vec<ColumnRef>,
            // It is important for the allocation to be deterministic, so that
            // multiple compilations of the same constraint system result in the
            // same allocation; hence the use of BTreeMap instead of HashMap.
            // Module -> SizeFactor -> Type -> Perspective -> Columns names
            perspectives: BTreeMap<
                String,
                BTreeMap<usize, BTreeMap<Magma, BTreeMap<String, Vec<ColumnRef>>>>,
            >,
        }

        let mut pool: ColumnPool = Default::default();
        for (h, col) in self
            .columns
            .iter()
            .sorted_by(|a, b| a.1.handle.cmp(&b.1.handle))
        {
            if col.kind == Kind::Commitment {
                match &col.handle.perspective {
                    Some(name) => {
                        let module = col.handle.module.to_string();
                        let magma = col.t;
                        pool.perspectives
                            .entry(module)
                            .or_default()
                            .entry(self.length_multiplier(&col.handle.clone().into()))
                            .or_default()
                            .entry(magma)
                            .or_default()
                            .entry(name.to_string())
                            .or_default()
                            .push(h.to_owned());
                    }
                    None => {
                        pool.root.push(h.to_owned());
                    }
                }
            }
        }

        // module-global columns all have their own register
        for c in pool.root {
            let reg = self.columns.new_register(
                self.handle(&c).to_owned(),
                self.columns.column(&c).unwrap().t,
            );
            self.columns.assign_register(&c, reg).unwrap();
        }

        // perspective columns are grouped together if they are of the same type & size factor
        for (module, sizes) in pool.perspectives.iter() {
            for (size, magmas) in sizes.iter() {
                for (magma, sets) in magmas.iter() {
                    trace!(
                        "allocating registers for {}/×{}/{:?}: {:?}",
                        module,
                        size,
                        magma,
                        sets.values().collect_vec()
                    );
                    let longest = sets.values().map(|cols| cols.len()).max().unwrap();
                    for i in 0..longest {
                        let names = sets
                            .values()
                            .filter_map(|v| v.get(i))
                            .map(|r| self.handle(r).name.to_owned())
                            .join("_xor_");
                        let reg = self
                            .columns
                            .new_register(Handle::new(module, names), *magma);
                        for cols in sets.values() {
                            if let Some(col_id) = cols.get(i) {
                                self.columns.assign_register(col_id, reg).unwrap();
                            }
                        }
                    }
                }
            }
        }

        // Finally, computed columns registers are allocated depending on their
        // type.

        // columns involved in computations must get their register assigned
        // following their dependency tree, as register IDs are required to list
        // computation sources
        let dependent_columns = ComputationDag::from_computations(self.computations.iter());

        // let todos = jobs.job_slices();
        for slice in dependent_columns.job_slices() {
            for c in slice
                .iter()
                .filter_map(|h| self.computations.computation_idx_for(h))
                .collect::<HashSet<_>>()
                .into_iter()
                .map(|i| self.computations.get(i).unwrap().to_owned())
            {
                match c {
                    Computation::Interleaved { target, .. }
                    | Computation::CyclicFrom { target, .. }
                    | Computation::Composite { target, .. } => {
                        let col = self.columns.column(&target).unwrap();
                        let reg = self.columns.new_register(col.handle.clone(), col.t);
                        self.columns.assign_register(&target, reg).unwrap();
                    }
                    Computation::Sorted { froms, tos, .. } => {
                        let mut reg_translation = HashMap::<RegisterID, RegisterID>::new();
                        for (f, t) in froms.iter().zip(tos.iter()) {
                            let reg = reg_translation
                                .entry(self.columns.column(f).unwrap().register.unwrap())
                                .or_insert_with(|| {
                                    let col = self.columns.column(t).unwrap();
                                    self.columns.new_register(col.handle.clone(), col.t)
                                });
                            self.columns.assign_register(t, *reg).unwrap();
                        }
                    }
                    Computation::SortingConstraints {
                        ats,
                        eq,
                        delta,
                        delta_bytes,
                        ..
                    } => {
                        for r in std::iter::once(&eq)
                            .chain(std::iter::once(&delta))
                            .chain(ats.iter())
                            .chain(delta_bytes.iter())
                        {
                            let col = self.columns.column(r).unwrap();
                            let reg = self.columns.new_register(col.handle.clone(), col.t);
                            self.columns.assign_register(r, reg).unwrap();
                        }
                    }
                    _ => unreachable!(),
                }
            }
        }
    }

    fn fill_perspectives(&mut self) -> Result<()> {
        let dependent_computations = ComputationDag::from_computations(self.computations.iter());
        for slice in dependent_computations.job_slices() {
            trace!("Processing computation slice {:?}", slice);
            for i in slice
                .iter()
                .filter_map(|h| self.computations.computation_idx_for(h))
                .collect::<HashSet<_>>()
                .into_iter()
            {
                match self.computations.get_mut(i).unwrap().clone() {
                    Computation::Interleaved { target, froms } => {
                        if let Some(perspective) = self.columns.perspective_of(froms.iter())? {
                            let from_handle = self.columns.column(&froms[0])?.handle.to_owned();
                            let module = from_handle.module.to_owned();
                            if let Expression::Column { handle, .. } =
                                self.get_perspective(&module, &perspective)?.e().clone()
                            {
                                let srt_guard_col_handle = Handle::new(
                                    &module,
                                    format!(
                                        "{}%intrld",
                                        self.columns.column(&handle).unwrap().handle.name,
                                    ),
                                );
                                if let Some(srt_guard_id) =
                                    self.columns.maybe_insert_column_and_register(
                                        Column::builder()
                                            .kind(Kind::Computed)
                                            .handle(srt_guard_col_handle)
                                            .build(),
                                    )
                                {
                                    let srt_guard = Node::column()
                                        .handle(srt_guard_id.clone())
                                        .kind(Kind::Computed)
                                        .build();
                                    let srt_guard_name = format!("{}-intrld", perspective);
                                    self.computations.insert(
                                        &srt_guard_id,
                                        Computation::Interleaved {
                                            target: srt_guard_id.to_owned(),
                                            froms: vec![handle; froms.len()],
                                        },
                                    )?;
                                    self.insert_perspective(&module, &srt_guard_name, srt_guard)?;
                                    self.columns.set_perspective(&target, &srt_guard_name)?;
                                }
                            } else {
                                bail!(
                                    "perspective {} is not defined by a column",
                                    perspective.yellow().bold()
                                )
                            }
                        }
                    }
                    Computation::Sorted {
                        mut froms,
                        mut tos,
                        mut signs,
                    } => {
                        if let Some(perspective) = froms
                            .iter()
                            .find_map(|f| self.columns.perspective(f).unwrap())
                        {
                            let suffix =
                                hash_strings(froms.iter().map(|f| f.as_handle().name.clone()));
                            let module = self.columns.column(&froms[0])?.handle.module.to_owned();
                            if let Expression::Column { handle, .. } =
                                self.get_perspective(&module, &perspective)?.e().clone()
                            {
                                let srt_guard_col_handle = Handle::new(
                                    &module,
                                    format!(
                                        "{}-for-srt-{suffix}",
                                        self.columns.column(&handle).unwrap().handle.name,
                                    ),
                                );

                                let srt_guard_id = self
                                    .columns
                                    .insert_column_and_register(
                                        Column::builder()
                                            .kind(Kind::Computed)
                                            .handle(srt_guard_col_handle.clone())
                                            .build(),
                                    )
                                    .unwrap();
                                let srt_guard = Node::column()
                                    .handle(srt_guard_id.clone())
                                    .kind(Kind::Computed)
                                    .build();
                                self.insert_perspective(
                                    &module,
                                    &srt_guard_col_handle.name,
                                    srt_guard,
                                )?;

                                for to in tos.iter() {
                                    self.columns
                                        .force_perspective(to, &srt_guard_col_handle.name);
                                }
                                froms.insert(0, handle.clone());
                                tos.insert(0, srt_guard_id.clone());
                                signs.insert(0, true);
                                self.computations.add_dependency(srt_guard_id, i)?;
                            } else {
                                unimplemented!("non-column perspectives not yet supported");
                            };
                        };
                    }
                    _ => {}
                }
            }
        }

        Ok(())
    }

    // do not pollute compilation output of the binary
    #[allow(dead_code)]
    pub fn from_ptr<'a>(ptr: *const ConstraintSet) -> &'a Self {
        assert!(!ptr.is_null());
        unsafe { &*ptr }
    }

    #[allow(dead_code)]
    pub fn mut_from_ptr<'a>(ptr: *mut ConstraintSet) -> &'a mut Self {
        assert!(!ptr.is_null());
        unsafe { &mut *ptr }
    }

    pub(crate) fn handle(&self, h: &ColumnRef) -> &Handle {
        &self.columns.column(h).unwrap().handle
    }

    pub(crate) fn insert_constraint(&mut self, c: Constraint) {
        match &c {
            Constraint::Vanishes { expr, .. } => {
                for c in expr.dependencies().into_iter() {
                    self.columns.get_col_mut(&c).unwrap().used = true
                }
            }
            Constraint::Lookup {
                including,
                included,
                ..
            } => {
                for c in including
                    .iter()
                    .flat_map(Node::dependencies)
                    .chain(included.iter().flat_map(Node::dependencies))
                {
                    self.columns.mark_used(&c).unwrap();
                }
            }
            Constraint::Permutation { from, to, .. } => {
                for c in from.iter().chain(to.iter()) {
                    self.columns.mark_used(c).unwrap();
                }
            }
            Constraint::InRange { exp, .. } => {
                for c in exp.dependencies().into_iter() {
                    self.columns.mark_used(&c).unwrap();
                }
            }
            Constraint::Normalization { .. } => {}
        }
        self.constraints.push(c);
    }

    pub(crate) fn insert_perspective(
        &mut self,
        module: &str,
        name: &str,
        guard: Node,
    ) -> Result<bool> {
        let r = self
            .perspectives
            .get_mut(module)
            .ok_or_else(|| anyhow!("unknown module"))?
            .insert(name.into(), guard)
            .is_some();
        Ok(r)
    }

    pub fn dependencies_len(&self, expr: &Node, with_padding: bool) -> Result<Option<usize>> {
        let cols_lens = expr
            .dependencies()
            .into_iter()
            .map(|handle| {
                if with_padding {
                    self.columns.padded_len(&handle)
                } else {
                    self.columns.len(&handle)
                }
            })
            .collect::<Vec<_>>();
        if cols_lens.is_empty() {
            return Ok(None);
        }

        if cols_lens.iter().all(|l| l.is_none()) {
            return Ok(None);
        }

        if !cols_lens
            .iter()
            .filter(|l| l.is_some())
            .all(|&l| l.unwrap_or_default() == cols_lens[0].unwrap_or_default())
        {
            bail!(
                "all columns in {} are not of the same length:\n{}",
                expr.pretty(),
                expr.dependencies()
                    .iter()
                    .map(|handle| format!(
                        "\t{}: {}",
                        handle,
                        self.columns
                            .padded_len(handle)
                            .map(|x| x.to_string())
                            .unwrap_or_else(|| "nil".into()),
                    ))
                    .collect::<Vec<_>>()
                    .join("\n")
            )
        }
        let l = cols_lens[0].unwrap_or(0);
        if l == 0 {
            bail!("empty trace, aborting")
        } else {
            Ok(Some(l))
        }
    }

    pub(crate) fn get_perspective(&self, module: &str, name: &str) -> Result<&Node> {
        self.perspectives
            .get(module)
            .ok_or_else(|| anyhow!("unknown module"))?
            .get(name)
            .ok_or_else(|| anyhow!("perspective not found"))
    }

    pub fn convert_refs_to_ids(&mut self) -> Result<()> {
        let convert_to_id = |h: &mut ColumnRef| {
            let id = self.columns.id_of(h);
            h.set_id(id);
        };
        self.constraints
            .iter_mut()
            .for_each(|x| x.add_id_to_handles(&convert_to_id));

        self.computations.dependencies = self
            .computations
            .dependencies
            .iter()
            .map(|(k, v)| {
                let mut k = k.clone();
                convert_to_id(&mut k);
                (k, *v)
            })
            .collect();

        for c in self.computations.iter_mut() {
            match c {
                Computation::Composite { target, exp } => {
                    convert_to_id(target);
                    exp.add_id_to_handles(&convert_to_id);
                }
                Computation::Interleaved { target, froms } => std::iter::once(target)
                    .chain(froms.iter_mut())
                    .for_each(convert_to_id),
                Computation::Sorted { froms, tos, .. } => froms
                    .iter_mut()
                    .chain(tos.iter_mut())
                    .for_each(convert_to_id),
                Computation::CyclicFrom { target, froms, .. } => std::iter::once(target)
                    .chain(froms.iter_mut())
                    .for_each(convert_to_id),
                Computation::SortingConstraints { .. } => {
                    // These computations are built with IDs from the very start
                }
                Computation::ExoOperation {
                    sources, target, ..
                } => {
                    for source in sources.iter_mut() {
                        source.add_id_to_handles(&convert_to_id);
                    }
                    convert_to_id(target);
                }
                Computation::ExoConstant { target, .. } => {
                    convert_to_id(target);
                }
            }
        }

        for p in self.perspectives.values_mut().flat_map(|k| k.values_mut()) {
            p.add_id_to_handles(&convert_to_id)
        }

        Ok(())
    }

    pub fn effective_len_for(&self, m: &str) -> Option<isize> {
        self.columns.effective_len.get(m).copied()
    }

    pub fn effective_len_or_set(&mut self, m: &str, x: isize) -> isize {
        *self.columns.effective_len.entry(m.to_string()).or_insert(x)
    }

    pub fn spilling_for_column(&self, h: &ColumnRef) -> Option<isize> {
        let module = if h.is_handle() {
            &h.as_handle().module
        } else {
            &self.columns.column(h).ok()?.handle.module
        };
        self.spilling_of(module)
    }

    pub(crate) fn compute_spilling(&mut self, m: &str) -> isize {
        let spilling = self
            .computations
            .iter()
            .filter_map(|c| match c {
                Computation::Composite { target, exp } => {
                    if target.as_handle().module == m {
                        Some(exp.past_spill().abs())
                    } else {
                        None
                    }
                }
                _ => None,
            })
            .chain(self.constraints.iter().filter_map(|c| match c {
                Constraint::Vanishes { handle, expr, .. } => {
                    if handle.module == m {
                        Some(expr.past_spill().abs())
                    } else {
                        None
                    }
                }
                _ => None,
            }))
            .max()
            .unwrap_or(0);
        self.columns.spilling.insert(m.to_owned(), spilling);
        spilling
    }

    pub fn iter_len(&self, module: &str) -> usize {
        self.effective_len_for(module)
            // If the module is empty, use its spilling
            .or_else(|| self.spilling_of(module))
            .unwrap() as usize
    }

    pub fn spilling_of(&self, m: &str) -> Option<isize> {
        self.columns.spilling.get(m).cloned()
    }

    fn compute_spillings(&mut self) {
        let all_modules = self.columns.modules();
        for m in all_modules {
            let spilling = self.compute_spilling(&m);
            self.columns.spilling.insert(m, spilling);
        }
    }

    pub(crate) fn module_of_expr(&self, e: &Node) -> Option<String> {
        self.columns.module_for(e.dependencies())
    }

    pub(crate) fn module_of_exprs(&self, es: &[Node]) -> Option<String> {
        es.iter().find_map(|e| self.module_of_expr(e))
    }

    pub fn length_multiplier(&self, h: &ColumnRef) -> usize {
        self.computations
            .computation_for(h)
            .map(|comp| match comp {
                Computation::Composite { exp, .. } => exp
                    .dependencies()
                    .iter()
                    .next()
                    .map(|d| self.length_multiplier(d))
                    .unwrap_or(1),
                Computation::Interleaved { froms, .. } => {
                    self.length_multiplier(&froms[0]) * froms.len()
                }
                Computation::Sorted { froms, .. } | Computation::CyclicFrom { froms, .. } => {
                    self.length_multiplier(&froms[0])
                }
                Computation::SortingConstraints { .. } => 1,
                Computation::ExoOperation { sources, .. } => sources
                    .iter()
                    .flat_map(|s| s.dependencies())
                    .next()
                    .map(|c| self.length_multiplier(&c))
                    .unwrap_or(1),
                Computation::ExoConstant { .. } => 1,
            })
            .unwrap_or(1)
            * self
                .columns
                .column(h)
                .unwrap()
                .intrinsic_size_factor
                .unwrap_or(1)
    }

    #[time("info", "Exporting expanded trace")]
    pub fn write(&mut self, out: &mut impl Write) -> Result<()> {
        let mut cache = cached::SizedCache::with_size(200000); // ~1.60MB cache

        out.write_all("{\"columns\":{\n".as_bytes())?;

        for (i, module) in self.columns.modules().into_iter().enumerate() {
            debug!("Exporting {}", &module);
            if i > 0 {
                out.write_all(b",")?;
            }

            let mut current_col = self
                .columns
                .all()
                .into_iter()
                .map(|h| (h.clone(), self.columns.column(&h).unwrap()))
                .filter(|(_, c)| c.handle.module == module)
                .peekable();
            let empty_backing: ValueBacking = ValueBacking::default();
            while let Some((r, column)) = current_col.next() {
                let handle = &column.handle;
                trace!("Writing {}", handle);
                let backing = self.columns.backing(&r).unwrap_or(&empty_backing);
                let padding: Value = if let Some(v) = column.padding_value.as_ref() {
                    v.clone()
                } else {
                    backing.get(0, false, &self.columns).unwrap_or_else(|| {
                        self.computations
                            .computation_for(&r)
                            .map(|c| match c {
                                Computation::Composite { exp, .. } => exp
                                    .eval(
                                        0,
                                        |_, _, _| Some(Value::zero()),
                                        &mut None,
                                        &EvalSettings::default(),
                                    )
                                    .unwrap_or_else(Value::zero),
                                Computation::Interleaved { .. } => Value::zero(),
                                Computation::Sorted { .. } => Value::zero(),
                                Computation::CyclicFrom { .. } => Value::zero(),
                                Computation::SortingConstraints { .. } => Value::zero(),
                                Computation::ExoOperation { .. } => Value::zero(), // TODO: FIXME:
                                Computation::ExoConstant { .. } => Value::zero(),  // TODO: FIXME:
                            })
                            .unwrap_or_else(Value::zero)
                    })
                };

                out.write_all(format!("\"{}\":{{\n", handle).as_bytes())?;
                out.write_all("\"values\":[".as_bytes())?;

                let mut value = backing.iter(&self.columns).peekable();
                while let Some(x) = value.next() {
                    out.write_all(
                        cache
                            .cache_get_or_set_with(x.to_owned(), || {
                                format!("\"0x0{}\"", x.to_string())
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

    pub fn validate(&self) -> Result<()> {
        //
        // Check that all ColumnRef are IDs
        //

        // Check the constraints
        for c in self.constraints.iter() {
            match c {
                Constraint::Vanishes { handle, expr, .. } => {
                    if expr.dependencies().into_iter().any(|r| !r.is_id()) {
                        bail!(errors::compiler::Error::ConstraintWithHandles(
                            handle.to_string()
                        ))
                    }
                }
                Constraint::Lookup {
                    handle,
                    including,
                    included,
                } => {
                    if including
                        .iter()
                        .flat_map(|i| i.dependencies())
                        .chain(included.iter().flat_map(|i| i.dependencies()))
                        .any(|r| !r.is_id())
                    {
                        bail!(errors::compiler::Error::ConstraintWithHandles(
                            handle.to_string()
                        ))
                    }
                }
                Constraint::Permutation {
                    handle, from, to, ..
                } => {
                    if from.iter().chain(to.iter()).any(|r| !r.is_id()) {
                        bail!(errors::compiler::Error::ConstraintWithHandles(
                            handle.to_string()
                        ))
                    }
                }
                Constraint::InRange { handle, exp, .. } => {
                    if exp.dependencies().into_iter().any(|r| !r.is_id()) {
                        bail!(errors::compiler::Error::ConstraintWithHandles(
                            handle.to_string()
                        ))
                    }
                }
                Constraint::Normalization {
                    handle,
                    reference,
                    inverted,
                } => {
                    if !(reference.dependencies().into_iter().any(|h| h.is_id())
                        && inverted.is_id())
                    {
                        bail!(errors::compiler::Error::ConstraintWithHandles(
                            handle.to_string()
                        ))
                    }
                }
            }
        }

        // Check the computations
        if self.computations.dependencies.keys().any(|r| !r.is_id()) {
            bail!(errors::compiler::Error::ComputationWithHandles(
                "dependencies".to_string()
            ))
        }
        for c in self.computations.iter() {
            match c {
                Computation::Composite { target, exp } => {
                    if !target.is_id() || exp.dependencies().into_iter().any(|r| !r.is_id()) {
                        bail!(errors::compiler::Error::ComputationWithHandles(
                            c.to_string(),
                        ))
                    }
                }
                Computation::Interleaved { target, froms }
                | Computation::CyclicFrom { target, froms, .. } => {
                    if !target.is_id() || froms.iter().any(|r| !r.is_id()) {
                        bail!(errors::compiler::Error::ComputationWithHandles(
                            c.to_string()
                        ))
                    }
                }
                Computation::Sorted { froms, tos, .. } => {
                    if tos.iter().any(|r| !r.is_id()) || froms.iter().any(|r| !r.is_id()) {
                        bail!(errors::compiler::Error::ComputationWithHandles(
                            c.to_string()
                        ))
                    }
                }
                Computation::SortingConstraints {
                    ats,
                    eq,
                    delta,
                    delta_bytes,
                    froms,
                    sorted,
                    ..
                } => {
                    if std::iter::once(eq)
                        .chain(sorted.iter())
                        .chain(ats.iter())
                        .chain(std::iter::once(delta))
                        .chain(delta_bytes.iter())
                        .chain(froms.iter())
                        .any(|r| !r.is_id())
                    {
                        bail!(errors::compiler::Error::ComputationWithHandles(
                            c.to_string()
                        ))
                    }
                }
                Computation::ExoOperation {
                    sources, target, ..
                } => {
                    if std::iter::once(target.clone())
                        .chain(sources.iter().flat_map(|s| s.dependencies()))
                        .any(|r| !r.is_id())
                    {
                        bail!(errors::compiler::Error::ComputationWithHandles(
                            c.to_string()
                        ))
                    }
                }
                Computation::ExoConstant { target, .. } => {
                    if !target.is_id() {
                        bail!(errors::compiler::Error::ComputationWithHandles(
                            target.to_string()
                        ))
                    }
                }
            }
        }

        // Check that no constraint mixes cardinalities
        for c in self.constraints.iter() {
            match c {
                Constraint::Vanishes {
                    handle,
                    domain: _,
                    expr,
                } => {
                    let mut sizes = expr.dependencies().into_iter();
                    if let Some(first) = sizes.next() {
                        let first_size = self.length_multiplier(&first);
                        for other in sizes {
                            let other_size = self.length_multiplier(&other);
                            if first_size != other_size {
                                bail!(
                                        "constraint {} mixes columns {} (×{}) and {} (×{}) of different size factors ",
                                        handle.pretty(), first.pretty(), first_size,
                                        other.pretty(), other_size,
                                    );
                            }
                        }
                    }
                }
                Constraint::Normalization { .. } => {}
                _ => {}
            }
        }

        //Check that all columns are assigned to a register
        for c in self.columns.iter_cols() {
            if c.register.is_none() {
                bail!("column {} has no register", c.handle.pretty())
            }
        }

        //
        // Check that computations are perspective-coherent
        //
        for c in self.computations.iter() {
            match c {
                Computation::Interleaved { froms, .. } => {
                    for f in froms.iter().skip(1) {
                        if self.columns.perspective(f).as_ref().unwrap()
                            != self.columns.perspective(&froms[0]).as_ref().unwrap()
                        {
                            bail!(
                                "columns {} and {} are in different perspectives",
                                self.handle(f).pretty(),
                                self.handle(&froms[0]).pretty()
                            )
                        }
                    }
                }
                Computation::Sorted { froms, tos, signs } => {
                    assert!(froms.len() == tos.len());
                    assert!(froms.len() == signs.len());
                }
                _ => {}
            }
        }
        Ok(())
    }
}

// Compared to a function, a form do not evaluate all of its arguments by default
fn apply_form(
    f: Form,
    args: &[AstNode],
    ctx: &mut Scope,
    settings: &CompileSettings,
) -> Result<Option<Node>> {
    f.validate_args(args)
        .with_context(|| anyhow!("evaluating call to {:?}", f))?;

    match f {
        Form::For => {
            if let (Token::Symbol(i_name), Token::Domain(is), body) =
                (&args[0].class, &args[1].class, &args[2])
            {
                let is = is.concretize(|n| {
                    crate::compiler::generator::reduce(n, &mut ctx.clone(), settings)
                        .transpose()
                        .unwrap()
                        .and_then(|r| r.pure_eval())
                        .and_then(|bi| bi.to_isize().ok_or_else(|| anyhow!("{} is not an i64", bi)))
                })?;
                let mut l = vec![];
                let mut t = Type::INFIMUM;
                for i in is.iter() {
                    let mut for_ctx = ctx.derive(&uniquify(format!("{}-for-{}", ctx.name(), i)))?;

                    for_ctx.insert_symbol(i_name, Expression::Const(Value::from(i)).into())?;

                    if let Some(r) = reduce(&body.clone(), &mut for_ctx, settings)? {
                        t = t.max(r.t());
                        l.push(r);
                    } else {
                        warn!("empty for loop body: {}", body.src.white().bold())
                    };
                }

                Ok(Some(Node::from(Expression::List(l)).with_type(t)))
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
        Form::Todo => {
            error!("TODO not yet implemented");
            Ok(None)
        }
        Form::Let => {
            let sub_ctx_name = uniquify(format!("{}-let", ctx.name()));
            let mut sub_ctx = ctx.derive(&sub_ctx_name)?;
            for pair in args[0].as_list().unwrap().iter() {
                let pair = pair.as_list().unwrap();
                let name = pair[0].as_symbol().unwrap();
                let value = reduce(&pair[1], &mut sub_ctx, settings)?.unwrap();
                sub_ctx.insert_symbol(name, value)?;
            }
            let body = reduce(&args[1], &mut sub_ctx, settings)?.unwrap();

            Ok(Some(body))
        }
        Form::Reduce => {
            let f_name = args[0].as_symbol().unwrap();
            let f = ctx.resolve_function(f_name)?;

            let mut body = reduce(&args[1], ctx, settings)?.unwrap();

            return match body.e_mut() {
                Expression::Column { .. }
                | Expression::Void
                | Expression::ArrayColumn { .. }
                | Expression::Funcall { .. }
                | Expression::Const(_) => panic!(),
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
                Expression::ExoColumn { .. } => todo!(),
            };
        }
    }
}

fn apply_defined(
    b: &Defined,
    h: &Handle,
    traversed_args: Vec<Node>,
    ctx: &mut Scope,
    settings: &CompileSettings,
) -> Result<Option<Node>> {
    let f_mangle = uniquify(format!("fn-{}", h));
    let b = b
        .get_specialization(
            traversed_args
                .iter()
                .map(|a| a.t())
                .collect::<Vec<_>>()
                .as_slice(),
        )
        .with_context(|| anyhow!("validating call to {}", h.pretty()))?;
    let mut f_ctx = ctx.derive(&f_mangle)?.closed(b.pure);
    for (i, f_arg) in b.args.iter().enumerate() {
        f_ctx.insert_symbol(f_arg, traversed_args[i].clone())?;
    }
    Ok(if let Some(r) = reduce(&b.body, &mut f_ctx, settings)? {
        let found_type = r.t();
        let final_type = if let Some(expected_type) = b.out_type {
            if found_type > expected_type {
                if b.force {
                    if !expected_type.is_conditioned() {
                        expected_type
                            .with_scale(found_type)
                            .force_with_conditioning_of(&found_type)
                    } else {
                        expected_type.with_scale(found_type)
                    }
                } else {
                    bail!(
                    "in call to {} with {}: inferred output type {} is incompatible with declared return type {}",
                    h.pretty(),
                    traversed_args.iter().map(|x| x.pretty()).join(" "),
                    found_type.yellow().bold(),
                    expected_type.blue().bold()
                )
                }
            } else {
                found_type.force_with_conditioning_of(&expected_type)
            }
        } else {
            r.t()
        };
        Some(r.with_type(final_type))
    } else {
        None
    })
}

fn apply_builtin(
    b: &Builtin,
    traversed_args: Vec<Node>,
    _ctx: &mut Scope,
    _settings: &CompileSettings,
) -> Result<Option<Node>> {
    b.validate_args(&traversed_args)?;

    match b {
        Builtin::Len => {
            if let Expression::ArrayColumn {
                handle: _,
                domain,
                base: _,
            } = traversed_args[0].e()
            {
                Ok(Some(Node::from_isize(domain.len().try_into().unwrap())))
            } else {
                bail!(RuntimeError::NotAnArray(traversed_args[0].e().clone()))
            }
        }
        Builtin::Shift => {
            let shift = traversed_args[1].pure_eval()?.to_i16().unwrap();
            Ok(Some(traversed_args.get(0).unwrap().clone().shift(shift)))
        }
        Builtin::NormFlat => {
            if traversed_args[0].is_exocolumn() {
                todo!("{}", traversed_args[0].pretty())
            } else {
                Ok(Some(traversed_args[0].clone()))
            }
        }
        Builtin::If => match traversed_args[0].t().c() {
            super::Conditioning::None => {
                bail!("{} is not a valid condition", traversed_args[0].pretty())
            }
            super::Conditioning::Boolean => Ok(Some(Intrinsic::IfNotZero.call(&traversed_args)?)),
            super::Conditioning::Loobean => Ok(Some(Intrinsic::IfZero.call(&traversed_args)?)),
        },
    }
}

fn apply_intrinsic(
    b: &Intrinsic,
    traversed_args: Vec<Node>,
    _settings: &CompileSettings,
) -> Result<Option<Node>> {
    b.validate_args(&traversed_args)?;
    let traversed_args_t = traversed_args.iter().map(|a| a.t()).collect::<Vec<_>>();
    match b {
        // Begin flattens & concatenate any list argument
        Intrinsic::Begin => Ok(Some(
            Node::from(Expression::List(traversed_args.into_iter().fold(
                vec![],
                |mut ax, mut e| match e.e_mut() {
                    Expression::List(ref mut es) => {
                        ax.append(es);
                        ax
                    }
                    _ => {
                        ax.push(e.to_owned());
                        ax
                    }
                },
            )))
            .with_type(super::max_type(&traversed_args_t)?),
        )),

        b @ Intrinsic::IfZero | b @ Intrinsic::IfNotZero => {
            let r = b.call(&traversed_args)?;
            if traversed_args[0].may_overflow() {
                let pretty = if let Some(d) = traversed_args[0].dbg() {
                    d.to_owned()
                } else {
                    traversed_args[0].to_string()
                };
                warn!("condition {} may overflow", pretty.bright_white().bold());
            }
            Ok(Some(r))
        }

        b @ (Intrinsic::Add
        | Intrinsic::Sub
        | Intrinsic::Mul
        | Intrinsic::VectorAdd
        | Intrinsic::VectorSub
        | Intrinsic::VectorMul
        | Intrinsic::Exp
        | Intrinsic::Neg
        | Intrinsic::Inv
        | Intrinsic::Normalize) => Ok(Some(b.call(&traversed_args)?)),
    }
}

fn apply_function(
    f: &Function,
    args: Vec<Node>,
    ctx: &mut Scope,
    settings: &CompileSettings,
) -> Result<Option<Node>> {
    match &f.class {
        FunctionClass::UserDefined(d) => apply_defined(d, &f.handle, args, ctx, settings),
        FunctionClass::Intrinsic(i) => apply_intrinsic(i, args, settings),
        FunctionClass::Builtin(b) => apply_builtin(b, args, ctx, settings),
        _ => unreachable!(),
    }
}

fn apply(
    f: &Function,
    args: &[AstNode],
    ctx: &mut Scope,
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

pub fn reduce(e: &AstNode, ctx: &mut Scope, settings: &CompileSettings) -> Result<Option<Node>> {
    match &e.class {
        Token::Keyword(_) | Token::Domain(_) => Ok(None),
        Token::Value(x) => Ok(Some(
            // We want the value to specifically be a BigInt here, as we may
            // have negative ones, e.g. as shift arguments.
            Node::from(Expression::Const(Value::big_int(x.clone()))).with_type(
                if *x >= Zero::zero() && *x <= One::one() {
                    Type::Scalar(Magma::binary())
                } else {
                    Type::Scalar(Magma::native())
                },
            ),
        )),
        Token::Symbol(name) => Ok(Some(
            ctx.resolve_symbol(name)
                .with_context(|| make_ast_error(e))?,
        )),
        Token::IndexedSymbol { name, index } => {
            let symbol = ctx.resolve_symbol(name)?;
            if let Expression::ArrayColumn {
                handle,
                domain,
                base,
            } = symbol.e()
            {
                let i = reduce(index, ctx, settings)?
                    .and_then(|n| n.pure_eval().ok())
                    .and_then(|b| b.to_usize())
                    .ok_or_else(|| anyhow!("{:?} is not a valid index", index))?;
                if domain.contains(i.try_into().unwrap()) {
                    Ok(Some(
                        Node::column()
                            .handle(handle.as_handle().ith(i))
                            .kind(Kind::Commitment)
                            .base(*base)
                            .t(symbol.t().m())
                            .build(),
                    ))
                } else {
                    bail!("tried to access {} at index {}", symbol.pretty().bold(), i)
                }
            } else {
                bail!(anyhow!(
                    "{} of type {} is not indexable",
                    name.red().bold(),
                    symbol.t()
                ))
            }
        }
        Token::List(args) => {
            if args.is_empty() {
                Ok(Some(Expression::List(vec![]).into()))
            } else if let Token::Symbol(verb) = &args[0].class {
                let func = ctx
                    .resolve_function(verb)
                    .with_context(|| make_ast_error(e))?;

                let r = apply(&func, &args[1..], ctx, settings);
                match func.class {
                    FunctionClass::UserDefined(_) => {
                        r.map(|o| o.map(|n| n.with_debug(e.debug_info())))
                    }
                    _ => r,
                }
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
            Kind::Expression(e) => {
                let n = reduce(e, ctx, settings)?.unwrap();
                ctx.edit_symbol(name, &|x| {
                    if let Expression::Column { kind, .. } = x {
                        *kind = Kind::Expression(Box::new(n.clone()))
                    }
                })?;
                Ok(None)
            }
            _ => Ok(None),
        },
        Token::DefInterleaving { target, froms } => {
            let target_handle =
                if let Expression::Column { handle, .. } = ctx.resolve_symbol(&target.name)?.e() {
                    handle.to_owned()
                } else {
                    unreachable!()
                };

            let mut from_handles = Vec::new();
            for from in froms {
                match &from.class {
                    Token::Symbol(name) => {
                        if let Expression::Column { handle, .. } = ctx.resolve_symbol(name)?.e() {
                            from_handles.push(handle.clone());
                        } else {
                            bail!("{} is not a column", name.white().bold());
                        }
                    }
                    Token::IndexedSymbol { name, index } => {
                        if let Expression::ArrayColumn { handle, domain, .. } =
                            ctx.resolve_symbol(name)?.e()
                        {
                            let index_usize = reduce(index, ctx, settings)?
                                .and_then(|n| n.pure_eval().ok())
                                .and_then(|b| b.to_usize())
                                .ok_or_else(|| {
                                    anyhow!("{:?} is not a valid index", index.white().bold())
                                })?;

                            if !domain.contains(index_usize.try_into().unwrap()) {
                                bail!("index {} is not in domain {:?}", index_usize, domain);
                            }
                            from_handles
                                .push(ColumnRef::from_handle(handle.as_handle().ith(index_usize)));
                        } else {
                            bail!("{} is not an array column", name.white().bold());
                        };
                    }
                    _ => unreachable!(),
                }
            }
            ctx.insert_computation(
                &target_handle,
                Computation::Interleaved {
                    target: target_handle.clone(),
                    froms: from_handles.clone(),
                },
            )?;
            Ok(None)
        }
        Token::DefColumns(_)
        | Token::DefPerspective { .. }
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
        | Token::DefLookup { .. }
        | Token::DefInrange(..) => Ok(None),
        Token::BlockComment(_) | Token::InlineComment(_) => unreachable!(),
    }
    .with_context(|| make_ast_error(e))
}

pub(crate) fn reduce_toplevel(
    e: &AstNode,
    ctx: &mut Scope,
    settings: &CompileSettings,
) -> Result<Option<Constraint>> {
    match &e.class {
        Token::DefConstraint {
            name,
            domain,
            guard,
            perspective,
            body,
        } => {
            let handle = Handle::new(ctx.module(), name);
            let module = ctx.module();
            let mut ctx = if let Some(perspective) = perspective {
                ctx.jump_in(&format!("in-{perspective}"))?
            } else {
                ctx.clone()
            };
            let body = reduce(body, &mut ctx, settings)?.unwrap_or_else(|| Expression::Void.into());
            let body = if let Some(guard) = guard {
                let guard_expr = reduce(guard, &mut ctx, settings)?
                    .with_context(|| anyhow!("guard `{:?}` is empty", guard))?;
                match guard_expr.t().c() {
                    Conditioning::Loobean => {
                        bail!("unexpected loobean guard in {}", handle.pretty())
                    }
                    _ => Intrinsic::IfNotZero.call(&[guard_expr, body])?,
                }
            } else {
                body
            };
            let body = if let Some(perspective) = perspective {
                let persp_guard = ctx
                    .tree
                    .borrow()
                    .metadata()
                    .get_perspective_trigger(&module, perspective)?;
                // Perspectives are just multiplicative coefficients, and are
                // controlled exceptions to the usual loobean typing rules
                let body_type = body.t();
                Intrinsic::Mul
                    .call(&[persp_guard, body])?
                    .with_type(body_type)
            } else {
                body
            };

            if body.t() == Type::Void {
                warn!(
                    "constraint {} should be of type {}, found {}",
                    handle.pretty(),
                    "Loobean".yellow().bold(),
                    body.t().red().bold()
                );
                Ok(None)
            } else {
                if !body.t().m().is_loobean() {
                    error!(
                        "constraint {} should be {}, found {}",
                        handle.pretty(),
                        "loobean".yellow().bold(),
                        body.t().red().bold()
                    )
                }
                let domain = if let Some(d) = domain {
                    Some(d.concretize(|n| {
                        crate::compiler::generator::reduce(n, &mut ctx.clone(), settings)
                            .transpose()
                            .unwrap()
                            .and_then(|r| r.pure_eval())
                            .and_then(|bi| {
                                bi.to_isize().ok_or_else(|| anyhow!("{} is not an i64", bi))
                            })
                    })?)
                } else {
                    None
                };

                Ok(Some(Constraint::Vanishes {
                    handle,
                    domain,
                    expr: Box::new(body),
                }))
            }
        }
        Token::DefLookup {
            name,
            including: parent,
            included: child,
        } => {
            *ctx = ctx.derive(&format!("lookup-{}", name))?.global(true);
            let handle = Handle::new(ctx.module(), name);
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
                Ok(Some(Constraint::Lookup {
                    handle,
                    including: parents,
                    included: children,
                }))
            }
        }
        Token::DefInrange(e, range) => {
            let handle = Handle::new(ctx.module(), format!("{}_lt_{}", e, range));
            Ok(Some(Constraint::InRange {
                handle,
                exp: reduce(e, ctx, settings)?.unwrap(),
                max: Value::from(*range),
            }))
        }
        Token::DefColumns(columns) => {
            for c in columns {
                reduce(c, ctx, settings)?;
            }
            Ok(None)
        }
        Token::DefPerspective {
            name,
            trigger,
            columns,
        } => {
            // Create the new perspective in the current module and ensures it does not exist
            let module = ctx.module();
            let trigger = reduce(trigger, ctx, settings)?.ok_or_else(|| {
                anyhow!(
                    "guard for perspective {} can not be empty",
                    name.bold().yellow()
                )
            })?;
            ctx.tree
                .borrow_mut()
                .metadata_mut()
                .set_perspective_trigger(&module, name, trigger)?;

            // Insert all columns in this new perspective
            let mut new_ctx = ctx.jump_in(&format!("in-{name}"))?;
            for c in columns {
                reduce(c, &mut new_ctx, settings)?;
            }
            Ok(None)
        }
        Token::DefModule(name) => {
            *ctx = ctx.switch_to_module(name)?;
            Ok(None)
        }
        Token::Value(_) | Token::Symbol(_) | Token::List(_) | Token::Domain(_) => {
            bail!("unexpected top-level form: {:?}", e)
        }
        Token::Defun { .. }
        | Token::Defpurefun { .. }
        | Token::DefAliases(_)
        | Token::DefunAlias(..)
        | Token::DefConsts(..) => Ok(None),
        Token::DefPermutation { from, to, signs } => {
            let froms = from
                .iter()
                .map(|from| {
                    if let Some(n) = reduce(from, ctx, settings)? {
                        if let Expression::Column { handle, .. } = n.e() {
                            return Ok(handle.clone());
                        }
                    }
                    bail!("`{}` is not a column", from.white().bold())
                })
                .collect::<Result<Vec<_>>>()?;

            // TODO is this needed?
            to.iter()
                .map(|f| ctx.resolve_symbol(&f.name))
                .collect::<Result<Vec<_>, errors::symbols::Error>>()
                .with_context(|| anyhow!("while defining permutation"))?;
            let suffix = hash_strings(froms.iter().map(|f| f.as_handle().name.clone()));

            let tos = to
                .iter()
                .enumerate()
                .map(|(i, t)| {
                    Handle::new(ctx.module(), &t.name)
                        .and_with_perspective(
                            froms[i]
                                .as_handle()
                                .perspective
                                .as_ref()
                                .map(|p| format!("{p}-srt-{suffix}")),
                        )
                        .into()
                })
                .collect::<Vec<ColumnRef>>();

            ctx.insert_many_computations(
                &tos,
                Computation::Sorted {
                    froms: froms.clone(),
                    tos: tos.clone(),
                    signs: signs.clone(),
                },
            )?;

            Ok(Some(Constraint::Permutation {
                handle: Handle::new(
                    ctx.module(),
                    format!(
                        "{}_intrld_{}",
                        froms.iter().map(|f| f.to_string()).join("_"),
                        tos.iter().map(|f| f.to_string()).join("_"),
                    ),
                ),
                from: froms,
                to: tos,
            }))
        }
        Token::DefInterleaving { .. } => {
            reduce(e, ctx, settings)?;
            Ok(None)
        }
        _ => unreachable!("{:?}", e),
    }
}

pub fn make_ast_error(exp: &AstNode) -> String {
    errors::parser::make_src_error(&exp.src, exp.lc)
}

pub fn pass(ast: &Ast, ctx: Scope, settings: &CompileSettings) -> Vec<Result<Constraint>> {
    let mut module = ctx;

    ast.exprs
        .iter()
        .filter_map(|exp| reduce_toplevel(exp, &mut module, settings).transpose())
        .collect()
}
