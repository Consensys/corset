use anyhow::*;
use cached::Cached;
use colored::Colorize;
use itertools::Itertools;
use log::*;
use logging_timer::time;
use num_bigint::BigInt;
use num_traits::cast::ToPrimitive;
use num_traits::{One, Zero};
use once_cell::sync::OnceCell;
use pairing_ce::bn256::Fr;
use pairing_ce::ff::{Field, PrimeField};
use serde::{Deserialize, Serialize};

use std::collections::{HashMap, HashSet};
use std::fmt::Debug;
use std::io::Write;

use std::sync::atomic::AtomicUsize;
use std::{unimplemented, unreachable};

use super::node::ColumnRef;
use super::tables::{ComputationTable, Scope};
use super::{common::*, CompileSettings, Expression, Magma, Node, Type};
use crate::column::{Column, ColumnSet, Computation, RegisterID};
use crate::compiler::parser::*;
use crate::dag::ComputationDag;
use crate::errors::{self, CompileError, RuntimeError};
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
        from: Vec<ColumnRef>,
        to: Vec<ColumnRef>,
        signs: Vec<bool>,
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

    pub fn add_id_to_handles(&mut self, set_id: &dyn Fn(&mut ColumnRef)) {
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
                signs: _,
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

// enum Perspective {
//     Expression(Node),
//     Column(Handle),
// }

// #[derive(Default, Debug, Clone)]
pub type PerspectiveTable = HashMap<String, HashMap<String, Node>>;
// impl PerspectiveTable {
//     pub fn insert(&mut self, module: &str, name: &str, guard: Perspective) -> Result<()> {
//         todo!()
//     }
// }

#[derive(Default, Debug, Serialize, Deserialize, Clone)]
pub struct ConstraintSet {
    pub columns: ColumnSet,
    pub constraints: Vec<Constraint>,
    pub constants: HashMap<Handle, BigInt>,
    pub computations: ComputationTable,
    pub perspectives: PerspectiveTable,
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
        };
        r.fill_spilling();
        r.convert_refs_to_ids()?;
        r.allocate_registers();
        r.fill_perspectives()?;
        r.validate()?;
        Ok(r)
    }

    fn allocate_registers(&mut self) {
        #[derive(Default, Debug)]
        struct ColumnPool {
            root: Vec<ColumnRef>,
            // Module -> SizeFactor -> Type -> Perspective -> Columns names
            perspectives:
                HashMap<String, HashMap<usize, HashMap<Magma, HashMap<String, Vec<ColumnRef>>>>>,
        }

        let mut pool: ColumnPool = Default::default();
        for (h, col) in self.columns.iter() {
            match &col.kind {
                Kind::Atomic => match &col.handle.perspective {
                    Some(name) => {
                        let module = col.handle.module.to_string();
                        let magma = col.t.magma();
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
                },
                _ => {}
            }
        }

        // module-global columns all have their own register
        for c in pool.root {
            let reg = self.columns.new_register(Handle::new("reg", c.to_string()));
            self.columns.assign_register(&c, reg).unwrap();
        }

        // subset columns are grouped together if they are of the same type
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
                        let reg = self.columns.new_register(Handle::new(&module, names));
                        for cols in sets.values() {
                            if let Some(col_id) = cols.get(i) {
                                self.columns.assign_register(col_id, reg).unwrap();
                            }
                        }
                    }
                }
            }
        }

        // finally, computed columns registers are allocated depending on their type
        let mut jobs: ComputationDag = Default::default();
        for c in self.computations.iter() {
            jobs.insert_computation(c)
        }

        let todos = jobs.job_slices();
        for slice in todos {
            for c in slice
                .iter()
                .filter_map(|h| self.computations.computation_idx_for(h))
                .collect::<HashSet<_>>()
                .into_iter()
                .map(|i| self.computations.get(i).unwrap().to_owned())
            {
                match c {
                    Computation::Interleaved { target, .. }
                    | Computation::CyclicFrom { target, .. } => {
                        let col = self.columns.get_col(&target).unwrap();
                        let reg = self.columns.new_register(col.handle.clone());
                        self.columns.assign_register(&target, reg).unwrap();
                    }
                    Computation::Sorted { froms, tos, .. } => {
                        let mut reg_translation = HashMap::<RegisterID, RegisterID>::new();
                        for (f, t) in froms.iter().zip(tos.iter()) {
                            let reg = reg_translation
                                .entry(self.columns.get_col(f).unwrap().register.unwrap())
                                .or_insert_with(|| {
                                    self.columns.new_register(
                                        self.columns.get_col(t).unwrap().handle.clone(),
                                    )
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
                            let col = self.columns.get_col(r).unwrap();
                            let reg = self.columns.new_register(col.handle.clone());
                            self.columns.assign_register(r, reg).unwrap();
                        }
                    }
                    _ => unreachable!(),
                }
            }
        }
    }

    fn fill_perspectives(&mut self) -> Result<()> {
        let mut jobs: ComputationDag = Default::default();
        for c in self.computations.iter() {
            jobs.insert_computation(c)
        }

        let todos = jobs.job_slices();
        for slice in todos {
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
                            let from_handle = self.columns.get_col(&froms[0])?.handle.to_owned();
                            let module = from_handle.module.to_owned();
                            if let Expression::Column { handle, .. } =
                                self.get_perspective(&module, &perspective)?.e().clone()
                            {
                                let srt_guard_col_handle = Handle::new(
                                    &module,
                                    format!(
                                        "{}%intrld",
                                        self.columns.get_col(&handle).unwrap().handle.name,
                                    ),
                                );
                                let srt_guard_id = self
                                    .columns
                                    .insert_column_and_register(
                                        Column::builder()
                                            .kind(Kind::Phantom)
                                            .handle(srt_guard_col_handle)
                                            .build(),
                                        true,
                                    )
                                    .unwrap();
                                let srt_guard = Node::column()
                                    .handle(srt_guard_id.clone())
                                    .kind(Kind::Phantom)
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
                            } else {
                                unreachable!()
                            }
                        }
                    }
                    Computation::Sorted { froms, tos, .. } => {
                        for (j, from) in froms.clone().iter().enumerate() {
                            if let Some(perspective) = self.columns.perspective(from)?.cloned() {
                                let from_handle = self.columns.get_col(from)?.handle.to_owned();
                                let module = from_handle.module.to_owned();
                                if let Expression::Column { handle, .. } =
                                    self.get_perspective(&module, &perspective)?.e().clone()
                                {
                                    let srt_guard_col_handle = Handle::new(
                                        &module,
                                        format!(
                                            "{}%srt",
                                            self.columns.get_col(&handle).unwrap().handle.name,
                                        ),
                                    );
                                    let srt_guard_id = self
                                        .columns
                                        .insert_column_and_register(
                                            Column::builder()
                                                .kind(Kind::Phantom)
                                                .handle(srt_guard_col_handle)
                                                .build(),
                                            true,
                                        )
                                        .unwrap();
                                    let srt_guard = Node::column()
                                        .handle(srt_guard_id.clone())
                                        .kind(Kind::Phantom)
                                        .build();
                                    let srt_guard_name = format!("{}-srt", perspective);

                                    self.insert_perspective(&module, &srt_guard_name, srt_guard)?;
                                    self.columns.set_perspective(&tos[j], &srt_guard_name)?;
                                    if let Computation::Sorted { froms, tos, signs } =
                                        self.computations.get_mut(i).unwrap()
                                    {
                                        froms.insert(0, handle.clone());
                                        tos.insert(0, srt_guard_id.clone());
                                        signs.insert(0, true);
                                    } else {
                                        unreachable!()
                                    }
                                    self.computations.add_dependency(srt_guard_id, i)?;
                                } else {
                                    unimplemented!("non-column perspectives not yet supported");
                                };
                            } else {
                                unreachable!()
                            };
                        }
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
        &self.columns.get_col(h).unwrap().handle
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
                Computation::Composite { exp, .. } => exp.add_id_to_handles(&convert_to_id),
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
            }
        }

        for p in self.perspectives.values_mut().flat_map(|k| k.values_mut()) {
            p.add_id_to_handles(&convert_to_id)
        }

        Ok(())
    }

    pub fn raw_len_for_or_set(&mut self, m: &str, x: isize) -> isize {
        *self.columns.raw_len.entry(m.to_string()).or_insert(x)
    }

    pub fn spilling_for(&self, h: &ColumnRef) -> Option<isize> {
        let col = self.columns.get_col(h).ok()?;
        self.columns.spilling.get(&col.handle.module).cloned()
    }

    fn compute_spilling(&mut self, m: &str) -> isize {
        self.computations
            .iter()
            .filter_map(|c| match c {
                Computation::Composite { target, exp } => {
                    if target.as_handle().module == m {
                        Some(exp.past_spill().abs().max(exp.future_spill().abs()))
                    } else {
                        None
                    }
                }
                _ => None,
            })
            .chain(self.constraints.iter().filter_map(|c| match c {
                Constraint::Vanishes { expr, .. } => {
                    Some(expr.past_spill().abs().max(expr.future_spill().abs()))
                }
                _ => None,
            }))
            .max()
            .unwrap_or(0)
    }

    fn fill_spilling(&mut self) {
        let all_modules = self.columns.modules();
        for m in all_modules {
            let spilling = self.compute_spilling(&m);
            self.columns.spilling.insert(m, spilling);
        }
    }

    pub fn length_multiplier(&self, h: &ColumnRef) -> usize {
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
                .columns
                .get_col(h)
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
                .map(|h| (h.clone(), self.columns.get_col(&h).unwrap()))
                .filter(|(_, c)| c.handle.module == module)
                .peekable();
            let empty_vec = Vec::new();
            while let Some((r, column)) = current_col.next() {
                let handle = &column.handle;
                trace!("Writing {}", handle);
                let value = self.columns.value(&r).unwrap_or(&empty_vec);
                let padding = if let Some(x) = column.padding_value {
                    Fr::from_str(&x.to_string()).unwrap()
                } else {
                    value.get(0).cloned().unwrap_or_else(|| {
                        self.computations
                            .computation_for(&r)
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
                Constraint::Plookup {
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
                            c.to_string()
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
            if let (Token::Symbol(i_name), Token::Range(is), body) =
                (&args[0].class, &args[1].class, &args[2])
            {
                let mut l = vec![];
                let mut t = Type::INFIMUM;
                for i in is {
                    let mut for_ctx = ctx.derived(
                        &format!(
                            "{}-for-{}-{}",
                            ctx.name(),
                            COUNTER
                                .get_or_init(|| AtomicUsize::new(0))
                                .fetch_add(1, std::sync::atomic::Ordering::Relaxed),
                            i
                        ),
                        false,
                        false,
                        false,
                    );
                    for_ctx.insert_symbol(
                        i_name,
                        Expression::Const(BigInt::from(*i), Fr::from_str(&i.to_string())).into(),
                    )?;

                    if let Some(r) = reduce(&body.clone(), &mut for_ctx, settings)? {
                        t = t.max(r.t());
                        l.push(r);                        
                    } else {
                        warn!("empty for loop body: {}", body.src.white().bold())
                    };
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
        Form::Todo => {
            error!("TODO not yet implemented");
            Ok(None)
        }
        Form::Let => {
            let sub_ctx_name = format!("let-{}", ctx.module());
            let mut sub_ctx = ctx.derived(&sub_ctx_name, false, false, false);
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
    ctx: &mut Scope,
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
    let mut f_ctx = ctx.derived(&f_mangle, b.pure, false, false);
    for (i, f_arg) in b.args.iter().enumerate() {
        f_ctx.insert_symbol(f_arg, traversed_args[i].clone())?;
    }
    reduce(&b.body, &mut f_ctx, settings)
}

fn apply_builtin(
    b: &Builtin,
    traversed_args: Vec<Node>,
    _ctx: &mut Scope,
    _settings: &CompileSettings,
) -> Result<Option<Node>> {
    b.validate_args(&traversed_args)?;

    match b {
        Builtin::ForceBool => {
            let arg = traversed_args[0].to_owned();
            if traversed_args[0].t().is_bool() {
                warn!(
                    "useless use of force-bool: {} is already boolean",
                    traversed_args[0].pretty()
                );
                Ok(Some(arg))
            } else {
                let new_type = arg.t().same_scale(Magma::Boolean);
                Ok(Some(arg.with_type(new_type)))
            }
        }
        Builtin::Len => {
            if let Expression::ArrayColumn {
                handle: _,
                domain,
                base: _,
            } = traversed_args[0].e()
            {
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
    ctx: &mut Scope,
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
            if let Expression::ArrayColumn { handle, .. } = &traversed_args[0].e() {
                let i = traversed_args[1].pure_eval()?.to_usize().ok_or_else(|| {
                    anyhow!("{:?} is not a valid indice", traversed_args[1].pure_eval())
                })?;
                let array = ctx.resolve_handle(handle)?;
                match array.e() {
                    Expression::ArrayColumn {
                        handle,
                        domain: range,
                        base,
                    } => {
                        if range.contains(&i) {
                            Ok(Some(
                                Node::column()
                                    .handle(handle.ith(i))
                                    .kind(Kind::Atomic)
                                    .base(*base)
                                    .t(array.t().magma())
                                    .build(),
                            ))
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
    ctx: &mut Scope,
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
                .resolve_symbol(name)
                .with_context(|| make_ast_error(e))?;
            Ok(Some(r))
        }

        Token::List(args) => {
            if args.is_empty() {
                Ok(Some(Expression::List(vec![]).into()))
            } else if let Token::Symbol(verb) = &args[0].class {
                let func = ctx
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
                ctx.edit_symbol(name, &|x| {
                    if let Expression::Column { kind, .. } = x {
                        *kind = Kind::Composite(Box::new(n.clone()))
                    }
                })?;
                Ok(None)
            }
            Kind::Interleaved(froms, _) => {
                // Create the interleaving
                let from_handles = froms
                    .iter()
                    .map(|f| match reduce(f, ctx, settings)?.unwrap().e() {
                        Expression::Column { handle: h, .. } => Ok(h.to_owned()),
                        x => Err(anyhow!("expected column, found {:?}", x)),
                    })
                    .collect::<Result<Vec<_>>>()
                    .with_context(|| anyhow!("while defining {}", name))?;

                ctx.edit_symbol(name, &|x| {
                    if let Expression::Column { kind, .. } = x {
                        *kind = Kind::Interleaved(vec![], Some(from_handles.to_vec()))
                    } else {
                        unreachable!()
                    }
                })?;

                // // Handle perspective stuff
                // let persp = from_handles
                //     .iter()
                //     .filter_map(|h| h.perspective.clone())
                //     .collect::<HashSet<_>>();
                // if persp.len() > 1 {
                //     bail!(
                //         "unable to interleave columns {} from different perspectives {}",
                //         from_handles.iter().map(Pretty::pretty).join(", "),
                //         persp.iter().join(", ")
                //     );
                // }
                // if let Some(persp) = persp.into_iter().next() {
                //     // Create a new column that will extend the perspective
                //     // guard to the size of the interleaving.
                //     // The new guards:
                //     // 1. does not belong to the perspective
                //     // 2. is made of interleaving the original guard
                //     // 3. is assigned as a guard of the automatically
                //     //    created interleaving representing the interleaved
                //     //    perspective.
                //     // TODO: handle the case where the guard is an expression
                //     let new_perspective = format!("{}%intrld×{}", persp, froms.len());
                //     let module = ctx.module();
                //     let guard = ctx
                //         .tree
                //         .borrow()
                //         .metadata()
                //         .get_perspective_trigger(&module, &persp)?;

                //     if let Expression::Column {
                //         handle,
                //         padding_value,
                //         ..
                //     } = guard
                //     {
                //         let new_name = format!("{}%intrld×{}", handle.name, froms.len());
                //         let new_handle = Handle::new(&module, &new_name);
                //         let kind = Kind::Interleaved(
                //             vec![],
                //             Some(
                //                 std::iter::repeat(handle.clone())
                //                     .take(froms.len())
                //                     .collect::<Vec<_>>(),
                //             ),
                //         );

                //         ctx.insert_symbol(
                //             &new_name,
                //             Node::column()
                //                 .handle(new_handle.clone())
                //                 .kind(kind.clone())
                //                 .t(Magma::Integer) // FIXME: get guard type
                //                 .and_padding_value(padding_value)
                //                 .build(),
                //         )?;

                //         if ctx.create_perspective(&new_perspective).is_ok() {
                //             debug!("defining perspective {}", new_perspective);
                //             ctx.tree
                //                 .borrow_mut()
                //                 .metadata_mut()
                //                 .set_perspective_trigger(
                //                     &module,
                //                     &new_perspective,
                //                     Expression::Column {
                //                         handle: new_handle.clone(),
                //                         kind,
                //                         padding_value: None,
                //                         base: Base::Dec,
                //                     },
                //                 )?;
                //         } else {
                //             debug!("perspective {} already set", new_perspective);
                //         }
                //     } else {
                //         // TODO: implement
                //         bail!(
                //             "expression-based perspective ({:?}) are not yet supported",
                //             guard
                //         )
                //     }
                //     ctx.edit_symbol(name, &|x| {
                //         if let Expression::Column { handle, .. } = x {
                //             handle.perspective = Some(new_perspective.clone());
                //         } else {
                //             unreachable!()
                //         }
                //     })?;
                // }
                Ok(None)
            }
            _ => Ok(None),
        },
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
        | Token::DefPlookup { .. }
        | Token::DefInrange(..) => Ok(None),
    }
    .with_context(|| make_ast_error(e))
}

fn perspective_of<'a, H: IntoIterator<Item = &'a Handle>>(hs: H) -> Result<Option<String>> {
    // TODO: make better errors
    // for h in hs.into_iter() {
    //     match c.perspective {
    //         Some(ref p) if *p != perspective => {
    //             bail!(anyhow!(
    //                 "column {} that is not available in perspective {}",
    //                 handle.pretty(),
    //                 c.pretty(),
    //                 perspective.bold(),
    //             ));
    //         }
    //         _ => {}
    //     }
    // }
    let ps = hs
        .into_iter()
        .filter_map(|h| h.perspective.clone())
        .collect::<HashSet<_>>();
    if ps.len() > 1 {
        bail!("no unique perspective")
    }

    Ok(ps.into_iter().next())
}

fn reduce_toplevel(
    e: &AstNode,
    ctx: &mut Scope,
    settings: &CompileSettings,
) -> Result<Option<Constraint>> {
    match &e.class {
        Token::DefConstraint {
            name,
            domain,
            guard,
            body,
        } => {
            let handle = Handle::new(ctx.module(), name);
            let module = ctx.module();
            let body = reduce(body, ctx, settings)?.unwrap_or_else(|| Expression::Void.into());
            let body = if let Some(guard) = guard {
                let guard_expr = reduce(guard, ctx, settings)?
                    .with_context(|| anyhow!("guard `{:?}` is empty", guard))?;
                Intrinsic::IfNotZero.call(&[guard_expr, body])?
            } else {
                body
            };
            // Check that no constraint crosses perspective boundaries
            let body = if let Some(perspective) =
                perspective_of(body.dependencies().iter().map(|h| h.as_handle()))?
            {
                let persp_guard = ctx
                    .tree
                    .borrow()
                    .metadata()
                    .get_perspective_trigger(&module, &perspective)?;
                Intrinsic::Mul.call(&[persp_guard.into(), body])?
            } else {
                body
            };

            Ok(Some(Constraint::Vanishes {
                handle,
                domain: domain.to_owned(),
                expr: Box::new(body),
            }))
        }
        Token::DefPlookup {
            name,
            including: parent,
            included: child,
        } => {
            *ctx = ctx.derived(&format!("plookup-{}", name), false, false, true);
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
                Ok(Some(Constraint::Plookup {
                    handle,
                    including: parents,
                    included: children,
                }))
            }
        }
        Token::DefInrange(e, range) => {
            let handle = Handle::new(ctx.module(), names::Generator::default().next().unwrap());
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
        Token::DefPerspective {
            name,
            trigger,
            columns,
        } => {
            // Create the new perspective in the current module and ensures it does not exist
            let module = ctx.module();
            let trigger = reduce(trigger, ctx, settings)?.unwrap().e().to_owned();
            ctx.tree
                .borrow_mut()
                .metadata_mut()
                .set_perspective_trigger(&module, name, trigger)?;

            // Insert all columns in this new perspective
            let mut new_ctx = ctx.clone_in_perspective(name)?;
            for c in columns {
                reduce(c, &mut new_ctx, settings)?;
            }
            Ok(None)
        }
        Token::DefModule(name) => {
            *ctx = ctx.derived(name, false, true, false);
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
        Token::DefPermutation { from, to, signs } => {
            // We look up the columns involved in the permutation just to ensure that they
            // are marked as "used" in the symbol table
            let froms = from
                .iter()
                .map(|from| {
                    if let Expression::Column { handle, .. } = ctx
                        .resolve_symbol(from)
                        .with_context(|| "while defining permutation")?
                        .e()
                    {
                        Ok(handle.to_owned())
                    } else {
                        unreachable!()
                    }
                })
                .collect::<Result<Vec<_>>>()?;
            to.iter()
                .map(|f| ctx.resolve_symbol(f))
                .collect::<Result<Vec<_>>>()
                .with_context(|| anyhow!("while defining permutation"))?;

            let tos = to
                .iter()
                .enumerate()
                .map(|(i, f)| {
                    Handle::new(ctx.module(), f)
                        .and_with_perspective(froms[i].as_handle().perspective.clone())
                        .into()
                })
                .collect::<Vec<ColumnRef>>();
            Ok(Some(Constraint::Permutation {
                handle: Handle::new(ctx.module(), names::Generator::default().next().unwrap()),
                from: froms,
                to: tos,
                signs: signs.clone(),
            }))
        }
        _ => unreachable!("{:?}", e),
    }
}

pub fn make_ast_error(exp: &AstNode) -> String {
    errors::parser::make_src_error(&exp.src, exp.lc)
}

pub fn pass(ast: &Ast, ctx: Scope, settings: &CompileSettings) -> Result<Vec<Constraint>> {
    let mut r = vec![];
    let mut module = ctx;

    for exp in ast.exprs.iter() {
        if let Some(c) = reduce_toplevel(exp, &mut module, settings)? {
            r.push(c)
        }
    }
    Ok(r)
}
