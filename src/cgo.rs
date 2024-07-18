use anyhow::*;
use compiler::ConstraintSet;
use log::*;
use rayon::prelude::*;
use transformer::{AutoConstraint, ExpansionLevel};

use crate::{
    column::{Computation, RegisterID, Value, ValueBacking},
    compiler,
    compiler::{ColumnRef, EvalSettings},
    compute,
    structs::Handle,
    transformer,
};

type Corset = ConstraintSet;

const EMPTY_MARKER: [u8; 32] = [
    2, 4, 8, 16, 32, 64, 128, 255, 255, 128, 64, 32, 16, 8, 4, 2, 2, 4, 8, 16, 32, 64, 128, 255,
    255, 128, 64, 32, 16, 8, 4, 2,
];

// For backwards compatibility.
const RON_BINFILE: bool = false;

pub struct ComputedColumn {
    pub padding_value: [u8; 32],
    pub values: Vec<[u8; 32]>,
}
impl ComputedColumn {
    pub fn empty() -> Self {
        ComputedColumn {
            padding_value: EMPTY_MARKER,
            values: vec![EMPTY_MARKER],
        }
    }

    pub fn is_empty(&self) -> bool {
        self.values.is_empty() && self.padding_value == EMPTY_MARKER
    }
}

#[derive(Default)]
pub struct Trace {
    pub columns: Vec<ComputedColumn>,
    pub ids: Vec<String>,
}
impl Trace {
    pub fn from_constraints(corset: &Corset) -> Self {
        let mut r = Trace {
            ..Default::default()
        };
        // Iterate columns determining their concrete values, as well
        // their padding value.
        // let rs = corset.columns.all().par_iter().map(|cref| {
        //     let handle = corset.handle(cref);
        //     let col = Self::construct_computed_column(cref, corset);
        //     trace!("Writing {}", handle);
        //     (col, handle.to_string())
        // }).collect::<Vec<_>>();
        //
        let rs = corset
            .columns
            .regs()
            .par_iter()
            .map(|reg_id| {
                // Access register info
                let register = &corset.columns.registers[*reg_id];
                let handle: &Handle = register.handle.as_ref().unwrap();
                let col = Self::construct_computed_register(*reg_id, corset);
                trace!("Writing {}", handle);
                (col, handle.to_string())
            })
            .collect::<Vec<_>>();
        //
        for (col, id) in rs {
            r.columns.push(col);
            r.ids.push(id);
        }
        // Done
        r
    }

    pub fn from_ptr<'a>(ptr: *const Trace) -> &'a Self {
        assert!(!ptr.is_null());
        unsafe { &*ptr }
    }

    pub fn mut_from_ptr<'a>(ptr: *mut Trace) -> &'a mut Self {
        assert!(!ptr.is_null());
        unsafe { &mut *ptr }
    }

    /// Responsible for determining the concrete values for a given
    /// register, along with an appropriate padding value for it.
    fn construct_computed_register(reg_id: RegisterID, corset: &Corset) -> ComputedColumn {
        let empty_backing: ValueBacking = ValueBacking::default();
        // Access register info
        let register = &corset.columns.registers[reg_id];
        // Determine values for this register
        let backing = register.backing().unwrap_or(&empty_backing);
        // Determine padding for this register
        let padding = Self::determine_register_padding(reg_id, backing, corset);
        // Iterate all values of the register, computing them as
        // necessary.
        let values: Vec<[u8; 32]> = backing
            .iter(&corset.columns)
            .map(|x| x.to_bytes().try_into().unwrap())
            .collect::<Vec<_>>();
        // Donme
        ComputedColumn {
            values,
            padding_value: { padding.to_bytes().try_into().unwrap() },
        }
    }

    /// Determine the padding value for a given register.  This may
    /// involve actually computing one (or more) values.
    fn determine_register_padding(
        reg_id: RegisterID,
        backing: &ValueBacking,
        corset: &Corset,
    ) -> Value {
        // Access register info
        let register = &corset.columns.registers[reg_id];
        let handle: &Handle = register.handle.as_ref().unwrap();
        let crefs = corset.columns.columns_of(reg_id);
        //
        if crefs.is_empty() {
            // This should be unreachable.
            unreachable!("No column assigned to register ({handle}:{reg_id})")
        } else {
            // I'm assuming every register is mapped to at least one
            // column.
            let padding = Self::determine_column_padding(&crefs[0], backing, corset);
            //
            for i in 1..crefs.len() {
                // Computing padding value for ith column
                let ith = Self::determine_column_padding(&crefs[i], backing, corset);
                // If they don't match, we have a problem.
                if padding != ith {
                    // In principle, this should be unreachable.  The
                    // argument is that user columns are assigned to
                    // the same register to manage perspectives.
                    // Furthermore, user columns have the same padding
                    // (zero).
                    panic!(
                        "Columns {} and {} for register {handle} have \
                            different padding requirements ({} vs {})",
                        crefs[0], crefs[i], padding, ith
                    );
                }
            }
            //
            padding
        }
    }

    fn determine_column_padding(
        cref: &ColumnRef,
        backing: &ValueBacking,
        corset: &Corset,
    ) -> Value {
        let column = corset.columns.column(cref).unwrap();
        let handle = &column.handle;
        // Determine spilling needed for the given module.
        let spilling = corset.spilling_of(&handle.module).unwrap_or(0);
        // consider the option
        if let Some(v) = column.padding_value.as_ref() {
            v.clone()
        } else if let Some(v) = backing.get(-spilling, false, &corset.columns) {
            v
        } else {
            Self::compute_padding_value(cref, corset)
        }
    }

    /// Determine the padding value for a computation, given that it
    /// is otherwise not determined.  This may involve actually
    /// computing a value.
    fn compute_padding_value(cref: &ColumnRef, corset: &Corset) -> Value {
        match corset.computations.computation_for(cref) {
            None => Value::zero(),
            Some(c) => {
                // Determine padding value based on the type of
                // computation.
                match c {
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
                    Computation::ExoConstant { value, .. } => value.clone(),
                }
            }
        }
    }
}

pub fn make_corset(mut constraints: ConstraintSet) -> Result<Corset> {
    transformer::expand_to(
        &mut constraints,
        ExpansionLevel::all().into(),
        AutoConstraint::all(),
    )?;
    transformer::concretize(&mut constraints);
    Ok(constraints)
}

pub fn corset_from_file(filename: &str) -> Result<Corset> {
    info!("Loading `{}`", &filename);
    // Read the constraint-set bin file
    let binfile = &std::fs::read_to_string(filename)
        .with_context(|| anyhow!("while reading `{}`", filename))?;
    //
    let constraints = if RON_BINFILE {
        ron::from_str(binfile).with_context(|| anyhow!("while parsing `{}` (RON)", filename))?
    } else {
        serde_json::from_str(binfile)
            .with_context(|| anyhow!("while parsing `{}` (JSON)", filename))?
    };
    make_corset(constraints)
}

pub fn corset_from_str(binfile: &str) -> Result<Corset> {
    let constraints = if RON_BINFILE {
        ron::from_str(binfile).with_context(|| anyhow!("while parsing bin file (RON)"))?
    } else {
        serde_json::from_str(binfile).with_context(|| anyhow!("while parsing bin file (JSON)"))?
    };
    make_corset(constraints)
}

pub fn compute_trace_from_file(
    constraints: &mut Corset,
    tracefile: &str,
    fail_on_missing: bool,
) -> Result<Trace> {
    compute::compute_trace(tracefile, constraints, fail_on_missing)
        .with_context(|| format!("while computing from file `{}`", tracefile))?;
    Ok(Trace::from_constraints(constraints))
}

pub fn compute_trace_from_str(
    constraints: &mut Corset,
    tracestr: &str,
    fail_on_missing: bool,
) -> Result<Trace> {
    compute::compute_trace_str(tracestr.as_bytes(), constraints, fail_on_missing)
        .with_context(|| format!("while computing from string `{}`", tracestr))?;
    Ok(Trace::from_constraints(constraints))
}
