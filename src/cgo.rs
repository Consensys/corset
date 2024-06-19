use anyhow::*;
use compiler::ConstraintSet;
use log::*;
use rayon::{prelude::*};
use transformer::{AutoConstraint, ExpansionLevel};

use crate::{
    compute,
    column::{Computation, Value, ValueBacking},
    compiler,
    compiler::{ColumnRef,EvalSettings},
    transformer
};

type Corset = ConstraintSet;

const EMPTY_MARKER: [u8; 32] = [
    2, 4, 8, 16, 32, 64, 128, 255, 255, 128, 64, 32, 16, 8, 4, 2, 2, 4, 8, 16, 32, 64, 128, 255,
    255, 128, 64, 32, 16, 8, 4, 2,
];

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
	let rs = corset.columns.all().par_iter().map(|cref| {
	    let handle = corset.handle(cref);
	    let col = Self::construct_computed_column(cref, corset);
            trace!("Writing {}", handle);
	    (col, handle.to_string())
	}).collect::<Vec<_>>();
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
    /// column, along with an appropriate padding value for a given
    /// column.  For computed columns, this means actually computing
    /// their values.
    fn construct_computed_column(cref: &ColumnRef, corset: &Corset) -> ComputedColumn {
	let empty_backing: ValueBacking = ValueBacking::default();
        let column = corset.columns.column(cref).unwrap();
        let handle = &column.handle;
	// Determine spilling needed for the given module.
        let spilling = corset.spilling_of(&handle.module).unwrap_or(0);
	// Get the back for the given column, or use a default.
        let backing = corset.columns.backing(cref).unwrap_or(&empty_backing);
	// Determine padding value for this column.
        let padding: Value = if let Some(v) = column.padding_value.as_ref() {
            v.clone()
        } else if let Some(v) = backing.get(-spilling, false, &corset.columns) {
	    v
	} else {
            Self::compute_padding_value(cref,corset)
        };
	// Iterate all values of the column, computing them as
	// necessary.
        let values: Vec<[u8; 32]> = backing
            .iter(&corset.columns)
            .map(|x| x.to_bytes().try_into().unwrap())
            .collect::<Vec<_>>();
	// Done
	ComputedColumn {
	    values,
            padding_value: { padding.to_bytes().try_into().unwrap() },
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
                    Computation::ExoConstant { value, .. } => value.clone()
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

pub fn corset_from_file(zkevmfile: &str) -> Result<Corset> {
    info!("Loading `{}`", &zkevmfile);
    let constraints = ron::from_str(
        &std::fs::read_to_string(zkevmfile)
            .with_context(|| anyhow!("while reading `{}`", zkevmfile))?,
    )
    .with_context(|| anyhow!("while parsing `{}`", zkevmfile))?;
    make_corset(constraints)
}

pub fn corset_from_str(zkevmstr: &str) -> Result<Corset> {
    let constraints =
        ron::from_str(zkevmstr).with_context(|| anyhow!("while parsing the provided zkEVM"))?;

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
