use crate::compiler::ConstraintSet;
use crate::{check, compiler, compute, import};
use crate::{transformer::ExpansionLevel, ConstraintSetBuilder};
use anyhow::*;
use std::fs;
use std::sync::Once;

pub static TEST_DIR: &str = "tests";

// Configure for native evaluation, since this is the gold standard.
static INIT: Once = Once::new();
pub fn initialize() {
    INIT.call_once(|| {
        *crate::IS_NATIVE.write().unwrap() = true;
    });
}

include!(concat!(env!("OUT_DIR"), "/lisp_tests.rs"));

// Check that a given set of constraints agrees with each of the
// traces provided.  By "agree" we mean that, if the trace is supposed
// to hold then the it does indeed hold; or, if the trace is supposed
// to be rejected then it is indeed rejected.
//
// NOTE: its pretty inefficient that we recompile the source from
// scratch for each trace.  It would be nice to avoid this!
fn check(name: &str) {
    check_json_traces(name, true);
    check_json_traces(name, false);
}

fn check_json_traces(name: &str, expected: bool) {
    let mut line = 1;
    // Determine which file to use!
    let tracefile = if expected {
        format!("{}/{}.accepts", TEST_DIR, name)
    } else {
        format!("{}/{}.rejects", TEST_DIR, name)
    };
    // Each line in the trace file is an "extended" trace.
    for trace in fs::read_to_string(&tracefile).unwrap().lines() {
        // Compile source constraints again (note: this is necessary
        // because ConstraintSet does not (yet) implement clone()).
        let cs = compile(name).unwrap();
        // Determine trace outcome
        let outcome = check_json_trace(trace, cs, expected).is_ok();
        // Check against what was expected
        assert_eq!(expected, outcome, "{tracefile}, line {line}: {trace}");
        line += 1;
    }
}

fn compile(name: &str) -> Result<ConstraintSet> {
    let lispfile = format!("{}/{}.lisp", TEST_DIR, name);
    // Read source file
    let source = fs::read_to_string(lispfile).unwrap();
    // Configure the build
    let mut r = ConstraintSetBuilder::from_sources(false, false);
    r.add_source(&source)?;
    r.expand_to(ExpansionLevel::top());
    // Done
    r.into_constraint_set()
}

/// Check a given constraint set against a given trace (in JSON).
fn check_json_trace(trace: &str, mut cs: ConstraintSet, report: bool) -> Result<()> {
    let keep_raw = false; // what does this do?
    let fail_on_missing = true;
    // Read trace data into constraint set
    import::read_trace_str(trace.as_bytes(), &mut cs, keep_raw)?;
    // Perform trace expansion
    compute::prepare(&mut cs, fail_on_missing)?;
    // Check whether constraints accepted or not.
    check::check(
        &cs,
        &None, // Consider all columns
        &[],   // Consider all constraints
        check::DebugSettings::new().report(report),
    )
}
