#![allow(dead_code)]
#[macro_use]
extern crate pest_derive;

mod cgo;
mod check;
mod column;
mod compiler;
mod compute;
mod constants;
mod dag;
mod errors;
mod import;
mod pretty;
mod structs;
mod transformer;
mod utils;

use clap::Parser;
use compiler::ConstraintSet;
use std::sync::RwLock;

#[derive(Parser)]
#[command(version, about)]
struct Args {
    #[clap(flatten)]
    verbose: clap_verbosity_flag::Verbosity,
    /// Trace file to use for the computation.
    tracefile: String,
    /// Target constraints (bin) file to be translated
    binfile: String,
    #[arg(long, help = "exit on failing columns")]
    fail_on_missing: bool,
}

pub(crate) static IS_NATIVE: RwLock<bool> = RwLock::new(true);

// This provides a CLI interface to the CGO API, primarily for testing
// purposes.
fn main() {
    // Parse command-line arguments
    let args = Args::parse();
    // Configure logging
    buche::new()
        .verbosity(args.verbose.log_level_filter())
        .quiet(args.verbose.is_silent())
        .init()
        .unwrap();
    // Report arguments
    println!("Constraints file: {}", args.binfile);
    println!("Trace file: {}", args.tracefile);
    println!("Fail on missing: {}", args.fail_on_missing);
    // Construct constraint set
    let mut corset = cgo::corset_from_file(&args.binfile).unwrap();
    // Read trace file
    let trace =
        cgo::compute_trace_from_file(&mut corset, &args.tracefile, args.fail_on_missing).unwrap();
    //
    for col in trace.ids {
        println!("COLUMN: {col}");
    }
}
