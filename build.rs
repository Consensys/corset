use std::path::PathBuf;
use std::{env, fs, io::Write, process::Command};

fn main() {
    // Export the current git hash
    let git_hash = Command::new("git")
        .args(["rev-parse", "HEAD"])
        .output()
        .ok()
        .and_then(|o| String::from_utf8(o.stdout).ok())
        .map(|s| s.chars().take(7).collect::<String>())
        .unwrap_or_else(|| "UNKNW".into());
    println!("cargo:rustc-env=GIT_HASH={}", git_hash);

    if cfg!(all(target_arch = "x86_64", target_feature = "avx")) {
        println!("cargo:rustc-env=SIMD_ENABLED=SIMD JSON parsing enabled");
    } else {
        println!("cargo:rustc-env=SIMD_ENABLED=SIMD JSON parsing unavailable");
    }

    // Generate C FFI bindings
    let crate_dir = env::var("CARGO_MANIFEST_DIR").unwrap();
    let package_name = env::var("CARGO_PKG_NAME").unwrap();
    let output_file = target_dir()
        .join(format!("{}.h", package_name))
        .display()
        .to_string();

    let mut config: cbindgen::Config = Default::default();
    config.language = cbindgen::Language::C;

    cbindgen::generate_with_config(crate_dir, config)
        .unwrap()
        .write_to_file(output_file);

    // Generate tests from lisp files
    generate_tests_from_lisp_files();
}

/// Find the location of the `target/` directory. Note that this may be
/// overridden by `cmake`, so we also need to check the `CARGO_TARGET_DIR`
/// variable.
fn target_dir() -> PathBuf {
    if let Ok(target) = env::var("CARGO_TARGET_DIR") {
        PathBuf::from(target)
    } else {
        PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap()).join("target")
    }
}

pub static TESTS_DIR: &str = "tests";

include!("tests/models.rs");

/// Generate a Rust file containing tests for each entry in the MODELS
/// array.
fn generate_tests_from_lisp_files() {
    let out_dir = std::env::var("OUT_DIR").unwrap();
    let limit = match env::var("CORSET_TEST_LIMIT") {
        Ok(s) => s.parse().unwrap(),
        Err(_) => 0,
    };
    let target = std::path::Path::new(&out_dir).join("lisp_tests.rs");
    let mut f = fs::File::create(target).unwrap();
    // Generate tests and inputs for each model.
    for m in MODELS {
        // Write out test runner
        writeln!(f).unwrap();
        writeln!(f, "#[test]").unwrap();
        writeln!(f, "fn test_{}() {{ check(\"{}\"); }}", m.name, m.name).unwrap();
        // Check whether oracle provided or not
        if m.oracle.is_some() {
            let limit = limit.max(m.limit);
            // Generate trace inputs (accepts / rejects)
            let (accepts, rejects) = m.generate_traces_upto(limit);
            // Write them out.
            write_traces(&m, "accepts", &accepts);
            write_traces(&m, "rejects", &rejects);
            println!(
                " Wrote {} / {} traces for {}.",
                accepts.len(),
                rejects.len(),
                m.name
            );
        }
    }
}

/// Write a set of zero or more traces into a file (whose is
/// determined by the model) with the given extension (which should be
/// either "accepts" or "rejects").
fn write_traces(m: &Model, ext: &str, traces: &[Trace]) {
    // Create output file
    let filename = format!("{}/{}.{}", TESTS_DIR, m.name, ext);
    let f = fs::File::create(filename).unwrap();
    // Write it all out
    for trace in traces {
        write_trace(&f, "<prelude>", &m.cols, &trace);
    }
    // Done
}

/// Write a specific trace to the output file.
fn write_trace<T: Write>(mut out: T, module: &str, cols: &[&str], trace: &Trace) -> T {
    let mut first = true;
    let _ = write!(out, "{{ \"{module}\": {{");
    for (i, col) in cols.iter().enumerate() {
        if !first {
            let _ = write!(out, ", ");
        }
        first = false;
        let _ = write!(out, "\"{col}\": {:?}", trace.get(i));
    }
    let _ = writeln!(out, "}} }}");
    out
}
