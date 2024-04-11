use std::path::{Path, PathBuf};
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

/// Generate a Rust file containing a test for each file in the given
/// TEST_DIR.
fn generate_tests_from_lisp_files() {
    let out_dir = std::env::var("OUT_DIR").unwrap();
    let target = std::path::Path::new(&out_dir).join("lisp_tests.rs");
    let mut f = fs::File::create(target).unwrap();
    // Open reference test directory
    let dir = fs::read_dir(TESTS_DIR).unwrap();

    for e in dir {
        let p = e.as_ref().unwrap().path();
        let n = p.file_stem().unwrap().to_str().unwrap();
        //
        if p.extension().unwrap() == "lisp" {
            writeln!(f).unwrap();
            writeln!(f, "#[test]").unwrap();
            writeln!(f, "fn test_{n}() {{ check(\"{n}\"); }}").unwrap();
        }
    }
}
