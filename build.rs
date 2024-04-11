use std::path::{PathBuf};
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
        if let Some(ext) = p.extension() {
	    if ext == "lisp" {
		writeln!(f).unwrap();
		writeln!(f, "#[test]").unwrap();
		writeln!(f, "fn test_{n}() {{ check(\"{n}\"); }}").unwrap();
		// 
		generate_traces_from_model(n);
	    }
        }
    }
}

fn generate_traces_from_model(name: &str) {
    let cols = &["X","Y"];
    let domain = Domain{min:-1, max: 1};
    //
    let filename = format!("{}/{name}.accepts",TESTS_DIR);
    let mut f = fs::File::create(filename).unwrap();
    for nrows in 0..3 {
	// Generate all possible traces with n rows.
	for trace_data in generate_trace_data(cols.len(),nrows,domain) {
	    let tr = generate_trace(cols, trace_data);
	    f = write_trace(f,"<prelude>",tr);
	}
    }
}

fn generate_trace_data(width: usize, height: usize, domain: Domain) -> Vec<Vec<isize>> {
    let mut tmp = vec![domain.min; (width * height)];
    let mut data = Vec::new();
    // Initial row
    data.push(tmp.clone());
    // Add remaining rows
    while next_trace(&mut tmp,domain) { data.push(tmp.clone()); }
    data
}

fn next_trace(data: &mut [isize], domain: Domain) -> bool {
    let mut i = 0;
    //
    while i < data.len() {
	if data[i] == domain.max {
	    data[i] = domain.min;
	    i = i + 1;	    
	} else {
	    data[i] += 1;
	    return true;
	}
    }
    // no more
    return false;
}

fn generate_trace(cols: &[&str], data: Vec<isize>) -> Vec<(String,Vec<isize>)> {
    let mut tr = Vec::new();
    // Determine how many rows we're expected
    let n = data.len() / cols.len();
    let mut i = 0;
    for col in cols {
	let j = i + n;
	tr.push((col.to_string(), data[i .. j].to_vec()));
	i += n;
    }
    tr
}

fn write_trace<T:Write>(mut out: T, module: &str, trace: Vec<(String,Vec<isize>)>) -> T {
    let mut first = true;
    write!(out, "{{ \"{module}\": {{");    
    for (col, data) in trace.into_iter() {
	if !first { write!(out, ", "); }
	first = false;
	write!(out, "\"{col}\": {data:?}");
    }
    writeln!(out, "}} }}");
    out
}

// Identifies a set of field elements.  This set could be constructed
// by enumerating all possible elements within a give range, or by
// sampling from the set of all elements, etc.
#[derive(Clone,Copy,Debug)]
struct Domain {
    pub min: isize,
    pub max: isize
}

impl Domain {
    pub fn size(&self) -> usize {
	(self.max - self.min) as usize
    }
}

impl Iterator for Domain {
    type Item = isize;
    
    fn next(&mut self) -> Option<isize> {
	todo!()
    }
}
