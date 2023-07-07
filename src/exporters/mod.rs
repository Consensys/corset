use log::*;

#[cfg(feature = "exporters")]
pub mod besu;
#[cfg(feature = "conflater")]
pub mod conflater;
pub(crate) mod debugger;
#[cfg(all(feature = "parser", feature = "exporters"))]
pub mod latex;
#[cfg(feature = "exporters")]
pub mod wizardiop;
#[cfg(feature = "exporters")]
pub mod zkgeth;

use crate::column::Register;

fn reg_to_string(r: &Register, i: usize) -> String {
    r.handle
        .as_ref()
        .map(|h| h.mangled_name())
        .unwrap_or_else(|| format!("r{}", i))
}

#[cfg(feature = "exporters")]
fn gofmt(filename: &str) {
    info!("Running gofmt on {}... ", filename);
    let output = std::process::Command::new("gofmt")
        .args(["-w", filename])
        .output()
        .expect("failed to execute gofmt");
    if output.status.success() {
        info!("done.");
    } else {
        error!("failed:");
        eprintln!("{}", std::str::from_utf8(&output.stdout).unwrap());
        eprintln!("{}", std::str::from_utf8(&output.stderr).unwrap());
    }
}
