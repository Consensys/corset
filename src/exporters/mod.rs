use log::*;

use crate::column::Column;

mod go;
mod latex;
mod wizardiop;

pub use go::*;
pub use latex::*;
pub use wizardiop::*;

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

fn goize(s: &str) -> String {
    s.replace('(', "_")
        .replace(')', "_")
        .replace('{', "_")
        .replace('}', "_")
        .replace('[', "_")
        .replace(']', "_")
        .replace('/', "_")
        .replace(':', "_")
        .replace('%', "_")
}
