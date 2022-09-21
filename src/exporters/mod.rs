use log::*;

use crate::column::Column;

mod go;
mod latex;

pub use go::*;
pub use latex::*;

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
        error!("STDOUT:\n{}", std::str::from_utf8(&output.stdout).unwrap());
        error!("STDERR:\n{}", std::str::from_utf8(&output.stderr).unwrap());
    }
}
