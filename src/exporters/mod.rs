use log::*;

#[cfg(feature = "exporters")]
pub mod besu;
#[cfg(feature = "conflater")]
pub mod conflater;
mod debugger;
#[cfg(feature = "exporters")]
pub mod go;
#[cfg(all(feature = "parser", feature = "exporters"))]
pub mod latex;
#[cfg(feature = "exporters")]
mod wizardiop;

pub use debugger::debug;
#[cfg(feature = "exporters")]
pub use wizardiop::WizardIOP;

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
