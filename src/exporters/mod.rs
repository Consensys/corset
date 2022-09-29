use log::*;

mod go;
mod latex;
mod wizardiop;

pub use go::GoExporter;
pub use latex::*;
pub use wizardiop::WizardIOP;

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
