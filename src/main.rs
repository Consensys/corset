#[macro_use]
extern crate pest_derive;
use crate::parser::Transpiler;
use clap::Parser;
use color_eyre::eyre::*;
use std::fs::File;
use std::io::BufWriter;

mod go;
mod parser;

#[derive(Parser, Debug, Clone)]
#[clap(version)]
pub struct Args {
    #[clap(long = "CE", default_value = "CE")]
    columns_assignment: String,

    #[clap(
        short = 'F',
        long = "function-name",
        value_parser,
        help = "The name of the function to be generated"
    )]
    fname: String,

    #[clap(
        required = true,
        help = "Either a file or a string containing the Corset code to process"
    )]
    source: Vec<String>,

    #[clap(
        short = 'P',
        long = "package",
        required = true,
        help = "In which package the function will be generated"
    )]
    package: String,

    #[clap(
        short = 'o',
        long = "out",
        help = "If set, write the result to this file"
    )]
    out_file: Option<String>,

    #[clap(long = "no-stdlib")]
    no_stdlib: bool,
}

fn main() -> Result<()> {
    color_eyre::install()?;
    let args = Args::parse();

    let mut inputs = vec![];
    for f in args.source.iter() {
        inputs.push((
            f.as_str(),
            std::fs::read_to_string(f).with_context(|| eyre!("reading `{}`", f))?,
        ));
    }

    let constraints = parser::ConstraintsSet::from_sources(inputs.as_slice())?;

    let go_exporter = go::GoExporter {
        settings: args.clone(),
    };


    let stdout = std::io::stdout();
    let (out_to_file, out): (bool, BufWriter<Box<dyn std::io::Write>>) =
        if let Some(out_filename) = args.out_file.as_ref() {
            println!("Generating {}", out_filename);
            (true, BufWriter::with_capacity(30_000_000, Box::new(File::create(out_filename).with_context(|| eyre!("creating `{}`", out_filename))?)))
        } else {
            (false, BufWriter::new(Box::new(stdout.lock())))
        };
    go_exporter.render(&constraints, out)?;
    if out_to_file {
        print!("Running gofmt... ");
        let output = std::process::Command::new("gofmt")
            .args(["-w", args.out_file.as_ref().unwrap()])
            .output()
            .expect("failed to execute process");
        if output.status.success() {
            println!("done.");
        } else {
            println!("failed:");
            println!("STDOUT:\n{}", std::str::from_utf8(&output.stdout).unwrap());
            println!("STDERR:\n{}", std::str::from_utf8(&output.stderr).unwrap());
        }
    }

    Ok(())
}
