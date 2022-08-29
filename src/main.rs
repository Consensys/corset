#[macro_use]
extern crate pest_derive;
use clap::Parser;
use color_eyre::eyre::*;
use std::io::prelude::*;
use std::path::Path;
use crate::parser::Exporter;

mod go;
mod parser;

#[derive(Parser, Debug, Clone)]
#[clap(version)]
pub struct Args {
    #[clap(long = "CE", default_value = "CE")]
    columns_assignment: String,

    #[clap(short, long, value_parser)]
    name: String,

    #[clap(required = true)]
    source: String,

    #[clap(short = 'P', long = "package", required = true)]
    package: String,

    #[clap(short = 'o', long = "out")]
    out_file: Option<String>,
}

fn main() -> Result<()> {
    if std::env::var("RUST_SPANTRACE").is_err() {
        std::env::set_var("RUST_SPANTRACE", "0");
    }
    color_eyre::install()?;
    let args = Args::parse();

    let constraints = if Path::new(&args.source).is_file() {
        parser::ConstraintsSet::from_file(&args.source, &args)
    } else {
        parser::ConstraintsSet::from_str(&args.source, &args)
    }?;

    let go_exporter = go::GoExporter{settings: args.clone()};
    let out = go_exporter.render(&constraints)?;
    if let Some(out_file) = args.out_file {
        std::fs::File::create(&out_file)?.write_all(out.as_bytes())?;
        println!("{} generated", out_file);
    } else {
        println!("{}", out);
    }

    Ok(())
}
