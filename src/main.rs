#[macro_use]
extern crate pest_derive;
use crate::parser::Transpiler;
use clap::Parser;
use color_eyre::eyre::*;
use std::io::prelude::*;
use std::path::Path;

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
    source: String,

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

    let mut source = if Path::new(&args.source).is_file() {
        std::fs::read_to_string(&args.source)?
    } else {
        args.source.clone()
    };
    if !args.no_stdlib {
        source.push_str(include_str!("stdlib.lisp"))
    }

    let constraints = parser::ConstraintsSet::from_str(&source)
        .with_context(|| format!("while parsing `{}`", &args.source))?;

    let go_exporter = go::GoExporter {
        settings: args.clone(),
    };
    let out = go_exporter.render(&constraints)?;
    if let Some(out_file) = args.out_file {
        std::fs::File::create(&out_file)
            .with_context(|| {
                eyre!(
                    "while creating `{}` in `{}",
                    Path::new(&out_file).display(),
                    std::env::current_dir().unwrap().display()
                )
            })?
            .write_all(out.as_bytes())?;
        println!("{} generated", out_file);
    } else {
        println!("{}", out);
    }

    Ok(())
}
