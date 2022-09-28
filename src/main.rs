#[macro_use]
extern crate pest_derive;
use clap_verbosity_flag::Verbosity;
use pairing_ce::ff::PrimeField;
use std::{collections::HashMap, io::Write};
use utils::export_symbol;

use clap::{Parser, Subcommand};
use color_eyre::eyre::*;
use serde_json::json;

mod check;
mod column;
mod compiler;
mod compute;
mod expander;
mod exporters;
mod utils;

#[derive(Parser)]
#[clap(author, version)]
#[clap(propagate_version = true)]
pub struct Args {
    #[clap(flatten)]
    verbose: Verbosity,

    #[clap(
        help = "Either a file or a string containing the Corset code to process",
        global = true
    )]
    source: Vec<String>,

    #[clap(long = "no-stdlib")]
    no_stdlib: bool,

    #[clap(
        short = 'E',
        long = "expand",
        help = "if true, expand INV computations",
        global = true
    )]
    expand: bool,

    #[clap(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Produce a Go-based constraint system
    Go {
        #[clap(
            short = 'C',
            long = "columns",
            help = "whether to render columns definition"
        )]
        render_columns: bool,

        #[clap(
            short = 'o',
            long = "constraints-file",
            help = "where to render the constraints"
        )]
        constraints_filename: Option<String>,

        #[clap(long = "columns-file", help = "where to render the columns")]
        columns_filename: Option<String>,

        #[clap(long = "assignment", default_value = "CE")]
        columns_assignment: String,

        #[clap(
            short = 'F',
            long = "function-name",
            value_parser,
            help = "The name of the function to be generated"
        )]
        fname: String,

        #[clap(
            short = 'P',
            long = "package",
            required = true,
            help = "In which package the function will be generated"
        )]
        package: String,
    },
    /// Produce a WizardIOP constraint system
    WizardIOP {
        #[clap(short = 'o', long = "out", help = "where to render the constraints")]
        out_filename: Option<String>,

        #[clap(
            short = 'P',
            long = "package",
            required = true,
            help = "In which package the function will be generated"
        )]
        package: String,
    },
    /// Produce a LaTeX file describing the constraints
    Latex {
        #[clap(
            short = 'o',
            long = "constraints-file",
            help = "where to render the constraints"
        )]
        constraints_filename: Option<String>,

        #[clap(long = "columns-file", help = "where to render the columns")]
        columns_filename: Option<String>,
    },
    /// Given a set of constraints and a trace file, fill the computed columns
    Compute {
        #[clap(
            short = 'T',
            long = "trace",
            required = true,
            help = "the trace to compute & verify"
        )]
        tracefile: String,

        #[clap(short = '0', long = "out", help = "where to write the computed trace")]
        outfile: Option<String>,
    },
    /// Given a set of constraints and a filled trace, check the validity of the constraints
    Check {
        #[clap(
            short = 'T',
            long = "trace",
            required = true,
            help = "the trace to compute & verify"
        )]
        tracefile: String,
    },
}

fn main() -> Result<()> {
    let args = Args::parse();
    color_eyre::install()?;
    simplelog::TermLogger::init(
        args.verbose.log_level_filter(),
        simplelog::ConfigBuilder::new()
            .set_time_level(simplelog::LevelFilter::Off)
            .build(),
        simplelog::TerminalMode::Stderr,
        simplelog::ColorChoice::Auto,
    )?;

    let mut inputs = vec![];
    if !args.no_stdlib {
        inputs.push(("stdlib", include_str!("stdlib.lisp").to_owned()));
    }
    for f in args.source.iter() {
        if std::path::Path::new(&f).is_file() {
            inputs.push((
                f.as_str(),
                std::fs::read_to_string(f).with_context(|| eyre!("reading `{}`", f))?,
            ));
        } else {
            inputs.push(("Immediate expression", f.into()));
        }
    }

    let (ast, mut constraints) = compiler::make(inputs.as_slice())?;
    if args.expand {
        expander::expand(&mut constraints)?;
    }

    match args.command {
        Commands::Go {
            constraints_filename,
            columns_filename,
            render_columns,
            package,
            columns_assignment,
            fname,
        } => {
            let mut go_exporter = exporters::GoExporter {
                constraints_filename,
                package,
                ce: columns_assignment,
                render_columns,
                columns_filename,
                fname,
            };
            go_exporter.render(&constraints)?;
        }
        Commands::WizardIOP {
            out_filename,
            package,
        } => {
            let mut wiop_exporter = exporters::WizardIOP {
                out_filename,
                package,
            };
            wiop_exporter.render(&constraints)?;
        }
        Commands::Latex {
            constraints_filename,
            columns_filename,
        } => {
            let mut latex_exporter = exporters::LatexExporter {
                constraints_filename,
                columns_filename,
                render_columns: true,
            };
            latex_exporter.render(&ast)?
        }
        Commands::Compute { tracefile, outfile } => {
            let r = compute::compute(&tracefile, &mut constraints)
                .with_context(|| format!("while computing from `{}`", tracefile))?;
            let stringified: HashMap<String, Vec<String>> = r
                .columns
                .into_iter()
                .map(|(k, v)| {
                    (
                        export_symbol(&k),
                        v.iter()
                            .map(|x| x.into_repr().to_string())
                            .collect::<Vec<_>>(),
                    )
                })
                .collect();
            let r = json!({ "columns": stringified }).to_string();

            if let Some(outfilename) = outfile.as_ref() {
                std::fs::File::create(outfilename)
                    .with_context(|| format!("while creating `{}`", outfilename))?
                    .write_all(r.as_bytes())
                    .with_context(|| format!("while writing to `{}`", outfilename))?;
            } else {
                println!("{}", r);
            }
        }
        Commands::Check { tracefile } => {
            let _ = compute::compute(&tracefile, &mut constraints)
                .with_context(|| format!("while computing from `{}`", tracefile))?;
            check::check(&constraints).with_context(|| {
                format!(
                    "while checking `{}` against {}",
                    tracefile,
                    inputs.iter().map(|x| x.0).collect::<Vec<_>>().join(", ")
                )
            })?;
        }
    }

    Ok(())
}
