#[macro_use]
extern crate pest_derive;
use clap_verbosity_flag::Verbosity;
use is_terminal::IsTerminal;
use log::*;
use once_cell::sync::OnceCell;
use std::{io::Write, path::Path};

use clap::{Parser, Subcommand};
use color_eyre::eyre::*;

mod check;
mod column;
mod compiler;
mod compute;
mod expander;
mod exporters;
mod pretty;
mod utils;

#[derive(Default, Debug)]
struct Settings {
    pub full_trace: bool,
    pub trace_span: isize,
}

static SETTINGS: OnceCell<Settings> = OnceCell::new();

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

    #[clap(
        short = 't',
        long = "threads",
        help = "number of threds to use",
        default_value_t = 1,
        global = true
    )]
    threads: usize,

    #[clap(long = "no-stdlib")]
    no_stdlib: bool,

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

        #[clap(
            short = 'O',
            long = "out",
            help = "where to write the computed trace",
            required = true
        )]
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

        #[clap(
            short = 'F',
            long = "trace-full",
            help = "print all the module columns on error"
        )]
        full_trace: bool,

        #[clap(short = 'S', long = "trace-span", help = "", default_value_t = 3)]
        trace_span: isize,
    },
    /// Given a set of Corset files, compile them into a single file for faster later use
    Compile {
        #[clap(
            short = 'o',
            long = "out",
            required = true,
            help = "compiled Corset file to create"
        )]
        outfile: String,
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
    let mut settings: Settings = Default::default();

    rayon::ThreadPoolBuilder::new()
        .num_threads(args.threads)
        .build_global()
        .unwrap();

    let (ast, mut constraints) = if args.source.len() == 1
        && Path::new(&args.source[0])
            .extension()
            .map(|e| e == "bin")
            .unwrap_or(false)
    {
        info!("Loading Corset binary...");
        (
            Vec::new(),
            ron::from_str(
                &std::fs::read_to_string(&args.source[0])
                    .with_context(|| eyre!("while reading `{}`", &args.source[0]))?,
            )
            .with_context(|| eyre!("while parsing `{}`", &args.source[0]))?,
        )
    } else {
        info!("Parsing Corset source files...");
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
        compiler::make(inputs.as_slice())?
    };

    info!("Done.");

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
            expander::expand(&mut constraints)?;
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
            let outfile = outfile.as_ref().unwrap();

            expander::expand(&mut constraints)?;
            let _ = compute::compute(&tracefile, &mut constraints, false)
                .with_context(|| format!("while computing from `{}`", tracefile))?;

            let mut f = std::fs::File::create(&outfile)
                .with_context(|| format!("while creating `{}`", &outfile))?;

            constraints
                .write(&mut f)
                .with_context(|| format!("while writing to `{}`", &outfile))?;
        }
        Commands::Check {
            tracefile,
            full_trace,
            trace_span,
        } => {
            settings.full_trace = full_trace;
            settings.trace_span = trace_span;
            SETTINGS.set(settings).unwrap();

            if utils::is_file_empty(&tracefile)? {
                warn!("`{}` is empty, exiting", tracefile);
                return Ok(());
            }

            let _ = compute::compute(&tracefile, &mut constraints, true)
                .with_context(|| format!("while expanding `{}`", tracefile))?;
            check::check(
                &constraints,
                args.verbose.log_level_filter() >= log::Level::Warn
                    && std::io::stdout().is_terminal(),
            )
            .with_context(|| format!("while checking `{}`", tracefile))?;
            info!("{}: SUCCESS", tracefile)
        }
        Commands::Compile { outfile } => {
            std::fs::File::create(&outfile)
                .with_context(|| format!("while creating `{}`", &outfile))?
                .write_all(ron::to_string(&constraints).unwrap().as_bytes())
                .with_context(|| format!("while writing to `{}`", &outfile))?;
        }
    }

    Ok(())
}
