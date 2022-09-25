#[macro_use]
extern crate pest_derive;
use clap::{Parser, Subcommand};
use color_eyre::eyre::*;

mod column;
mod compiler;
mod compute;
mod expander;
mod exporters;

#[derive(Parser)]
#[clap(author, version)]
#[clap(propagate_version = true)]
pub struct Args {
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

    #[clap(
        short = 'o',
        long = "constraints-file",
        help = "where to render the constraints",
        global = true
    )]
    constraints_filename: Option<String>,
    #[clap(
        short = 'C',
        long = "columns",
        help = "whether to render columns definition",
        global = true
    )]
    render_columns: bool,
    #[clap(
        long = "columns-file",
        help = "where to render the columns",
        global = true
    )]
    columns_filename: Option<String>,
}

#[derive(Subcommand)]
enum Commands {
    /// Produce a Go-based constraint system
    Go {
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
        #[clap(
            short = 'P',
            long = "package",
            required = true,
            help = "In which package the function will be generated"
        )]
        package: String,
    },
    /// Produce a LaTeX file describing the constraints
    Latex {},
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
}

fn main() -> Result<()> {
    color_eyre::install()?;
    simplelog::TermLogger::init(
        simplelog::LevelFilter::Trace,
        simplelog::ConfigBuilder::new()
            .set_time_level(simplelog::LevelFilter::Off)
            .build(),
        simplelog::TerminalMode::Stderr,
        simplelog::ColorChoice::Auto,
    )?;
    let args = Args::parse();

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

    match &args.command {
        Commands::Go {
            package,
            columns_assignment,
            fname,
        } => {
            let mut go_exporter = exporters::GoExporter {
                constraints_filename: args.constraints_filename,
                package: package.clone(),
                ce: columns_assignment.into(),
                render_columns: args.render_columns,
                columns_filename: args.columns_filename,
                fname: fname.to_owned(),
            };
            go_exporter.render(&constraints)?;
        }
        Commands::WizardIOP { package } => {
            let mut wiop_exporter = exporters::WizardIOP {
                out_filename: args.constraints_filename,
                package: package.clone(),
            };
            wiop_exporter.render(&constraints)?;
        }
        Commands::Latex {} => {
            let mut latex_exporter = exporters::LatexExporter {
                constraints_filename: args.constraints_filename,
                columns_filename: args.columns_filename,
                render_columns: args.render_columns,
            };
            latex_exporter.render(&ast)?
        }
        Commands::Compute { tracefile, outfile } => {
            compute::compute(tracefile, &mut constraints, outfile.clone())
                .with_context(|| format!("while computing from `{}`", tracefile))?;
        }
    }

    Ok(())
}
