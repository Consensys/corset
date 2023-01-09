#[macro_use]
#[cfg(feature = "interactive")]
extern crate pest_derive;
use flate2::read::GzDecoder;
use is_terminal::IsTerminal;
use log::*;
use once_cell::sync::OnceCell;
use serde_json::Value;
use std::{
    fs::File,
    io::{BufReader, Seek, Write},
    path::Path,
};

use anyhow::{anyhow, Context, Result};
use clap::{Parser, Subcommand};

mod check;
mod column;
mod compiler;
mod compute;
mod expander;
mod exporters;
mod pretty;
#[cfg(test)]
mod tests;
mod utils;

#[derive(Default, Debug)]
struct Settings {
    pub full_context: bool,
    pub context_span: isize,
}

static SETTINGS: OnceCell<Settings> = OnceCell::new();

#[derive(Parser)]
#[command(author, version, propagate_version = true)]
pub struct Args {
    #[clap(flatten)]
    verbose: clap_verbosity_flag::Verbosity,

    #[arg(
        help = "Either a file or a string containing the Corset code to process",
        global = true
    )]
    source: Vec<String>,

    #[arg(long = "debug", help = "Compile code in debug mode", global = true)]
    debug: bool,

    #[arg(
        long = "allow-dups",
        help = "Whether to allow re-declaration of symbols",
        global = true
    )]
    allow_dups: bool,

    #[arg(
        short = 't',
        long = "threads",
        help = "number of threads to use",
        default_value_t = 1,
        global = true
    )]
    threads: usize,

    #[arg(long = "no-stdlib")]
    no_stdlib: bool,

    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Produce a Go-based constraint system
    Go {
        #[arg(
            short = 'C',
            long = "columns",
            help = "whether to render columns definition"
        )]
        render_columns: bool,

        #[arg(
            short = 'o',
            long = "constraints-file",
            help = "where to render the constraints"
        )]
        constraints_filename: Option<String>,

        #[arg(long = "columns-file", help = "where to render the columns")]
        columns_filename: Option<String>,

        #[arg(long = "assignment", default_value = "CE")]
        columns_assignment: String,

        #[arg(
            short = 'F',
            long = "function-name",
            help = "The name of the function to be generated"
        )]
        fname: String,

        #[arg(
            short = 'P',
            long = "package",
            required = true,
            help = "In which package the function will be generated"
        )]
        package: String,
    },
    /// Produce a WizardIOP constraint system
    WizardIOP {
        #[arg(short = 'o', long = "out", help = "where to render the constraints")]
        out_filename: Option<String>,

        #[arg(
            short = 'P',
            long = "package",
            required = true,
            help = "In which package the function will be generated"
        )]
        package: String,
    },
    /// Produce a LaTeX file describing the constraints
    Latex {
        #[arg(
            short = 'o',
            long = "constraints-file",
            help = "where to render the constraints"
        )]
        constraints_filename: Option<String>,

        #[arg(long = "columns-file", help = "where to render the columns")]
        columns_filename: Option<String>,
    },
    /// Given a set of constraints and a trace file, fill the computed columns
    Compute {
        #[arg(
            short = 'T',
            long = "trace",
            required = true,
            help = "the trace to compute & verify"
        )]
        tracefile: String,

        #[arg(
            short = 'o',
            long = "out",
            help = "where to write the computed trace",
            required = true
        )]
        outfile: Option<String>,
    },
    /// Given a set of constraints, indefinitely fill the computed columns from/to an SQL table
    #[cfg(feature = "postgres")]
    ComputeLoop {
        #[arg(long, default_value = "localhost")]
        host: String,
        #[arg(long, default_value = "postgres")]
        user: String,
        #[arg(long)]
        password: Option<String>,
        #[arg(long, default_value = "zkevm")]
        database: String,
    },
    /// Given a set of constraints and a filled trace, check the validity of the constraints
    Check {
        #[arg(
            short = 'T',
            long = "trace",
            required = true,
            help = "the trace to compute & verify"
        )]
        tracefile: String,

        #[arg(
            short = 'F',
            long = "trace-full",
            help = "print all the module columns on error"
        )]
        full_trace: bool,

        #[arg(
            short = 'E',
            long = "expand",
            help = "perform all expansion operations before checking"
        )]
        expand: bool,

        #[arg(
            long = "no-abort",
            help = "continue checking a constraint after it met an error"
        )]
        continue_on_error: bool,

        #[arg(
            long = "only",
            help = "only check these constraints",
            value_delimiter = ','
        )]
        only: Option<Vec<String>>,

        #[arg(long = "skip", help = "skip these constraints", value_delimiter = ',')]
        skip: Vec<String>,

        #[arg(short = 'S', long = "trace-span", help = "", default_value_t = 3)]
        trace_span: isize,
    },
    /// Given a set of constraints, indefinitely check the traces from an SQL table
    #[cfg(feature = "postgres")]
    CheckLoop {
        #[arg(long, default_value = "localhost")]
        host: String,
        #[arg(long, default_value = "postgres")]
        user: String,
        #[arg(long)]
        password: Option<String>,
        #[arg(long, default_value = "zkevm")]
        database: String,
        #[arg(long = "rm", help = "remove succesully validated blocks")]
        remove: bool,

        #[arg(
            long = "only",
            help = "only check these constraints",
            value_delimiter = ','
        )]
        only: Option<Vec<String>>,

        #[arg(long = "skip", help = "skip these constraints", value_delimiter = ',')]
        skip: Vec<String>,
    },
    /// Given a set of Corset files, compile them into a single file for faster later use
    Compile {
        #[arg(
            short = 'o',
            long = "out",
            required = true,
            help = "compiled Corset file to create"
        )]
        outfile: String,
    },
}

fn read_trace<S: AsRef<str>>(tracefile: S) -> Result<Value> {
    let tracefile = tracefile.as_ref();
    info!("Parsing {}...", tracefile);
    let mut f = File::open(tracefile).with_context(|| format!("while opening `{}`", tracefile))?;

    let gz = GzDecoder::new(BufReader::new(&f));
    let v: Value = match gz.header() {
        Some(_) => serde_json::from_reader(gz),
        None => {
            f.rewind()?;
            serde_json::from_reader(BufReader::new(&f))
        }
    }
    .with_context(|| format!("while reading `{}`", tracefile))?;
    Ok(v)
}

fn main() -> Result<()> {
    let args = Args::parse();
    stderrlog::new()
        .verbosity(args.verbose.log_level_filter())
        .quiet(args.verbose.is_silent())
        .init()
        .unwrap();
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
        info!("Loading `{}`", &args.source[0]);
        (
            Vec::new(),
            ron::from_str(
                &std::fs::read_to_string(&args.source[0])
                    .with_context(|| anyhow!("while reading `{}`", &args.source[0]))?,
            )
            .with_context(|| anyhow!("while parsing `{}`", &args.source[0]))?,
        )
    } else {
        #[cfg(feature = "interactive")]
        {
            info!("Parsing Corset source files...");
            let mut inputs = vec![];
            if !args.no_stdlib {
                inputs.push(("stdlib", include_str!("stdlib.lisp").to_owned()));
            }
            for f in args.source.iter() {
                if std::path::Path::new(&f).is_file() {
                    inputs.push((
                        f.as_str(),
                        std::fs::read_to_string(f).with_context(|| anyhow!("reading `{}`", f))?,
                    ));
                } else {
                    inputs.push(("Immediate expression", f.into()));
                }
            }
            compiler::make(
                inputs.as_slice(),
                &compiler::CompileSettings {
                    debug: args.debug,
                    allow_dups: args.allow_dups,
                },
            )?
        }

        #[cfg(not(feature = "interactive"))]
        {
            panic!("Compile Corset with the `interactive` feature to enable the compiler")
        }
    };

    match args.command {
        Commands::Go {
            constraints_filename,
            columns_filename,
            render_columns,
            package,
            columns_assignment,
            fname,
        } => {
            expander::expand_ifs(&mut constraints);
            expander::lower_shifts(&mut constraints);
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
            expander::expand_ifs(&mut constraints);
            expander::lower_shifts(&mut constraints);
            expander::expand_constraints(&mut constraints)?;
            expander::expand_invs(&mut constraints)?;
            let mut wiop_exporter = exporters::WizardIOP {
                out_filename,
                package,
                sizes: Default::default(),
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

            expander::expand_ifs(&mut constraints);
            expander::lower_shifts(&mut constraints);
            expander::expand_constraints(&mut constraints)?;
            expander::expand_invs(&mut constraints)?;
            compute::compute(
                &read_trace(&tracefile)?,
                &mut constraints,
                compiler::PaddingStrategy::None,
            )
            .with_context(|| format!("while computing from `{}`", tracefile))?;

            let mut f = std::fs::File::create(&outfile)
                .with_context(|| format!("while creating `{}`", &outfile))?;

            constraints
                .write(&mut f)
                .with_context(|| format!("while writing to `{}`", &outfile))?;
        }
        #[cfg(feature = "postgres")]
        Commands::ComputeLoop {
            host,
            user,
            password,
            database,
        } => {
            use flate2::write::GzEncoder;
            use flate2::Compression;

            expander::expand_ifs(&mut constraints);
            expander::lower_shifts(&mut constraints);
            expander::expand_constraints(&mut constraints)?;
            expander::expand_invs(&mut constraints)?;
            let mut db = utils::connect_to_db(&user, &password, &host, &database)?;

            loop {
                let mut local_constraints = constraints.clone();

                let mut tx = db.transaction()?;
                for row in tx.query(
                    "SELECT id, status, payload FROM blocks WHERE STATUS='to_corset' LIMIT 1 FOR UPDATE SKIP LOCKED",
                    &[],
                )? {
                    let id: &str = row.get(0);
                    let payload: &[u8] = row.get(2);
                    info!("Processing {}", id);

                    let v: Value = serde_json::from_str(
                        &utils::decompress(payload).with_context(|| "while decompressing payload")?,
                    )?;

                    compute::compute(&v, &mut local_constraints, compiler::PaddingStrategy::None)
                        .with_context(|| "while computing columns")?;

                    let mut e = GzEncoder::new(Vec::new(), Compression::default());
                    local_constraints.write(&mut e)?;

                    tx.execute("UPDATE blocks SET payload=$1, status='to_prover' WHERE id=$2", &[&e.finish()?, &id])
                        .with_context(|| "while inserting back row")?;
                }
                tx.commit()?;

                std::thread::sleep(std::time::Duration::from_secs(1));
            }
        }
        #[cfg(feature = "postgres")]
        Commands::CheckLoop {
            host,
            user,
            password,
            database,
            remove,
            only,
            skip,
        } => {
            SETTINGS.set(settings).unwrap();

            let mut db = utils::connect_to_db(&user, &password, &host, &database)?;

            info!("Initiating waiting loop");
            loop {
                let mut local_constraints = constraints.clone();

                let mut tx = db.transaction()?;
                for row in tx.query(
                    "SELECT id, status, payload FROM blocks WHERE STATUS='to_corset' LIMIT 1 FOR UPDATE SKIP LOCKED",
                    &[],
                )? {
                    let id: &str = row.get(0);
                    let payload: &[u8] = row.get(2);
                    info!("Processing {}", id);

                    let gz = GzDecoder::new(std::io::Cursor::new(&payload));
                    let v: Value = match gz.header() {
                        Some(_) => serde_json::from_reader(gz),
                        None => {
                            serde_json::from_reader(std::io::Cursor::new(&payload))
                        }
                    }
                    .with_context(|| format!("while reading payload from {}", id))?;

                    compute::compute(
                        &v,
                        &mut local_constraints,
                        compiler::PaddingStrategy::OneLine,
                    )
                        .with_context(|| format!("while expanding from {}", id))?;

                    match check::check(
                        &local_constraints,
                        &only,
                        &skip,
                        args.verbose.log_level_filter() >= log::Level::Warn
                            && std::io::stdout().is_terminal(),
                    ) {
                        Ok(_) => {
                            if remove {
                                tx.execute("DELETE FROM blocks WHERE id=$1", &[&id])
                                    .with_context(|| "while inserting successful back row")?;
                            }
                        },
                        Err(_) => {
                            tx.execute("UPDATE blocks SET status='failed' WHERE id=$1", &[&id])
                                .with_context(|| "while inserting failed back row")?;
                        },
                    }

                }
                tx.commit()?;

                std::thread::sleep(std::time::Duration::from_secs(1));
            }
        }
        Commands::Check {
            tracefile,
            full_trace,
            trace_span,
            expand,
            only,
            skip,
            continue_on_error,
        } => {
            settings.full_context = full_trace;
            settings.context_span = trace_span;
            SETTINGS.set(settings).unwrap();

            if utils::is_file_empty(&tracefile)? {
                warn!("`{}` is empty, exiting", tracefile);
                return Ok(());
            }

            if expand {
                expander::lower_shifts(&mut constraints);
                expander::expand_ifs(&mut constraints);
                expander::expand_constraints(&mut constraints)?;
                expander::expand_invs(&mut constraints)?;
            }
            compute::compute(
                &read_trace(&tracefile)?,
                &mut constraints,
                compiler::PaddingStrategy::OneLine,
            )
            .with_context(|| format!("while expanding `{}`", tracefile))?;

            check::check(
                &constraints,
                &only,
                &skip,
                args.verbose.log_level_filter() >= log::Level::Warn
                    && std::io::stdout().is_terminal(),
                continue_on_error,
                args.verbose.log_level_filter() >= log::Level::Warn,
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
