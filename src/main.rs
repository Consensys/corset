#[macro_use]
#[cfg(feature = "interactive")]
extern crate pest_derive;
use is_terminal::IsTerminal;
use log::*;
use std::{io::Write, path::Path};

use anyhow::{anyhow, Context, Result};
use clap::{Parser, Subcommand};

mod check;
mod column;
mod compiler;
mod compute;
mod dag;
mod errors;
mod exporters;
#[cfg_attr(
    all(target_arch = "x86_64", target_feature = "avx"),
    path = "import.rs"
)]
#[cfg_attr(not(all(target_arch = "x86_64")), path = "import.rs")]
mod import;
mod pretty;
mod structs;
#[cfg(test)]
mod tests;
mod transformer;
mod utils;

#[derive(Parser)]
#[command(author, version = concat!(clap::crate_version!(), " ", std::env!("GIT_HASH")), propagate_version = true)]
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
    /// Export columns in a format usable by zkGeth
    Go {
        #[arg(
            short = 'P',
            long = "package",
            required = true,
            help = "In which package the function will be generated"
        )]
        package: String,

        #[arg(
            short = 'o',
            long = "columns-file",
            help = "where to render the columns"
        )]
        filename: Option<String>,
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
    /// Export columns in a format usable by zkBesu
    Besu {
        #[arg(
            short = 'P',
            long = "package",
            required = true,
            help = "In which package the function will be generated"
        )]
        package: String,

        #[arg(short = 'o', long = "out", help = "where to render the columns")]
        filename: Option<String>,
    },
    /// Produce a LaTeX file describing the constraints
    Latex {
        #[arg(
            short = 'o',
            long = "constraints-file",
            help = "where to render the constraints"
        )]
        constraints_filename: Option<String>,
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

        #[arg(long, help = "exit on failing columns")]
        fail_on_missing: bool,
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
            long = "debug-unclutter",
            help = "only display debug annotations for non-zero expressions in failing constraint"
        )]
        unclutter: bool,

        #[arg(
            long = "debug-dim",
            help = "when reporting on failing constraints, dim expressions reducing to 0"
        )]
        dim: bool,

        #[arg(
            long = "only",
            help = "only check these constraints",
            value_delimiter = ','
        )]
        only: Option<Vec<String>>,

        #[arg(long = "skip", help = "skip these constraints", value_delimiter = ',')]
        skip: Vec<String>,

        #[arg(short = 'S', long = "trace-span", help = "", default_value_t = 2)]
        trace_span: isize,
    },
    /// Display the compiled the constraint system
    Debug {
        #[arg(
            short = 'E',
            long = "expand",
            help = "perform all expansion operations before checking"
        )]
        expand: bool,
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

        #[arg(long)]
        rerun: bool,

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

        #[arg(long, help = "human-readably serialize the constraint system")]
        pretty: bool,
    },
}

fn main() -> Result<()> {
    let args = Args::parse();
    buche::new()
        .verbosity(args.verbose.log_level_filter())
        .quiet(args.verbose.is_silent())
        .init()
        .unwrap();

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
    transformer::precompute(&mut constraints);

    match args.command {
        Commands::Go { package, filename } => {
            exporters::go::render(&constraints, &package, filename.as_ref())?;
        }
        Commands::Besu { package, filename } => {
            exporters::besu::render(&constraints, &package, filename.as_ref())?;
        }
        Commands::WizardIOP {
            out_filename,
            package,
        } => {
            // transformer::validate_nhood(&mut constraints)?;
            transformer::lower_shifts(&mut constraints);
            transformer::expand_ifs(&mut constraints);
            transformer::expand_constraints(&mut constraints)?;
            transformer::sorts(&mut constraints)?;
            transformer::expand_invs(&mut constraints)?;

            let mut wiop_exporter = exporters::WizardIOP {
                out_filename,
                package,
                sizes: Default::default(),
            };
            wiop_exporter.render(&constraints)?;
        }
        Commands::Latex {
            constraints_filename,
        } => {
            exporters::latex::render(ast.as_slice(), constraints_filename)?;
        }
        Commands::Compute {
            tracefile,
            outfile,
            fail_on_missing,
        } => {
            transformer::validate_nhood(&mut constraints)?;
            transformer::lower_shifts(&mut constraints);
            transformer::expand_ifs(&mut constraints);
            transformer::expand_constraints(&mut constraints)?;
            transformer::sorts(&mut constraints)?;
            transformer::expand_invs(&mut constraints)?;

            compute::compute_trace(&tracefile, &mut constraints, fail_on_missing)
                .with_context(|| format!("while computing from `{}`", tracefile))?;

            let outfile = outfile.as_ref().unwrap();
            let mut f = std::fs::File::create(outfile)
                .with_context(|| format!("while creating `{}`", &outfile))?;

            let mut out = std::io::BufWriter::with_capacity(10_000_000, &mut f);
            constraints
                .write(&mut out)
                .with_context(|| format!("while writing to `{}`", &outfile))?;
            out.flush()?;
        }
        #[cfg(feature = "postgres")]
        Commands::CheckLoop {
            host,
            user,
            password,
            database,
            remove,
            rerun,
            only,
            skip,
        } => {
            transformer::validate_nhood(&mut constraints)
                .with_context(|| anyhow!("while creating nhood constraints"))?;
            transformer::lower_shifts(&mut constraints);
            transformer::expand_ifs(&mut constraints);
            transformer::expand_constraints(&mut constraints)
                .with_context(|| anyhow!("while expanding constraints"))?;
            transformer::sorts(&mut constraints)
                .with_context(|| anyhow!("while creating sorting constraints"))?;
            transformer::expand_invs(&mut constraints)
                .with_context(|| anyhow!("while expanding inverses"))?;

            let mut db = utils::connect_to_db(&user, &password, &host, &database)?;

            info!("Initiating waiting loop");
            loop {
                let mut local_constraints = constraints.clone();

                let mut tx = db.transaction()?;
                let todo = if rerun { "failed" } else { "to_corset" };
                for row in tx.query(
                    &format!("SELECT id, status, payload FROM blocks WHERE STATUS='{}' ORDER BY length(payload) ASC LIMIT 1 FOR UPDATE SKIP LOCKED", todo),
                    &[],
                )? {
                    let id: &str = row.get(0);
                    let payload: &str = row.get(2);
                    info!("Processing {}", id);

                    compute::compute_trace(
                        &payload,
                        &mut local_constraints,
                        false,
                    )
                        .with_context(|| format!("while expanding from {}", id))?;

                    match check::check(
                        &local_constraints,
                        &only,
                        &skip,
                        args.verbose.log_level_filter() >= log::Level::Warn
                            && std::io::stdout().is_terminal(),
                        false,
                        check::DebugSettings::new()
                            .unclutter(true)
                            .report(args.verbose.log_level_filter() >= log::Level::Warn)
                    ) {
                        Ok(_) => {
                            if remove {
                                tx.execute("DELETE FROM blocks WHERE id=$1", &[&id])
                                    .with_context(|| "while inserting successful back row")?;
                            } else {
                                tx.execute("UPDATE blocks SET status='done' WHERE id=$1", &[&id])
                                    .with_context(|| "while inserting failed back row")?;
                            }
                        },
                        Err(_) => {
                            tx.execute("UPDATE blocks SET status='failed' WHERE id=$1", &[&id])
                                .with_context(|| "while inserting failed back row")?;
                        },
                    }

                }
                if let Err(e) = tx.commit() {
                    error!("{:?}", e);
                }

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
            unclutter,
            dim,
        } => {
            if utils::is_file_empty(&tracefile)? {
                warn!("`{}` is empty, exiting", tracefile);
                return Ok(());
            }

            if expand {
                transformer::validate_nhood(&mut constraints)
                    .with_context(|| anyhow!("while creating nhood constraints"))?;
                transformer::lower_shifts(&mut constraints);
                transformer::expand_ifs(&mut constraints);
                transformer::expand_constraints(&mut constraints)
                    .with_context(|| anyhow!("while expanding constraints"))?;
                transformer::sorts(&mut constraints)
                    .with_context(|| anyhow!("while creating sorting constraints"))?;
                transformer::expand_invs(&mut constraints)
                    .with_context(|| anyhow!("while expanding inverses"))?;
            }
            compute::compute_trace(&tracefile, &mut constraints, false)
                .with_context(|| format!("while expanding `{}`", tracefile))?;

            check::check(
                &constraints,
                &only,
                &skip,
                args.verbose.log_level_filter() >= log::Level::Warn
                    && std::io::stdout().is_terminal(),
                expand,
                check::DebugSettings::new()
                    .unclutter(unclutter)
                    .dim(dim)
                    .continue_on_error(continue_on_error)
                    .report(args.verbose.log_level_filter() >= log::Level::Warn)
                    .full_trace(full_trace)
                    .context_span(trace_span),
            )
            .with_context(|| format!("while checking `{}`", tracefile))?;
            info!("{}: SUCCESS", tracefile)
        }
        Commands::Debug { expand } => {
            if expand {
                transformer::validate_nhood(&mut constraints)
                    .with_context(|| anyhow!("while creating nhood constraints"))?;
                transformer::lower_shifts(&mut constraints);
                transformer::expand_ifs(&mut constraints);
                transformer::expand_constraints(&mut constraints)
                    .with_context(|| anyhow!("while expanding constraints"))?;
                transformer::sorts(&mut constraints)
                    .with_context(|| anyhow!("while creating sorting constraints"))?;
                transformer::expand_invs(&mut constraints)
                    .with_context(|| anyhow!("while expanding inverses"))?;
            }
            exporters::debug(&constraints)?;
        }
        Commands::Compile { outfile, pretty } => {
            std::fs::File::create(&outfile)
                .with_context(|| format!("while creating `{}`", &outfile))?
                .write_all(
                    if pretty {
                        ron::ser::to_string_pretty(&constraints, ron::ser::PrettyConfig::default())
                    } else {
                        ron::ser::to_string(&constraints)
                    }
                    .unwrap()
                    .as_bytes(),
                )
                .with_context(|| format!("while writing to `{}`", &outfile))?;
        }
    }

    Ok(())
}
