#![cfg(feature = "cli")]
#[macro_use]
extern crate pest_derive;
use anyhow::*;
use compiler::parser::Ast;
use compiler::ConstraintSet;
use either::Either;
use log::*;
use logging_timer::time;
use owo_colors::OwoColorize;
use std::sync::RwLock;
use std::{
    io::{Read, Write},
    path::Path,
};
use transformer::{AutoConstraint, ExpansionLevel};

use clap::{Parser, Subcommand};

mod check;
mod column;
mod compiler;
mod compute;
mod constants;
mod dag;
mod errors;
#[cfg(test)]
mod evaluation_tests;
mod exporters;
mod formatter;
mod import;
#[cfg(feature = "inspector")]
mod inspect;
mod pretty;
mod structs;
#[cfg(test)]
mod tests;
mod transformer;
mod utils;

pub(crate) static IS_NATIVE: RwLock<bool> = RwLock::new(false);

#[derive(Parser)]
#[command(author, version = concat!(clap::crate_version!(), " ", std::env!("GIT_HASH"), " ", std::env!("SIMD_ENABLED")), propagate_version = true)]
pub struct Args {
    #[clap(flatten)]
    verbose: clap_verbosity_flag::Verbosity,

    #[arg(
        help = "Either a file or a string containing the Corset code to process",
        global = true
    )]
    source: Vec<String>,

    #[arg(short='e', action = clap::ArgAction::Count, help="perform various levels of expansion", global=true)]
    expand: u8,

    #[arg(long="auto-constraints", value_parser=["sorts", "nhood"], value_delimiter=',', global=true)]
    auto_constraints: Vec<String>,

    #[arg(long = "debug", help = "Compile code in debug mode", global = true)]
    debug: bool,

    #[arg(
        long,
        help = "generate binfile using Rusty Object Notation (RON) instead of JSON",
        global = true
    )]
    ron: bool,

    #[arg(
        short = 't',
        long = "threads",
        help = "number of threads to use",
        default_value_t = 1,
        global = true
    )]
    threads: usize,

    #[arg(
        long = "native",
        short = 'N',
        help = "execute computations in target Galois field",
        global = true
    )]
    native_arithmetic: bool,

    #[arg(long = "no-stdlib")]
    no_stdlib: bool,

    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    #[cfg(feature = "exporters")]
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
    #[cfg(feature = "exporters")]
    /// Produce a WizardIOP constraint system
    WizardIOP {
        #[arg(short = 'o', long = "out", help = "where to render the constraints")]
        out_filename: Option<String>,
    },
    #[cfg(feature = "exporters")]
    /// Export columns in a format usable by zkBesu
    Besu {
        #[arg(
            short = 'P',
            long = "package",
            required = true,
            help = "In which package the function will be generated"
        )]
        package: String,

        #[arg(
            short = 'c',
            long = "class",
            default_value = "Trace",
            help = "class name for generated trace file"
        )]
        class: String,

        #[arg(short = 'o', long = "out", help = "where to render the columns")]
        output_file_path: Option<String>,
    },
    #[cfg(feature = "conflater")]
    /// Export columns in a format usable by the trace conflater
    Conflater {
        #[arg(short = 'o', long = "out", help = "where to render the columns")]
        filename: Option<String>,
    },
    #[cfg(feature = "exporters")]
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
    Convert {
        #[arg(
            short = 'T',
            long = "trace",
            required = true,
            help = "the trace to convert"
        )]
        tracefile: String,

        #[arg(
            long = "exclude",
            help = "do not export these modules",
            value_delimiter = ','
        )]
        exclude: Option<Vec<String>>,

        #[arg(short = 'o', long = "out", help = "where to write the computed trace")]
        outfile: Option<String>,

        #[arg(short='F', long="format", help="output format", value_parser=["csv", "json", "lt"], default_value="sqlite")]
        format: String,
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
            long = "only",
            help = "only check these constraints",
            value_delimiter = ','
        )]
        only: Option<Vec<String>>,

        #[arg(long = "skip", help = "skip these constraints", value_delimiter = ',')]
        skip: Vec<String>,

        #[arg(
            long = "no-abort",
            help = "continue checking a constraint after it met an error"
        )]
        continue_on_error: bool,

        #[arg(short = 'r', long = "report", help = "detail the failing constraint")]
        report: bool,

        #[arg(
            short = 'u',
            long = "report-unclutter",
            help = "only display debug annotations for non-zero expressions in failing constraint",
            requires = "report"
        )]
        unclutter: bool,

        #[arg(
            short = 'd',
            long = "report-dim",
            help = "when reporting on failing constraints, dim expressions reducing to 0",
            requires = "report"
        )]
        dim: bool,

        #[arg(
            short = 's',
            long = "report-src",
            help = "display the original source code along its compiled form",
            requires = "report"
        )]
        with_src: bool,

        #[arg(short = 'S', long = "trace-span", help = "", default_value_t = 2)]
        trace_span: isize,

        #[arg(short = 'B', long = "trace-span-before", help = "")]
        trace_span_before: Option<isize>,

        #[arg(short = 'A', long = "trace-span-after", help = "")]
        trace_span_after: Option<isize>,
    },
    /// Inspect a trace file
    #[cfg(feature = "inspector")]
    Inspect {
        #[arg(
            short = 'T',
            long = "trace",
            required = true,
            help = "the trace to inspect"
        )]
        tracefile: String,

        #[arg(
            long = "open",
            short = 'o',
            help = "directly open the specified module"
        )]
        open_module: Option<String>,

        #[arg(long = "high-contrast", help = "avoid low-contrast colors")]
        high_contrast: bool,
    },
    /// Display the compiled the constraint system
    Debug {
        #[arg(
            short = 'm',
            long = "modules",
            help = "show modules and their properties"
        )]
        show_modules: bool,
        #[arg(short = 'n', long = "constants", help = "show constants")]
        show_constants: bool,
        #[arg(
            short = 'C',
            long = "columns",
            help = "show columns and their properties"
        )]
        show_columns: bool,
        #[arg(
            short = 'c',
            long = "constraints",
            help = "display constraint expressions"
        )]
        show_constraints: bool,
        #[arg(
            short = 'x',
            long = "computations",
            help = "display computed columns details"
        )]
        show_computations: bool,
        #[arg(
            short = 'p',
            long = "perspectives",
            help = "display perspective details"
        )]
        show_perspectives: bool,
        #[arg(
            short = 'R',
            long = "registers",
            help = "show regsters and their properties"
        )]
        show_registers: bool,
        #[arg(
            short = 's',
            long = "spilling",
            help = "display spilling for all modules"
        )]
        show_spilling: bool,
        #[arg(long = "stats", help = "display statistical information")]
        show_stats: bool,
        #[arg(short = 'T', long = "types", help = "display types information")]
        show_types: bool,
        #[arg(
            long = "only",
            help = "only show these constraints",
            value_delimiter = ',',
            requires = "show_constraints"
        )]
        only: Option<Vec<String>>,
        #[arg(
            long = "skip",
            help = "do not show these constraints",
            value_delimiter = ',',
            requires = "show_constraints"
        )]
        skip: Vec<String>,
        #[arg(
            long = "toml",
            help = "generate information (where applicable) in TOML format",
            requires = "show_spilling"
        )]
        toml: bool,
    },
    /// Format the given source in an idiomatic way
    Format {
        #[arg(
            short = 'i',
            long = "in-place",
            help = "format the given file in-place"
        )]
        inplace: bool,
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

        #[arg(long = "rm", help = "remove successfully validated blocks")]
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

type SourceMapping = Vec<(String, String)>;
struct ConstraintSetBuilder {
    debug: bool,
    no_stdlib: bool,
    source: Either<SourceMapping, ConstraintSet>,
    expand_to: ExpansionLevel,
    auto_constraints: Vec<AutoConstraint>,
}
impl ConstraintSetBuilder {
    fn from_sources(no_stdlib: bool, debug: bool) -> ConstraintSetBuilder {
        ConstraintSetBuilder {
            debug,
            no_stdlib,
            source: Either::Left(Vec::new()),
            expand_to: Default::default(),
            auto_constraints: Default::default(),
        }
    }

    fn from_bin(ron: bool, filename: &str) -> Result<ConstraintSetBuilder> {
        // Read the constraint-set bin file
        let contents = &std::fs::read_to_string(filename)
            .with_context(|| anyhow!("while reading `{}`", filename))?;
        // format.
        let cs = if ron {
            ron::from_str(contents)
                .with_context(|| anyhow!("while parsing `{}` (RON)", filename))?
        } else {
            serde_json::from_str(contents)
                .with_context(|| anyhow!("while parsing `{}` (JSON)", filename))?
        };
        //
        Ok(ConstraintSetBuilder {
            debug: false,
            no_stdlib: false,
            source: Either::Right(cs),
            expand_to: Default::default(),
            auto_constraints: Default::default(),
        })
    }

    fn expand_to(&mut self, to: ExpansionLevel) {
        self.expand_to = to;
    }

    fn auto_constraints(&mut self, auto: &[AutoConstraint]) {
        self.auto_constraints = auto.to_vec();
    }

    fn find_section(root: &Path, section: &str) -> Result<Option<SourceMapping>> {
        let section_file = root.join(format!("{}.lisp", section));
        let section_str = section_file.to_str().unwrap();
        // 1. Find a matching file
        if section_file.is_file() {
            let content = std::fs::read_to_string(&section_file)
                .with_context(|| anyhow!("reading {}", section_str.yellow().bold()))?;
            info!("adding {}", section_str.bright_white().bold());
            Ok(Some(vec![(
                section_file.to_str().unwrap().to_owned(),
                content,
            )]))
            // 2. Fail is the file is actually a directory
        } else if section_file.is_dir() {
            bail!(
                "expected {} to be a file, not a directory",
                section_file.to_str().unwrap().yellow().bold()
            )
        } else {
            // 3. Otherwise, repeat the process with a folder
            let section_dir = root.join(section);
            if section_dir.is_file() {
                bail!(
                    "expected {} to be a directory, not a file",
                    section_file.to_str().unwrap().yellow().bold()
                )
            } else if section_dir.is_dir() {
                let mut r = Vec::new();
                for entry in section_dir.read_dir().with_context(|| {
                    anyhow!(
                        "while reading {}",
                        section_dir.to_str().unwrap().yellow().bold()
                    )
                })? {
                    let p = entry?.path();
                    if p.extension()
                        .map(|ext| ["lisp", "corset"].contains(&ext.to_str().unwrap()))
                        .unwrap_or(false)
                    {
                        info!("adding {}", p.to_str().unwrap().bright_white().bold());
                        let content = std::fs::read_to_string(&p).with_context(|| {
                            anyhow!("reading {}", section_file.to_str().unwrap().yellow().bold())
                        })?;
                        r.push((p.file_name().unwrap().to_str().unwrap().to_owned(), content))
                    }
                }
                Ok(Some(r))
            } else {
                Ok(None)
            }
        }
    }

    fn parse_dir(dir: &Path) -> Result<SourceMapping> {
        let mut sources = Vec::new();
        let mut columns = Self::find_section(dir, "columns")?.ok_or_else(|| {
            anyhow!(
                "no columns found in {}",
                dir.to_str().unwrap().yellow().bold()
            )
        })?;
        sources.append(&mut columns);

        let mut constraints = Self::find_section(dir, "constraints")?.ok_or_else(|| {
            anyhow!(
                "no constraints found in {}",
                dir.to_str().unwrap().yellow().bold()
            )
        })?;
        sources.append(&mut constraints);

        if let Some(mut constants) = Self::find_section(dir, "constants")? {
            sources.append(&mut constants);
        }

        if let Some(mut lookups) = Self::find_section(dir, "lookups")? {
            sources.append(&mut lookups);
        }

        Ok(sources)
    }

    /// Add a source to the sources to compile:
    ///   - if it's a filename, add its content;
    ///   - if it's a path, tries to parse it following the standardized
    ///     hierarchy;
    ///   - if it's `-`, plug in STDIN;
    ///   - otherwise, just include it as an immediate expression.
    fn add_source(&mut self, src: &str) -> Result<()> {
        if let Either::Left(ref mut sources) = self.source {
            let as_path = std::path::Path::new(src);
            if as_path.is_dir() {
                sources.append(&mut Self::parse_dir(as_path)?);
            } else if as_path.is_file() {
                sources.push((
                    src.to_string(),
                    std::fs::read_to_string(src)
                        .with_context(|| anyhow!("reading {}", src.yellow().bold()))?,
                ));
            } else if src == "-" {
                let mut buffer = String::new();
                std::io::stdin().read_to_string(&mut buffer)?;
                sources.push(("STDIN".to_string(), buffer));
            } else {
                sources.push(("Immediate expression".to_string(), src.into()));
            }
            Ok(())
        } else {
            bail!("unable to push source to ConstraintSetBuilder built from compiled ConstraintSet")
        }
    }

    /// Pre-process the sources before compilation:
    ///   - insert the stdlib if it is enabled
    fn prepare_sources(&self, sources: &[(String, String)]) -> Vec<(String, String)> {
        let mut sources = sources.to_vec();
        if !self.no_stdlib {
            sources.insert(
                0,
                ("stdlib".to_string(), include_str!("stdlib.lisp").to_owned()),
            );
        }
        sources
    }

    /// Builds a simple AST that will be used by the formatter
    fn to_simple_ast(&self) -> Result<Vec<(String, Ast)>> {
        match self.source.as_ref() {
            Either::Left(sources) => {
                compiler::parser::parse_simple_ast(&self.prepare_sources(sources))
            }
            Either::Right(_) => bail!("unable to retrieve AST from compiled CponstraintSet"),
        }
    }

    /// Builds a standard AST that will be used for compilation
    fn to_ast(&self) -> Result<Vec<(String, Ast)>> {
        match self.source.as_ref() {
            Either::Left(sources) => compiler::parser::parse_ast(&self.prepare_sources(sources)),
            Either::Right(_) => bail!("unable to retrieve AST from compiled CponstraintSet"),
        }
    }

    #[time("info", "Compiling into constraint set")]
    fn into_constraint_set(self) -> Result<ConstraintSet> {
        let mut cs = match self.source {
            Either::Left(ref sources) => compiler::make(
                &self.prepare_sources(sources),
                &compiler::CompileSettings { debug: self.debug },
            )
            .map(|r| r.1),
            Either::Right(cs) => Ok(cs),
        }?;
        transformer::expand_to(&mut cs, self.expand_to, &self.auto_constraints)?;
        transformer::concretize(&mut cs);
        Ok(cs)
    }
}

#[cfg(feature = "cli")]
fn main() -> Result<()> {
    use crate::{inspect::InspectorSettings, transformer::concretize};

    let args = Args::parse();
    *crate::IS_NATIVE.write().unwrap() = args.native_arithmetic;
    buche::new()
        .verbosity(args.verbose.log_level_filter())
        .quiet(args.verbose.is_silent())
        .init()
        .unwrap();

    rayon::ThreadPoolBuilder::new()
        .num_threads(args.threads)
        .build_global()
        .unwrap();

    let mut builder = if matches!(args.command, Commands::Format { .. }) {
        if args.source.len() != 1 {
            bail!(
                "can only format one file at a time; found {}",
                args.source.len()
            )
        } else if args.source.len() == 1
            && Path::new(&args.source[0])
                .extension()
                .map(|e| e == "bin")
                .unwrap_or(false)
        {
            bail!("expected Corset source file, found compiled constraint set")
        } else {
            let mut r = ConstraintSetBuilder::from_sources(args.no_stdlib, args.debug);
            for f in args.source.iter() {
                r.add_source(f)?;
            }
            r
        }
    } else if args.source.len() == 1
        && Path::new(&args.source[0])
            .extension()
            .map(|e| e == "bin")
            .unwrap_or(false)
    {
        info!("Loading `{}`", &args.source[0]);
        ConstraintSetBuilder::from_bin(args.ron, &args.source[0])?
    } else {
        info!("Parsing Corset source files...");
        let mut r = ConstraintSetBuilder::from_sources(args.no_stdlib, args.debug);
        for f in args.source.iter() {
            r.add_source(f)?;
        }
        r
    };

    builder.expand_to(args.expand.into());
    builder.auto_constraints(&AutoConstraint::parse(&args.auto_constraints));

    match args.command {
        #[cfg(feature = "exporters")]
        Commands::Go { package, filename } => {
            exporters::zkgeth::render(
                &builder.into_constraint_set()?,
                &package,
                filename.as_ref(),
            )?;
        }
        #[cfg(feature = "exporters")]
        Commands::Besu {
            package,
            class,
            output_file_path: output_path,
        } => {
            exporters::besu::render(
                &builder.into_constraint_set()?,
                &package,
                &class,
                output_path.as_ref(),
            )?;
        }
        #[cfg(feature = "conflater")]
        Commands::Conflater { filename } => {
            exporters::conflater::render(&builder.to_constraint_set(), filename.as_ref())?;
        }
        #[cfg(feature = "exporters")]
        Commands::WizardIOP { out_filename } => {
            *crate::IS_NATIVE.write().unwrap() = true;
            builder.expand_to(ExpansionLevel::top());
            builder.auto_constraints(AutoConstraint::all());
            let mut cs = builder.into_constraint_set()?;
            concretize(&mut cs);

            exporters::wizardiop::render(&cs, &out_filename)?;
        }
        #[cfg(feature = "exporters")]
        Commands::Latex {
            constraints_filename,
        } => {
            exporters::latex::render(
                builder
                    .to_ast()?
                    .into_iter()
                    .map(|x| x.1)
                    .collect::<Vec<_>>()
                    .as_slice(),
                constraints_filename,
            )?;
        }
        Commands::Convert {
            tracefile,
            outfile,
            format,
            exclude,
        } => {
            let mut cs = builder.into_constraint_set()?;
            compute::compute_trace(&tracefile, &mut cs, false)
                .with_context(|| format!("while expanding `{}`", tracefile))?;

            match format.as_str() {
                "csv" => exporters::convert::to_csv(
                    &cs,
                    &exclude.unwrap_or_default(),
                    outfile.as_ref().map(String::as_str).unwrap_or("trace.csv"),
                ),
                "json" => exporters::convert::to_json(
                    &cs,
                    &exclude.unwrap_or_default(),
                    outfile.as_ref().map(String::as_str).unwrap_or("trace.json"),
                ),
                // "lt" => exporters::convert::to_lt(
                //     &cs,
                //     &exclude.unwrap_or_default(),
                //     outfile.as_ref().map(String::as_str).unwrap_or("trace.csv"),
                // ),
                _ => unreachable!(),
            }?;
        }
        Commands::Compute {
            tracefile,
            outfile,
            fail_on_missing,
        } => {
            builder.expand_to(ExpansionLevel::top());
            builder.auto_constraints(AutoConstraint::all());
            let mut cs = builder.into_constraint_set()?;

            compute::compute_trace(&tracefile, &mut cs, fail_on_missing)
                .with_context(|| format!("while computing from `{}`", tracefile))?;

            let outfile = outfile.as_ref().unwrap();
            let mut f = std::fs::File::create(outfile)
                .with_context(|| format!("while creating `{}`", &outfile))?;

            let mut out = std::io::BufWriter::with_capacity(10_000_000, &mut f);
            cs.write(&mut out)
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
            let mut constraints = builder.to_constraint_set()?;
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
                    let payload: &[u8] = row.get(2);
                    info!("Processing {}", id);

                    compute::compute_trace_str(
                        payload,
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
            report,
            only,
            skip,
            continue_on_error,
            unclutter,
            dim,
            with_src,
            trace_span,
            trace_span_before,
            trace_span_after,
        } => {
            if utils::is_file_empty(&tracefile)? {
                warn!("`{}` is empty, exiting", tracefile);
                return Ok(());
            }

            let mut cs = builder.into_constraint_set()?;

            compute::compute_trace(&tracefile, &mut cs, false)
                .with_context(|| format!("while expanding `{}`", tracefile))?;
            check::check(
                &cs,
                &only,
                &skip,
                check::DebugSettings::new()
                    .unclutter(unclutter)
                    .dim(dim)
                    .src(with_src)
                    .continue_on_error(continue_on_error)
                    .report(report)
                    .full_trace(full_trace)
                    .context_span(trace_span)
                    .and_context_span_before(trace_span_before)
                    .and_context_span_after(trace_span_after),
            )
            .with_context(|| format!("while checking {}", tracefile.bright_white().bold()))?;
            info!("{}: SUCCESS", tracefile)
        }
        #[cfg(feature = "inspector")]
        Commands::Inspect {
            tracefile,
            open_module,
            high_contrast,
        } => {
            if utils::is_file_empty(&tracefile)? {
                warn!("`{}` is empty, exiting", tracefile);
                return Ok(());
            }
            let mut cs = builder.into_constraint_set()?;

            compute::compute_trace(&tracefile, &mut cs, false)
                .with_context(|| format!("while expanding `{}`", tracefile))?;

            inspect::inspect(
                &cs,
                InspectorSettings {
                    open_module,
                    high_contrast,
                    blank_perspectives: true,
                },
            )
            .with_context(|| format!("while checking {}", tracefile.bright_white().bold()))?;
            info!("{}: SUCCESS", tracefile)
        }
        Commands::Debug {
            show_modules,
            show_constants,
            show_columns,
            show_constraints,
            show_computations,
            show_perspectives,
            show_registers,
            show_types,
            show_spilling,
            show_stats,
            only,
            skip,
            toml,
        } => {
            let cs = builder.into_constraint_set()?;

            exporters::debugger::debug(
                &cs,
                exporters::debugger::DebugSettings {
                    modules: show_modules,
                    constants: show_constants,
                    constraints: show_constraints,
                    columns: show_columns,
                    types: show_types,
                    perspectives: show_perspectives,
                    registers: show_registers,
                    computations: show_computations,
                    spilling: show_spilling,
                    stats: show_stats,
                    toml: toml,
                },
                only.as_ref(),
                &skip,
            )?;
        }
        Commands::Format { inplace } => {
            builder.no_stdlib = true;
            let asts = builder.to_simple_ast()?;
            for (filename, ast) in asts.iter() {
                let formatted = ast.format();
                if inplace {
                    std::fs::File::create(filename)?.write_all(formatted.as_bytes())?;
                } else {
                    println!("{}", formatted);
                }
            }
        }
        Commands::Compile { outfile, pretty } => {
            let constraints = builder.into_constraint_set()?;
            std::fs::File::create(&outfile)
                .with_context(|| format!("while creating `{}`", &outfile))?
                .write_all(
                    if args.ron && pretty {
                        ron::ser::to_string_pretty(&constraints, ron::ser::PrettyConfig::default())?
                    } else if args.ron {
                        ron::ser::to_string(&constraints)?
                    } else if pretty {
                        serde_json::to_string_pretty(&constraints)?
                    } else {
                        serde_json::to_string(&constraints)?
                    }
                    .as_bytes(),
                )
                .with_context(|| format!("while writing to `{}`", &outfile))?;
        }
    }

    Ok(())
}
