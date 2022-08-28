#[macro_use] extern crate pest_derive;
use color_eyre::eyre::*;
use clap::Parser;
use std::path::Path;

mod parser;

#[derive(Parser, Debug, Clone)]
#[clap(version)]
pub struct Args {
    #[clap(long = "CE", default_value="CE")]
    columns_assignment: String,

    #[clap(long, value_parser, default_value="constraint")]
    name: String,

    #[clap(required=true, default_value="constraint")]
    source: String,
}


fn main() -> Result<()> {
    if std::env::var("RUST_SPANTRACE").is_err() {
        std::env::set_var("RUST_SPANTRACE", "0");
    }
    color_eyre::install()?;
    let args = Args::parse();



    let constraint = "
(eq HU
    (sub
      (and ;; Comment
        (sub (mul 2 SUX) 1)
        (sub DELTA HEIGHT))
      SUX))
";
    let constraints = if Path::new(&args.source).is_file() {
        parser::ConstraintsSet::from_file(&args.source, &args)
    } else {
        parser::ConstraintsSet::from_str(constraint, &args)
    }?;




    println!("{:?}", constraints.render());

    Ok(())
}
