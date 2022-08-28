#[macro_use] extern crate pest_derive;
use color_eyre::eyre::*;
use clap::Parser;

mod parser;

#[derive(Parser, Debug, Clone)]
#[clap(version)]
pub struct Args {
   /// Name of the person to greet
   #[clap(long = "CE", value_parser, default_value="CE")]
   columns_assignment: String,

   /// Number of times to greet
   #[clap(short, long, value_parser, default_value_t = 1)]
   count: u8,
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
      (and
        (sub (mul 2 SUX) 1)
        (sub DELTA HEIGHT))
      SUX))

(= 3 4)
";
    // dbg!(&test)
    println!("Result:");
    let constraints = parser::ConstraintsSet::from_str(constraint, &args)?;
    println!("{:?}", constraints.render());

    Ok(())
}
