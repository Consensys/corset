use super::pretty::Pretty;
use colored::Colorize;
use pairing_ce::bn256::Fr;
use thiserror::Error;

use crate::{
    compiler::{Expression, Type},
    structs::Handle,
};

#[derive(Error, Debug)]
pub(crate) enum CompileError<'a> {
    #[error("{}", compiler::make_type_error_msg(.0, .1, .2))]
    TypeError(String, &'a [&'a [Type]], Vec<Type>),
    #[error("{} is never used", .0.pretty())]
    NotUsed(Handle),
}

#[derive(Error, Debug)]
pub enum RuntimeError<'a> {
    #[error("{} not found in the given trace", .0.pretty())]
    EmptyColumn(Handle),
    #[error("{} could not be computed", .0.pretty())]
    NotComputed(Handle),
    #[error("expected a {} value, found {}", .0.white().bold(), .1.pretty().red())]
    InvalidValue(&'a str, Fr),
    #[error("expected an array, found {:?}", .0)]
    NotAnArray(Expression),
}

pub mod parser {
    use colored::Colorize;

    pub fn make_src_error(src: &str, lc: (usize, usize)) -> String {
        let src_str = src
            .chars()
            .take_while(|x| *x != '\n')
            .collect::<String>()
            .bold()
            .bright_white()
            .to_string();

        format!(
            "at line {}: {}{}",
            lc.0.to_string().blue(),
            src_str,
            if src_str.len() < src.len() { "..." } else { "" }.bright_white()
        )
    }
}

pub(crate) mod compiler {
    use crate::compiler::Type;
    use colored::Colorize;

    pub(crate) fn make_type_error_msg(fname: &str, expected: &[&[Type]], found: &[Type]) -> String {
        let expected_str = format!(
            "({})",
            expected
                .iter()
                .cycle()
                .zip(found.iter())
                .map(|(es, f)| {
                    if es.iter().any(|e| e >= f) {
                        "..".into()
                    } else {
                        es.iter()
                            .map(|e| format!("{:?}", e))
                            .collect::<Vec<_>>()
                            .join("|")
                    }
                })
                .collect::<Vec<_>>()
                .join(" ")
                .blue()
                .bold()
        );
        let found_str = format!(
            "({})",
            expected
                .iter()
                .cycle()
                .zip(found.iter())
                .map(|(e, f)| {
                    if e.iter().any(|e| f <= e) {
                        "..".into()
                    } else {
                        format!("{:?}", f)
                    }
                })
                .collect::<Vec<_>>()
                .join(" ")
                .red()
                .bold()
        );

        format!(
            "{} expects {}, found {}",
            fname.yellow().bold(),
            expected_str,
            found_str
        )
    }
}

pub mod symbols {
    use colored::Colorize;
    use thiserror::Error;

    #[derive(Error, Debug)]
    pub enum Error {
        #[error("module {} not found in {}", .0.red(), .1.blue())]
        ModuleNotFound(String, String),
        #[error("symbol {} not found in module {}", .0.red(), .1.blue())]
        SymbolNotFound(String, String),
        #[error("{} already exists in {}", .0.yellow(), .1.blue())]
        SymbolAlreadyExists(String, String),
        #[error("function {} already defined in {}", .0.yellow(), .1.blue())]
        FunctionAlreadyExists(String, String),
        #[error("{} already exists: {} → {}", .0.yellow(), .0.red(), .1.magenta())]
        AliasAlreadyExists(String, String),
        #[error("ciruclar definition found for {}", .0.red())]
        CircularDefinition(String),
    }
}
