use super::pretty::Pretty;
use crate::{column::Value, pretty::Base};
use owo_colors::OwoColorize;
use thiserror::Error;

use crate::{
    compiler::{Expression, Type},
    structs::Handle,
};

#[derive(Error, Debug)]
pub(crate) enum CompileError<'a> {
    #[error("{}", compiler::make_type_error_msg(.0, .1, .2))]
    TypeError(String, &'a [&'a [Type]], Vec<Type>),

    #[error("{} expects a condition, found {}", .0, .1.red().bold())]
    ConditioningError(String, Type),

    #[error("{} is never used", .0.pretty())]
    NotUsed(Handle),

    #[error("column {} not found", .0.pretty())]
    NotFound(Handle),
}

#[derive(Error, Debug)]
pub enum RuntimeError {
    #[error("{} not found in the given trace", .0.pretty())]
    EmptyColumn(Handle),

    #[error("{} has not been computed", .0.pretty())]
    NotComputed(Handle),

    #[error("expected a {} value, found {}", .0.white().bold(), .1.pretty_with_base(Base::Hex).red())]
    InvalidValue(&'static str, Value),

    #[error("expected an array, found {:?}", .0)]
    NotAnArray(Expression),
}

pub mod parser {
    use owo_colors::OwoColorize;

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
    use itertools::Itertools;
    use owo_colors::OwoColorize;
    use thiserror::Error;

    #[derive(Error, Debug)]
    pub enum Error {
        #[error("constraint {} refers to non-ID columns", .0)]
        ConstraintWithHandles(String),

        #[error("computation {} refers to non-ID columns", .0)]
        ComputationWithHandles(String),
    }

    pub(crate) fn type_comparison_message(expected: &[Type], found: &[Type]) -> (String, String) {
        if found.len() != expected.len() {
            return (
                format!("{} arguments", expected.len()).blue().to_string(),
                format!("{} arguments", found.len()).red().to_string(),
            );
        }
        let expected_str = format!(
            "({})",
            expected
                .iter()
                .cycle()
                .zip(found.iter())
                .map(|(e, f)| {
                    if e >= f {
                        e.white().to_string()
                    } else {
                        e.blue().to_string()
                    }
                })
                .join(" ")
                .bold()
        );
        let found_str = format!(
            "({})",
            expected
                .iter()
                .cycle()
                .zip(found.iter())
                .map(|(e, f)| {
                    if e >= f {
                        f.white().to_string()
                    } else {
                        f.red().to_string()
                    }
                })
                .join(" ")
                .red()
                .bold()
        );
        (expected_str, found_str)
    }

    pub(crate) fn cyclic_type_comparison_message(
        expected: &[&[Type]],
        found: &[Type],
    ) -> (String, String) {
        let expected_str = format!(
            "({})",
            expected
                .iter()
                .cycle()
                .chain(std::iter::repeat(expected.last().unwrap()))
                .zip(found.iter())
                .map(|(es, f)| {
                    let es_str = es
                        .iter()
                        .map(|e| e.to_string())
                        .collect::<Vec<_>>()
                        .join("|");
                    if es.iter().any(|e| f.can_cast_to(*e)) {
                        es_str.white().to_string()
                    } else {
                        es_str.blue().to_string()
                    }
                })
                .join(" ")
                .blue()
                .bold()
        );
        let found_str = format!(
            "({})",
            expected
                .iter()
                .cycle()
                .chain(std::iter::repeat(expected.last().unwrap()))
                .zip(found.iter())
                .map(|(e, f)| {
                    if e.iter().any(|e| f.can_cast_to(*e)) {
                        f.white().to_string()
                    } else {
                        f.red().to_string()
                    }
                })
                .collect::<Vec<_>>()
                .join(" ")
                .red()
                .bold()
        );
        (expected_str, found_str)
    }

    pub(crate) fn make_type_error_msg(fname: &str, expected: &[&[Type]], found: &[Type]) -> String {
        let (expected_str, found_str) = cyclic_type_comparison_message(expected, found);
        format!(
            "{} expects {}, found {}",
            fname.yellow().bold(),
            expected_str,
            found_str
        )
    }
}

pub mod symbols {
    use owo_colors::OwoColorize;
    use thiserror::Error;

    #[derive(Error, Debug)]
    pub enum Error {
        #[error("expected {}, found {}", .0.blue(), .1.yellow())]
        NotASomethings(&'static str, String),

        #[error("unable to convert {} to {}", .1.yellow(), .0.blue())]
        InvalidConversion(&'static str, String),

        #[error("module {} not found in {}", .0.red(), .1.blue())]
        ModuleNotFound(String, String),

        #[error("symbol {} not found in module {}{}", .0.red(), .1.blue(), if let Some(p) = .2 {format!("/{}", p.yellow())} else {"".to_string()})]
        SymbolNotFound(String, String, Option<String>),

        #[error("perspective {} not found in module {}", .0.red(), .1.blue())]
        PerspectiveNotFound(String, String),

        #[error("symbol {} already exists in {}", .0.yellow(), .1.blue())]
        SymbolAlreadyExists(String, String),

        #[error("function {} already defined in {}", .0.yellow(), .1.blue())]
        FunctionAlreadyExists(String, String),

        #[error("function {} already exists: {} → {}", .0.yellow(), .0.red(), .1.magenta())]
        AliasAlreadyExists(String, String),

        #[error("circular definition found for {}", .0.red())]
        CircularDefinition(String),

        #[error("other modules can not be reached from here")]
        NotAGlobalScope,

        #[error("symbol {} can not be used in a pure context", .0.red().bold())]
        UnavailableInPureContext(String),

        #[error("missing column name in {}", 0.yellow().bold())]
        MissingColumn(String),

        #[error("missing perspective name in {}", 0.yellow().bold())]
        MissingPerspective(String),
    }
}
