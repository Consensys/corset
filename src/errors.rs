use super::pretty::Pretty;
use colored::Colorize;
use pairing_ce::bn256::Fr;
use thiserror::Error;

use crate::compiler::{Expression, Handle, Type};

#[derive(Error, Debug)]
pub enum CompileError<'a> {
    #[error("{}", make_type_error_msg(.0, .1, .2))]
    TypeError(String, &'a [&'a [Type]], Vec<Type>),
    #[error("{} is never used", .0.pretty())]
    NotUsed(Handle),
}

#[derive(Error, Debug)]
pub enum RuntimeError<'a> {
    #[error("{} is empty", .0.pretty())]
    EmptyColumn(Handle),
    #[error("{} could not be computed", .0.pretty())]
    NotComputed(Handle),
    #[error("expected a {} value, found {}", .0.white().bold(), .1.pretty().red())]
    InvalidValue(&'a str, Fr),
    #[error("expected an array, found {:?}", .0)]
    NotAnArray(Expression),
}

pub fn make_type_error_msg(fname: &str, expected: &[&[Type]], found: &[Type]) -> String {
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
