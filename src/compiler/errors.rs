use colored::Colorize;
use thiserror::Error;

use super::Type;

#[derive(Error, Debug)]
pub enum CompileError<'a> {
    #[error("{}", make_type_error_msg(.0, .1, .2))]
    TypeError(String, &'a [&'a [Type]], Vec<Type>),
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
