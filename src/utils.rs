use anyhow::*;
#[cfg(feature = "postgres")]
use postgres::Client;
#[cfg(feature = "postgres")]
use std::io::Read;

use crate::{column::Value, compiler::Magma, pretty::Pretty, structs::Handle};

pub fn is_file_empty(f: &str) -> Result<bool> {
    std::fs::metadata(f)
        .with_context(|| anyhow!("unable to read metadata of `{}`", f))
        .map(|f| f.len() == 0)
}

#[cfg(feature = "postgres")]
pub fn connect_to_db(
    user: &str,
    password: &Option<String>,
    host: &str,
    database: &str,
) -> Result<Client> {
    Client::connect(
        &format!(
            "postgres://{}{}@{}/{}",
            user,
            password
                .as_ref()
                .map(|p| format!(":{}", p))
                .unwrap_or_default(),
            host,
            database
        ),
        postgres::NoTls,
    )
    .with_context(|| format!("while connecting to {}@{}/{}", user, host, database))
}

pub fn maybe_warn(t: Magma, xs: &[Value], h: &Handle) -> Result<()> {
    if !t.is_binary()
        && xs.iter().all(|x| x.is_zero() || x.is_one())
        && xs.iter().any(|x| x.is_one())
    {
        bail!(
            "Column {} filled with boolean, but not annotated as :bool",
            h.pretty(),
        );
    }

    Ok(())
}

/// Remove all symbols in a symbol which are invalid in Go identifiers
pub fn purify(s: &str) -> String {
    s.replace(
        [
            '(', ')', '{', '}', '[', ']', '<', '>', ':', '%', '.', '-', '#', ' ',
        ],
        "_",
    )
    .replace('*', "mul")
    .replace('+', "add")
    .replace('/', "div")
    .replace('^', "pow")
    .replace('~', "norm")
    .replace('α', "alpha")
    .replace('β', "beta")
    .replace('γ', "gamma")
    .replace('δ', "delta")
    .replace('ϵ', "epsilon")
    .replace('λ', "lambda")
    .replace('τ', "tau")
    .replace('μ', "mu")
    .replace('ν', "nu")
    .replace('∅', "empty")
    .replace('ₐ', "a")
    .replace('ₑ', "e")
    .replace('ₓ', "x")
    .replace('₀', "0")
    .replace('₁', "1")
    .replace('₂', "2")
    .replace('₃', "3")
    .replace('₄', "4")
    .replace('₅', "5")
    .replace('₆', "6")
    .replace('₇', "7")
    .replace('₈', "8")
    .replace('₉', "9")
    .replace(|c: char| !c.is_ascii(), "_")
}
