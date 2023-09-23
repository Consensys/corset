use anyhow::*;
#[cfg(feature = "postgres")]
use postgres::Client;
#[cfg(feature = "postgres")]
use std::io::Read;
use std::unreachable;

use crate::{
    column::Value, compiler::Magma, errors::RuntimeError, pretty::Pretty, structs::Handle,
};

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

lazy_static::lazy_static! {
    static ref F_15: Value = Value::from(15);
    static ref F_255: Value = Value::from(255);
}

pub fn maybe_warn(t: Magma, xs: &[Value], h: &Handle) -> Result<()> {
    if t != Magma::Boolean
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

pub fn validate(t: Magma, x: Value) -> Result<Value> {
    match t {
        Magma::None => unreachable!(),
        Magma::Boolean => {
            if x.is_zero() || x == Value::one() {
                Ok(x)
            } else {
                bail!(RuntimeError::InvalidValue("bool", x))
            }
        }
        Magma::Nibble => {
            if x.le(&F_15) {
                Ok(x)
            } else {
                bail!(RuntimeError::InvalidValue("nibble", x))
            }
        }
        Magma::Byte => {
            if x.le(&F_255) {
                Ok(x)
            } else {
                bail!(RuntimeError::InvalidValue("byte", x))
            }
        }
        Magma::Native => Ok(x),
        Magma::Integer(_) => Ok(x), // TODO: FIXME:
        Magma::Any => unreachable!(),
        Magma::Loobean => unreachable!(), // input should never be declared as loobeans
    }
}

/// Remove all symbols in a symbol which are invalid in Go identifiers
pub fn purify(s: &str) -> String {
    s.replace(
        [
            '(', ')', '{', '}', '[', ']', '<', '>', ':', '%', '.', '-', '#',
        ],
        "_",
    )
    .replace('*', "mul")
    .replace('+', "add")
    .replace('/', "div")
    .replace('^', "pow")
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
    .replace(|c: char| !c.is_ascii(), "_")
}
