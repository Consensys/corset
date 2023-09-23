use anyhow::*;
use pairing_ce::{
    bn256::Fr,
    ff::{Field, PrimeField},
};
#[cfg(feature = "postgres")]
use postgres::Client;
#[cfg(feature = "postgres")]
use std::io::Read;
use std::unreachable;

use crate::{compiler::Magma, errors::RuntimeError, pretty::Pretty, structs::Handle};

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
    static ref F_15: Fr = Fr::from_str("15").unwrap();
    static ref F_255: Fr = Fr::from_str("255").unwrap();
}

pub fn maybe_warn(t: Magma, xs: &[Fr], h: &Handle) -> Result<()> {
    if t != Magma::Boolean
        && xs.iter().all(|x| x.is_zero() || *x == Fr::one())
        && xs.iter().any(|x| *x == Fr::one())
    {
        bail!(
            "Column {} filled with boolean, but not annotated as :bool",
            h.pretty(),
        );
    }

    Ok(())
}

pub fn validate(t: Magma, x: Fr) -> Result<Fr> {
    match t {
        Magma::None => unreachable!(),
        Magma::Boolean => {
            if x.is_zero() || x == Fr::one() {
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
        Magma::Integer(_) => unreachable!(),
        Magma::Any => unreachable!(),
        Magma::Loobean => unreachable!(), // input should never be declared as loobeans
    }
}

/// Remove all symbols in a symbol which are invalid in Go identifiers
pub fn purify(s: &str) -> String {
    s.replace(
        ['(', ')', '{', '}', '[', ']', '<', '>', ':', '%', '.', '_'],
        "_",
    )
    .replace('-', "sub")
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
