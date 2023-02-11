use anyhow::{anyhow, bail, Context, Result};
use colored::Colorize;
use log::*;
use pairing_ce::{
    bn256::Fr,
    ff::{Field, PrimeField},
};
use rayon::prelude::*;
use serde_json::Value;

use crate::{
    compiler::{ConstraintSet, Handle, Type},
    errors::RuntimeError,
};

type F = Fr;

lazy_static::lazy_static! {
    static ref F_15: F = Fr::from_str("15").unwrap();
    static ref F_255: F = Fr::from_str("255").unwrap();
}

fn validate(t: Type, x: F) -> Result<F> {
    match t.magma() {
        crate::compiler::Magma::Boolean => {
            if x.is_zero() || x == F::one() {
                Ok(x)
            } else {
                bail!(RuntimeError::InvalidValue("bool", x))
            }
        }
        crate::compiler::Magma::Nibble => {
            if x.le(&F_15) {
                Ok(x)
            } else {
                bail!(RuntimeError::InvalidValue("nibble", x))
            }
        }
        crate::compiler::Magma::Byte => {
            if x.le(&F_255) {
                Ok(x)
            } else {
                bail!(RuntimeError::InvalidValue("byte", x))
            }
        }
        crate::compiler::Magma::Integer => Ok(x),
        crate::compiler::Magma::Any => unreachable!(),
    }
}

fn parse_column(xs: &[Value], t: Type) -> Result<Vec<F>> {
    let mut r = vec![F::zero()];
    let xs = xs
        .par_iter()
        .map(|x| match x {
            Value::Number(n) => Fr::from_str(&n.to_string())
                .with_context(|| format!("while parsing `{:?}`", x))
                .and_then(|x| validate(t, x)),
            Value::String(s) => Fr::from_str(s)
                .with_context(|| format!("while parsing `{:?}`", x))
                .and_then(|x| validate(t, x)),
            _ => bail!("expected numeric value, found `{}`", x),
        })
        .collect::<Result<Vec<_>>>()?;
    r.extend(xs);
    Ok(r)
}

fn fill_traces(v: &Value, path: Vec<String>, cs: &mut ConstraintSet) -> Result<()> {
    match v {
        Value::Object(map) => {
            for (k, v) in map.iter() {
                if k == "Trace" {
                    info!("Importing {}", path[path.len() - 1]);
                    fill_traces(v, path.clone(), cs)?;
                } else {
                    let mut path = path.clone();
                    path.push(k.to_owned());
                    fill_traces(v, path, cs)?;
                }
            }
            Ok(())
        }
        Value::Array(xs) => {
            if !xs.is_empty() && path.len() >= 2 {
                let handle = Handle::new(&path[path.len() - 2], &path[path.len() - 1]);
                // The first column set the size of its module
                let module_raw_size = cs.raw_len_for_or_set(&handle.module, xs.len() as isize);
                let module_spilling = cs.spilling_or_insert(&handle.module);

                if let Some(column) = cs.modules.by_handle_mut(&handle) {
                    trace!("Inserting {} ({})", handle, xs.len());

                    if xs.len() as isize != module_raw_size {
                        bail!(
                            "{} has an incorrect length: expected {}, found {}",
                            handle.to_string().blue(),
                            xs.len().to_string().yellow().bold(),
                            module_raw_size.to_string().red().bold()
                        );
                    }

                    column.set_value(
                        parse_column(xs, column.t)
                            .with_context(|| anyhow!("while importing {}", handle))?,
                        module_spilling,
                    )
                }
            }
            Ok(())
        }
        _ => Ok(()),
    }
}

pub fn compute(v: &Value, cs: &mut ConstraintSet) -> Result<()> {
    fill_traces(v, vec![], cs).with_context(|| "while reading columns")?;
    cs.compute_all()
        .with_context(|| "while computing columns")?;
    for h in cs.modules.handles() {
        if !cs.modules.get(&h).unwrap().is_computed() {
            warn!("{}", RuntimeError::NotComputed(h.clone()));
        }
    }

    Ok(())
}
