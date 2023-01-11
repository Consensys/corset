use anyhow::{anyhow, Context, Result};
use log::*;
use pairing_ce::{
    bn256::Fr,
    ff::{Field, PrimeField},
};
use rayon::prelude::*;
use serde_json::Value;

use crate::{
    column::ColumnSet,
    compiler::{ConstraintSet, Handle, Type},
};

type F = Fr;

fn validate(t: Type, x: F) -> Result<F> {
    if t.is_bool() {
        if x.is_zero() || x == F::one() {
            Ok(x)
        } else {
            Err(anyhow!("expected bool, found {}", x))
        }
    } else {
        Ok(x)
    }
}
fn parse_column(xs: &[Value], t: Type) -> Result<Vec<F>> {
    xs.par_iter()
        .map(|x| match x {
            Value::Number(n) => Fr::from_str(&n.to_string())
                .with_context(|| format!("while parsing `{:?}`", x))
                .and_then(|x| validate(t, x)),
            Value::String(s) => Fr::from_str(s)
                .with_context(|| format!("while parsing `{:?}`", x))
                .and_then(|x| validate(t, x)),
            _ => Err(anyhow!("expected numeric value, found `{}`", x)),
        })
        .collect()
}

fn fill_traces(v: &Value, path: Vec<String>, columns: &mut ColumnSet<F>) -> Result<()> {
    match v {
        Value::Object(map) => {
            for (k, v) in map.iter() {
                if k == "Trace" {
                    info!("Importing {}", path[path.len() - 1]);
                    fill_traces(v, path.clone(), columns)?;
                } else {
                    let mut path = path.clone();
                    path.push(k.to_owned());
                    fill_traces(v, path, columns)?;
                }
            }
            Ok(())
        }
        Value::Array(xs) => {
            if path.len() >= 2 {
                let module = &path[path.len() - 2];
                let colname = &path[path.len() - 1];
                let handle = Handle::new(module, colname);

                if !xs.is_empty() {
                    if let Some(column) = columns.by_handle_mut(&handle) {
                        trace!("Inserting {} ({})", handle, xs.len());
                        column.set_value(
                            parse_column(xs, column.t)
                                .with_context(|| anyhow!("while importing {}", handle))?,
                        )
                    }
                }
            }
            Ok(())
        }
        _ => Ok(()),
    }
}

pub fn compute(v: &Value, cs: &mut ConstraintSet, pad_trace: bool) -> Result<()> {
    fill_traces(v, vec![], &mut cs.modules).with_context(|| "while reading columns")?;
    cs.compute_all()
        .with_context(|| "while computing columns")?;
    cs.pad_columns();
    for h in cs.modules.handles() {
        if !cs.modules.get(&h).unwrap().is_computed() {
            error!("{} not computed", h);
        }
    }

    Ok(())
}
