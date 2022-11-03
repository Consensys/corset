use eyre::*;
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
            Err(eyre!("expected bool, found {}", x))
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
            _ => Err(eyre!("expected numeric value, found `{}`", x)),
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

                if let Some(column) = columns
                    .cols
                    .get_mut(module)
                    .and_then(|module| module.get_mut(colname))
                {
                    debug!("Inserting {}", Handle::new(module, colname));
                    column.set_value(parse_column(xs, column.t)?)
                }
            }
            Ok(())
        }
        _ => Ok(()),
    }
}

fn pad(r: &mut ColumnSet<F>) -> Result<()> {
    if r.is_empty() {
        return Ok(());
    }
    let max_len = r.len();
    let binary_not_len = r
        .cols
        .get_mut("binary")
        .and_then(|m| m.get("NOT"))
        .and_then(|c| c.len());
    let pad_to = (max_len + 1).next_power_of_two();

    r.cols
        .values_mut()
        .flat_map(|module| module.values_mut())
        .for_each(|x| {
            x.map(&|xs| {
                xs.reverse();
                xs.resize(pad_to, Fr::zero());
                xs.reverse();
            })
        });

    if let Some(col) = r.cols.get_mut("binary").and_then(|m| m.get_mut("NOT")) {
        let _255 = Fr::from_str("255").unwrap();
        col.map(&|xs| {
            for x in xs.iter_mut().take(pad_to - binary_not_len.unwrap()) {
                *x = _255;
            }
        });
    }

    Ok(())
}

pub fn compute(v: &Value, cs: &mut ConstraintSet, do_pad: bool) -> Result<()> {
    // 1. Read the traces and fill the computed columns
    fill_traces(v, vec![], &mut cs.modules).with_context(|| "while reading columns")?;
    if do_pad {
        pad(&mut cs.modules).with_context(|| "while padding columns")?;
    }
    cs.compute_all()
        .with_context(|| "while computing columns")?;

    Ok(())
}
