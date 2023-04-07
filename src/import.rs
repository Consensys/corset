use anyhow::*;
use cached::Cached;
use colored::Colorize;
use flate2::bufread::GzDecoder;
use log::*;
use logging_timer::time;
use pairing_ce::{
    bn256::Fr,
    ff::{Field, PrimeField},
};
use serde_json::Value;
use std::{
    fs::File,
    io::{BufReader, Seek},
};

use crate::{
    compiler::{ConstraintSet, Type},
    structs::Handle,
};

#[time("info", "Parsing trace from JSON file")]
pub fn read_trace(tracefile: &str, cs: &mut ConstraintSet) -> Result<()> {
    let mut f = File::open(tracefile).with_context(|| format!("while opening `{}`", tracefile))?;

    let gz = GzDecoder::new(BufReader::new(&f));
    let v: Value = match gz.header() {
        Some(_) => serde_json::from_reader(gz),
        None => {
            f.rewind()?;
            serde_json::from_reader(BufReader::new(&f))
        }
    }
    .with_context(|| format!("while reading `{}`", tracefile))?;
    fill_traces(&v, vec![], cs).with_context(|| "while reading columns")
}

pub fn read_trace_str(tracestr: &[u8], cs: &mut ConstraintSet) -> Result<()> {
    let gz = GzDecoder::new(BufReader::new(tracestr));
    let v: Value = match gz.header() {
        Some(_) => serde_json::from_reader(gz),
        None => serde_json::from_reader(BufReader::new(tracestr)),
    }?;
    fill_traces(&v, vec![], cs).with_context(|| "while reading columns")
}

fn parse_column(xs: &[Value], t: Type) -> Result<Vec<Fr>> {
    let mut cache_num = cached::SizedCache::with_size(200000); // ~1.60MB cache
    let mut cache_str = cached::SizedCache::with_size(200000); // ~1.60MB cache
    let mut r = vec![Fr::zero()];
    let xs = xs
        .iter()
        .map(|x| match x {
            Value::Number(n) => cache_num
                .cache_get_or_set_with(n, || Fr::from_str(&n.to_string()))
                .with_context(|| format!("while parsing Fr from Number `{:?}`", x))
                .and_then(|x| crate::utils::validate(t, x)),
            Value::String(s) => cache_str
                .cache_get_or_set_with(s, || Fr::from_str(s))
                .with_context(|| format!("while parsing Fr from String `{:?}`", x))
                .and_then(|x| crate::utils::validate(t, x)),
            _ => bail!("expected numeric value, found `{}`", x),
        })
        .collect::<Result<Vec<_>>>()?;
    r.extend(xs);
    Ok(r)
}

pub fn fill_traces(v: &Value, path: Vec<String>, cs: &mut ConstraintSet) -> Result<()> {
    match v {
        Value::Object(map) => {
            for (k, v) in map.iter() {
                if k == "Trace" {
                    debug!("Importing {}", path[path.len() - 1]);
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
                // The min length can be set if the module contains range
                // proofs, that require a minimal length of a certain power of 2
                let module_min_len = cs.modules.min_len.get(&handle.module).cloned().unwrap_or(0);

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

                    let mut xs = parse_column(xs, column.t)
                        .with_context(|| anyhow!("while importing {}", handle))?;

                    // If the parsed column is not long enought w.r.t. the
                    // minimal module length, prepend it with as many zeroes as
                    // required.
                    // Atomic columns are always padded with zeroes, so there is
                    // no need to trigger a more complex padding system.
                    if xs.len() < module_min_len {
                        xs.reverse();
                        xs.resize_with(module_min_len, Default::default);
                        xs.reverse();
                    }
                    column.set_value(xs, module_spilling)
                }
            }
            Ok(())
        }
        _ => Ok(()),
    }
}
