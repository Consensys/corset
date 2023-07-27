use super::compiler::{ColumnRef, Magma};
use anyhow::*;
use cached::Cached;
use flate2::bufread::GzDecoder;
use log::*;
use logging_timer::time;
use owo_colors::OwoColorize;
use pairing_ce::{
    bn256::Fr,
    ff::{Field, PrimeField},
};
#[cfg(not(all(target_arch = "x86_64", target_feature = "avx")))]
use serde_json::Value;
#[cfg(all(target_arch = "x86_64", target_feature = "avx"))]
use simd_json::BorrowedValue as Value;
#[cfg(all(target_arch = "x86_64", target_feature = "avx"))]
use std::io::Read;
use std::{
    fs::File,
    io::{BufReader, Seek},
};

use crate::{column::Column, compiler::ConstraintSet, pretty::Pretty, structs::Handle};

#[time("info", "Parsing trace from JSON file with SIMD")]
pub fn read_trace(tracefile: &str, cs: &mut ConstraintSet) -> Result<()> {
    let mut f = File::open(tracefile).with_context(|| format!("while opening `{}`", tracefile))?;
    let gz = GzDecoder::new(BufReader::new(&f));

    #[cfg(all(target_arch = "x86_64", target_feature = "avx"))]
    {
        let mut content = Vec::new();
        match gz.header() {
            Some(_) => gz.read_to_end(&mut content),
            None => {
                f.rewind()?;
                BufReader::new(&f).read_to_end(&mut content)
            }
        }
        .with_context(|| format!("while reading `{}`", tracefile))?;
        let v = simd_json::to_borrowed_value(&mut content)
            .map_err(|e| anyhow!("while parsing json: {}", e))?;
        fill_traces(&v, vec![], cs, &mut None).with_context(|| "while reading columns")
    }
    #[cfg(not(all(target_arch = "x86_64", target_feature = "avx")))]
    {
        let v: Value = match gz.header() {
            Some(_) => serde_json::from_reader(gz),
            None => {
                f.rewind()?;
                serde_json::from_reader(BufReader::new(&f))
            }
        }
        .with_context(|| format!("while reading `{}`", tracefile))?;
        fill_traces(&v, vec![], cs, &mut None).with_context(|| "while reading columns")
    }
}

#[time("info", "Parsing trace from JSON with SIMD")]
pub fn read_trace_str(tracestr: &[u8], cs: &mut ConstraintSet) -> Result<()> {
    let gz = GzDecoder::new(BufReader::new(tracestr));
    #[cfg(all(target_arch = "x86_64", target_feature = "avx"))]
    {
        let mut content = Vec::new();
        match gz.header() {
            Some(_) => {
                gz.read_to_end(&mut content)?;
            }
            None => {
                content = tracestr.to_vec();
            }
        };
        let v = simd_json::to_borrowed_value(&mut content)
            .map_err(|e| anyhow!("while parsing json: {}", e))?;
        fill_traces(&v, vec![], cs, &mut None).with_context(|| "while reading columns")
    }
    #[cfg(not(all(target_arch = "x86_64", target_feature = "avx")))]
    {
        let v: Value = match gz.header() {
            Some(_) => serde_json::from_reader(gz),
            None => serde_json::from_reader(BufReader::new(tracestr)),
        }?;
        fill_traces(&v, vec![], cs, &mut None).with_context(|| "while reading columns")
    }
}

#[cfg(not(all(target_arch = "x86_64", target_feature = "avx")))]
fn parse_column(xs: &[Value], t: Magma) -> Result<Vec<Fr>> {
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

    if crate::utils::maybe_warn(t, &r).is_err() {
        error!("Column of boolean found, but not annotated as :bool");
    };
    r.extend(xs);
    Ok(r)
}

#[cfg(all(target_arch = "x86_64", target_feature = "avx"))]
fn parse_column(xs: &[Value], t: Type) -> Result<Vec<Fr>> {
    let mut cache = cached::SizedCache::with_size(200000); // ~1.60MB cache
    let mut r = vec![Fr::zero()];
    let xs = xs
        .iter()
        .map(|x| {
            let s = match x {
                Value::Static(n) => match n {
                    simd_json::StaticNode::I64(i) => i.to_string(),
                    simd_json::StaticNode::U64(i) => i.to_string(),
                    _ => {
                        unreachable!()
                    }
                },
                Value::String(s) => s.to_string(),
                _ => bail!("expected numeric value, found `{}`", x),
            };
            cache
                .cache_get_or_set_with(s.clone(), || Fr::from_str(&s))
                .with_context(|| format!("while parsing Fr from `{:?}`", x))
                .and_then(|x| crate::utils::validate(t, x))
        })
        .collect::<Result<Vec<_>>>()?;
    r.extend(xs);
    if crate::utils::maybe_warn(t, &r).is_err() {
        error!("Column of boolean found, but not annotated as :bool");
    };
    Ok(r)
}

pub fn fill_traces(
    v: &Value,
    path: Vec<String>,
    cs: &mut ConstraintSet,
    initiator: &mut Option<&mut String>,
) -> Result<()> {
    match v {
        Value::Object(map) => {
            for (k, v) in map.iter() {
                if k == "Trace" {
                    debug!("Importing {}", path[path.len() - 1]);
                    let mut first_column = String::new();
                    let mut initiator = Some(&mut first_column);
                    fill_traces(v, path.clone(), cs, &mut initiator)?;
                } else {
                    let mut path = path.clone();
                    path.push(k.to_string());
                    fill_traces(v, path, cs, initiator)?;
                }
            }
            Ok(())
        }
        Value::Array(xs) => {
            if path.len() >= 2 {
                let module = path[path.len() - 2].to_string();
                let handle: ColumnRef = Handle::new(&module, &path[path.len() - 1]).into();
                // The first column sets the size of its module
                let module_raw_size = cs.raw_len_for_or_set(&module, xs.len() as isize);

                if let Result::Ok(Column {
                    t, padding_value, ..
                }) = cs.columns.get_col(&handle)
                {
                    trace!("inserting {} ({})", handle, xs.len());
                    if let Some(first_column) = initiator.as_mut() {
                        if first_column.is_empty() {
                            first_column.push_str(&handle.pretty());
                        }
                    }

                    // The min length can be set if the module contains range
                    // proofs, that require a minimal length of a certain power of 2
                    let module_min_len = cs.columns.min_len.get(&module).cloned().unwrap_or(0);

                    let module_spilling = cs
                        .spilling_for(&handle)
                        .ok_or_else(|| anyhow!("no spilling found for {}", handle.pretty()))?;

                    if xs.len() as isize != module_raw_size {
                        bail!(
                            "{} has an incorrect length: expected {} (from {}), found {}",
                            handle.to_string().blue(),
                            module_raw_size.to_string().red().bold(),
                            initiator.as_ref().unwrap(),
                            xs.len().to_string().yellow().bold(),
                        );
                    }

                    let mut xs = parse_column(xs, *t)
                        .with_context(|| anyhow!("while importing {}", handle))?;

                    // If the parsed column is not long enought w.r.t. the
                    // minimal module length, prepend it with as many zeroes as
                    // required.
                    // Atomic columns are always padded with zeroes, so there is
                    // no need to trigger a more complex padding system.
                    if xs.len() < module_min_len {
                        xs.reverse();
                        xs.resize_with(module_min_len, || {
                            padding_value.map(|v| v.1).unwrap_or_default()
                        });
                        xs.reverse();
                    }
                    cs.columns.set_value(&handle, xs, module_spilling)?
                } else {
                    debug!("ignoring unknown column {}", handle.pretty());
                }
            }
            Ok(())
        }
        _ => Ok(()),
    }
}
