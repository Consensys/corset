use eyre::*;
use log::*;
use num_bigint::BigInt;
use num_traits::{One, Zero};
use serde::Serialize;
use serde_json::Value;
use std::{collections::HashMap, io::Write, str::FromStr};

use crate::{
    column::{Column, ColumnSet},
    compiler::{ConstraintsSet, Type},
};

#[derive(Default, Serialize, Debug)]
struct ComputeResult {
    columns: HashMap<String, Vec<BigInt>>,
}

fn parse_column(xs: &[Value], t: Type) -> Result<Vec<BigInt>> {
    dbg!(xs)
        .iter()
        .map(|x| {
            match x {
                Value::Number(n) => BigInt::from_str(&n.to_string())
                    .with_context(|| format!("while parsing `{:?}`", x)),
                Value::Null => todo!(),
                Value::Bool(_) => todo!(),
                Value::String(s) => {
                    BigInt::from_str(s).with_context(|| format!("while parsing `{:?}`", x))
                }
                Value::Array(_) => todo!(),
                Value::Object(_) => todo!(),
            }
            // let x = BigInt::from_str(&dbg!(x.to_string())).unwrap();
            // Ok(x)
        })
        .collect()
}

fn fill_traces(v: &Value, path: Vec<String>, columns: &mut ColumnSet<BigInt>) -> Result<()> {
    // info!("Browsing {:?}", path);
    // let colname_regex = Regex::new(r"(.*)_([0-9]+)?").unwrap();
    match v {
        Value::Object(map) => {
            for (k, v) in map.iter() {
                if k == "Trace" || k == "Assignment" {
                    fill_traces(v, path.clone(), columns);
                } else {
                    let mut path = path.clone();
                    path.push(k.to_owned());
                    fill_traces(v, path, columns);
                }
            }
            Ok(())
        }
        Value::Null => Ok(()),
        Value::Bool(_) => Ok(()),
        Value::Number(_) => Ok(()),
        Value::String(_) => Ok(()),
        Value::Array(xs) => {
            if path.len() >= 2 {
                let module = &path[path.len() - 2];
                let colname = &path[path.len() - 1];
                let col_components = colname.split("_").collect::<Vec<_>>();
                let idx = if col_components.len() > 2 {
                    col_components.last().unwrap().parse::<usize>().ok()
                } else {
                    None
                };
                let radix = if idx.is_some() {
                    col_components[0..col_components.len() - 1].join("_")
                } else {
                    colname.to_string()
                };

                let r = columns
                    .cols
                    .get_mut(module)
                    .ok_or(eyre!("Module `{}` does not exist in constraints", module))
                    .and_then(|module| {
                        module
                            .get_mut(&radix)
                            .ok_or(eyre!("Column `{}` does not exist in constraints", colname))
                    })
                    .and_then(|column| match column {
                        Column::Atomic(ref mut value, t) => {
                            *value = parse_column(xs, *t)?;
                            Ok(())
                        } // TODO check type
                        Column::Array {
                            range,
                            ref mut content,
                        } => {
                            let idx = idx.unwrap();
                            if range.contains(&idx) {
                                content.insert(idx, parse_column(xs, Type::Numeric)?);
                                Ok(())
                            } else {
                                Err(eyre!(
                                    "index {} for column {} is out of range {:?}",
                                    idx,
                                    colname,
                                    range
                                ))
                            }
                        }
                        Column::Composite { ref mut value, .. } => {
                            warn!("composite column `{}` filled from trace", colname);
                            *value = Some(parse_column(xs, Type::Numeric)?);
                            Ok(())
                        }
                        Column::Interleaved { ref mut value, .. } => {
                            warn!("interleaved column `{}` filled from trace", colname);
                            *value = Some(parse_column(xs, Type::Numeric)?);
                            Ok(())
                        }
                    });
                if r.is_err() {
                    warn!("{:#?}", r);
                }
            } else {
                warn!("Found a path too short: {:?}", path)
            }
            Ok(())
        }
    }
}

fn pad(r: &mut ComputeResult) -> Result<()> {
    let max_len = r.columns.values().map(|xs| xs.len()).max().unwrap();
    let pad_to = max_len.next_power_of_two();
    r.columns
        .values_mut()
        .for_each(|xs| xs.resize(pad_to, Zero::zero()));
    Ok(())
}

pub fn compute(tracefile: &str, cs: &mut ConstraintsSet, outfile: Option<String>) -> Result<()> {
    let v: Value = serde_json::from_str(
        &std::fs::read_to_string(tracefile)
            .with_context(|| format!("while reading `{}`", tracefile))?,
    )?;
    fill_traces(&v, vec![], &mut cs.columns)?;

    let mut r = ComputeResult::default();
    for (module, columns) in cs.columns.cols.iter_mut() {
        for (colname, col) in columns.iter_mut() {
            match col {
                Column::Atomic(content, _) => {
                    r.columns.insert(
                        format!("{}{}{}", module, "__", colname), // TODO module separator
                        content.to_owned(),
                    );
                }
                Column::Array { content, .. } => {
                    for (i, col) in content.iter() {
                        r.columns.insert(
                            format!("{}{}{}{}{}", module, "__", colname, "_", i), // TODO module separator
                            col.clone(),
                        );
                    }
                }
                Column::Composite { value, exp } => todo!(),
                Column::Interleaved { value, from } => todo!(),
            }
        }
    }

    pad(&mut r)?;
    let stringified: HashMap<String, Vec<String>> = r
        .columns
        .into_iter()
        .map(|(k, v)| (k, v.iter().map(|x| x.to_string()).collect::<Vec<_>>()))
        .collect();
    let r = serde_json::to_string(&stringified)?;

    if let Some(outfilename) = outfile.as_ref() {
        std::fs::File::create(outfilename)
            .with_context(|| format!("while creating `{}`", outfilename))?
            .write_all(r.as_bytes())
            .with_context(|| format!("while writing to `{}`", outfilename))
    } else {
        println!("{}", r);
        Ok(())
    }
}
