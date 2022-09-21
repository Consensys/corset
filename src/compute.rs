use eyre::*;
use log::*;
use num_bigint::BigInt;
use regex::Regex;
use serde::Serialize;
use serde_json::Value;
use std::{collections::HashMap, str::FromStr};

use crate::{
    column::{Column, ColumnSet},
    compiler::{ConstraintsSet, Type},
};

#[derive(Default, Serialize, Debug)]
struct ComputeResult {
    columns: HashMap<String, Vec<BigInt>>,
}

fn parse_column(xs: &[Value], t: Type) -> Result<Vec<BigInt>> {
    xs.iter()
        .map(|x| {
            let x = BigInt::from_str(x.as_str().unwrap()).unwrap();
            Ok(x)
        })
        .collect()
}

fn fill_traces(v: &Value, path: Vec<String>, columns: &mut ColumnSet<BigInt>) -> Result<()> {
    let colname_regex = Regex::new(r"(.*)_([0-9]+)?").unwrap();
    match v {
        Value::Object(map) => {
            for (k, v) in map.iter() {
                if k == "Trace" || k == "Assignment" {
                    fill_traces(v, path.clone(), columns)?;
                } else {
                    let mut path = path.clone();
                    path.push(k.to_owned());
                    fill_traces(v, path, columns)?;
                }
            }
            Ok(())
        }
        Value::Null => Ok(()),
        Value::Bool(_) => Ok(()),
        Value::Number(_) => Ok(()),
        Value::String(_) => Ok(()),
        Value::Array(xs) => {
            println!("Found an array: {:?}", path);
            if path.len() >= 2 {
                let module = &path[path.len() - 2];
                let colname = &path[path.len() - 1];
                let caps = colname_regex.captures(colname).unwrap();
                let colname = caps
                    .get(0)
                    .ok_or(eyre!("Invalid column name :`{}`", colname))?
                    .as_str()
                    .to_string();
                let col_idx = caps
                    .get(1)
                    .ok_or("")
                    .map(|i| i.as_str())
                    .map(|i| i.parse::<usize>());
                let r = columns
                    .cols
                    .get_mut(module)
                    .ok_or(eyre!("Module `{}` does not exist in constraints", module))
                    .and_then(|module| {
                        module
                            .get_mut(&colname)
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
                            let idx = col_idx.unwrap().unwrap();
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
                    warn!("{:?}", r);
                }
            } else {
                warn!("Found a path too short: {:?}", path)
            }
            Ok(())
        }
    }
}

pub fn compute(tracefile: &str, cs: &mut ConstraintsSet) -> Result<()> {
    let v: Value = serde_json::from_str(
        &std::fs::read_to_string(tracefile)
            .with_context(|| format!("while reading `{}`", tracefile))?,
    )?;
    fill_traces(&v, vec![], &mut cs.columns)?;

    let mut r = ComputeResult::default();
    for (module, columns) in cs.columns.cols.iter_mut() {
        for (colname, col) in columns.iter_mut() {
            r.columns.insert(
                format!("{}{}{}", module, "__", colname), // TODO module separator
                col.values().to_owned(),
            );
        }
    }
    Ok(())
}
