use std::io::Write;

use crate::compiler::{ConstraintSet, Kind, Magma};
use anyhow::*;
use convert_case::{Case, Casing};
use handlebars::Handlebars;
use itertools::Itertools;
use serde::Serialize;

const TEMPLATE: &str = include_str!("besu.java");

#[derive(Serialize)]
struct BesuColumn {
    corset_name: String,
    java_name: String,
    tupe: String,
    appender_name: String,
}
#[derive(Serialize)]
struct BesuConstant {
    name: String,
    value: String,
}
#[derive(Serialize)]
struct TemplateData {
    module: String,
    module_prefix: String,
    columns: Vec<BesuColumn>,
    constants: Vec<BesuConstant>,
}

pub fn render(cs: &ConstraintSet, package: &str, outfile: Option<&String>) -> Result<()> {
    let columns = cs
        .modules
        .iter_cols()
        .filter_map(|(handle, c)| {
            if matches!(c.kind, Kind::Atomic) {
                Some(BesuColumn {
                    corset_name: handle.name.to_string(),
                    java_name: handle.name.to_case(Case::Camel),
                    tupe: match c.t.magma() {
                        Magma::Boolean => "Boolean",
                        Magma::Nibble => "UnsignedByte",
                        Magma::Byte => "UnsignedByte",
                        Magma::Integer => "BigInteger",
                        Magma::Any => unreachable!(),
                    }
                    .into(),
                    appender_name: format!("append{}", handle.name.to_case(Case::Pascal)),
                })
            } else {
                None
            }
        })
        .sorted_by(|a, b| a.corset_name.cmp(&b.corset_name))
        .collect::<Vec<_>>();

    let constants = cs
        .constants
        .iter()
        .map(|c| BesuConstant {
            name: c.0.name.to_owned(),
            value: c.1.to_string(),
        })
        .collect::<Vec<_>>();

    let r = Handlebars::new().render_template(
        TEMPLATE,
        &TemplateData {
            module: package.to_owned(),
            module_prefix: package.to_case(Case::Pascal),
            columns,
            constants,
        },
    )?;

    if let Some(filename) = outfile.as_ref() {
        std::fs::File::create(filename)
            .with_context(|| format!("while creating `{}`", filename))?
            .write_all(r.as_bytes())
            .with_context(|| format!("while writing to `{}`", filename))
    } else {
        println!("{}", r);
        Ok(())
    }
}
