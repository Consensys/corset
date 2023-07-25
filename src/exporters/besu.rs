use std::io::Write;

use crate::{
    compiler::{ConstraintSet, Kind, Magma},
    structs::Handle,
};
use anyhow::*;
use convert_case::{Case, Casing};
use handlebars::Handlebars;
use itertools::Itertools;
use owo_colors::OwoColorize;
use serde::Serialize;

use super::reg_to_string;

const TEMPLATE: &str = include_str!("besu.java");

#[derive(Serialize)]
struct BesuColumn {
    corset_name: String,
    java_name: String,
    tupe: String,
    appender: String,
    register: String,
}
#[derive(Serialize)]
struct BesuRegister {
    corset_name: String,
    java_name: String,
    tupe: String,
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
    registers: Vec<BesuRegister>,
    constants: Vec<BesuConstant>,
}

fn magma_to_java_type(m: Magma) -> String {
    match m {
        Magma::None => unreachable!(),
        Magma::Boolean => "Boolean",
        Magma::Nibble => "UnsignedByte",
        Magma::Byte => "UnsignedByte",
        Magma::Integer => "BigInteger",
        Magma::Any => unreachable!(),
        Magma::Loobean => unreachable!(),
    }
    .to_string()
}

fn handle_to_appender(h: &Handle) -> String {
    format!(
        "append{}{}",
        h.perspective
            .as_ref()
            .unwrap_or(&String::new())
            .to_case(Case::Pascal),
        h.name.to_case(Case::Pascal)
    )
}

pub fn render(cs: &ConstraintSet, package: &str, outfile: Option<&String>) -> Result<()> {
    let registers = cs
        .columns
        .registers
        .iter()
        .enumerate()
        .map(|(i, r)| {
            let corset_name = reg_to_string(r, i);
            let java_name = corset_name.to_case(Case::Camel);
            BesuRegister {
                corset_name,
                java_name,
                tupe: magma_to_java_type(r.magma),
            }
        })
        .sorted_by_key(|f| f.java_name.clone())
        .collect::<Vec<_>>();

    let columns = cs
        .columns
        .iter_cols()
        .filter_map(|c| {
            if matches!(c.kind, Kind::Atomic) {
                let r = c.register.unwrap();
                let register =
                    super::reg_to_string(&cs.columns.registers[r], r).to_case(Case::Camel);
                Some(BesuColumn {
                    corset_name: c.handle.name.to_string(),
                    java_name: c.handle.name.to_case(Case::Camel),
                    tupe: magma_to_java_type(c.t).into(),
                    appender: handle_to_appender(&c.handle),
                    register,
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
            name: crate::utils::purify(&c.0.name),
            value: c.1.to_string(),
        })
        .sorted_by_cached_key(|c| c.name.to_owned())
        .collect::<Vec<_>>();

    let r = Handlebars::new().render_template(
        TEMPLATE,
        &TemplateData {
            module: package.to_owned(),
            module_prefix: package.to_case(Case::Pascal),
            constants,
            registers,
            columns,
        },
    )?;

    if let Some(filename) = outfile.as_ref() {
        std::fs::File::create(filename)
            .with_context(|| format!("while creating {}", filename.white().bold()))?
            .write_all(r.as_bytes())
            .with_context(|| format!("while writing to {}", filename.white().bold()))
    } else {
        println!("{}", r);
        Ok(())
    }
}
