use std::fs::File;
use std::io::Write;
use std::path::{Path, PathBuf};

use crate::{
    compiler::{ConstraintSet, Kind, Magma},
    structs::Handle,
};
use anyhow::*;
use convert_case::{Case, Casing};
use handlebars::Handlebars;
use itertools::Itertools;
use serde::Serialize;

use super::reg_to_string;

const TRACE_COLUMNS_TEMPLATE: &str = include_str!("besu_trace_columns.java");
const TRACE_MODULE_TEMPLATE: &str = include_str!("besu_module_trace.java");

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

pub fn render(cs: &ConstraintSet, package: &str, output_filepath: Option<&String>) -> () {
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

    let handlebars = Handlebars::new();

    let template_data = TemplateData {
        module: package.to_owned(),
        module_prefix: package.to_case(Case::Pascal),
        constants,
        registers,
        columns,
    };

    let trace_module_render = handlebars
        .render_template(TRACE_MODULE_TEMPLATE, &template_data)
        .expect("error rendering trace module java template for Besu");

    let trace_columns_render = handlebars
        .render_template(TRACE_COLUMNS_TEMPLATE, &template_data)
        .expect("error rendering trace columns java template for Besu");

    match output_filepath {
        Some(f) => {
            let trace_module_java_filepath = {
                let m = format!("{}{}", template_data.module_prefix, "Trace.java");
                let p = Path::new(f).join(m);
                p
            };

            let trace_columns_java_filepath = Path::new(f).join("Trace.java");

            create_file(trace_module_java_filepath, trace_module_render)
                .expect("error creating trace module java file for Besu");

            create_file(trace_columns_java_filepath, trace_columns_render)
                .expect("error creating trace columns java file for Besu");
        }
        None => {
            println!(
                "{}\n=========================================================================\n{}",
                trace_module_render, trace_columns_render
            );
        }
    }

    pub fn create_file(file_path: PathBuf, contents: String) -> Result<(), Error> {
        // Create a new file with the provided name
        let mut file = File::create(file_path)?;

        // Write the provided contents into the file
        file.write_all(contents.as_bytes())?;

        // Return Ok if everything was successful
        Ok(())
    }
}
