use std::fs::File;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::println;

use crate::compiler::RawMagma;
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

const TRACE_COLUMNS_TEMPLATE: &str = include_str!("besu_trace_columns.java");

#[derive(Serialize)]
struct BesuColumn {
    corset_name: String,
    java_name: String,
    appender: String,
    tupe: String,
    register: String,
    putter: String,
    reg_id: usize,
}
#[derive(Serialize)]
struct BesuRegister {
    corset_name: String,
    java_name: String,
    tupe: String,
    id: usize,
    zero_value: String,
    bytes_width: i16,
}
#[derive(Serialize)]
struct BesuConstant {
    name: String,
    value: String,
    tupe: String,
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
    match m.rm() {
        RawMagma::None => unreachable!(),
        RawMagma::Binary => "Boolean",
        RawMagma::Nibble => "UnsignedByte",
        RawMagma::Byte => "UnsignedByte",
        RawMagma::Native => "Bytes",
        RawMagma::Integer(_) => "Bytes",
        RawMagma::Any => unreachable!(),
    }
    .to_string()
}

fn magma_to_java_zero(m: Magma) -> String {
    match m.rm() {
        RawMagma::None => unreachable!(),
        RawMagma::Binary => "false",
        RawMagma::Nibble => "UnsignedByte.of(0)",
        RawMagma::Byte => "UnsignedByte.of(0)",
        RawMagma::Native => "Bytes.EMPTY",
        RawMagma::Integer(_) => "Bytes.EMPTY",
        RawMagma::Any => unreachable!(),
    }
    .to_string()
}

fn handle_to_appender(h: &Handle) -> String {
    match h.perspective.as_ref() {
        None => h.name.to_case(Case::Camel),
        Some(p) => perspectivize_name(h, p),
    }
}

fn handle_to_updater(h: &Handle) -> String {
    match h.perspective.as_ref() {
        None => h.name.to_case(Case::Camel),
        Some(p) => perspectivize_name(h, p),
    }
    .to_case(Case::Pascal)
}

fn perspectivize_name(h: &Handle, p: &str) -> String {
    format!(
        "p{}{}",
        p.to_case(Case::Pascal),
        h.name.to_case(Case::Pascal)
    )
}

fn fill_file(file_path: PathBuf, contents: String) -> Result<(), Error> {
    // Create a new file with the provided name
    let mut file = File::create(file_path)?;

    // Write the provided contents into the file
    file.write_all(contents.as_bytes())?;

    // Return Ok if everything was successful
    Ok(())
}

pub fn render(cs: &ConstraintSet, package: &str, output_path: Option<&String>) -> Result<()> {
    let registers = cs
        .columns
        .registers
        .iter()
        .enumerate()
        .map(|(i, r)| {
            let corset_name = format!(
                "{}.{}",
                r.handle.as_ref().unwrap().module,
                r.handle.as_ref().unwrap().name
            );
            let java_name = reg_to_string(r, i).to_case(Case::Camel);
            BesuRegister {
                corset_name,
                java_name,
                tupe: magma_to_java_type(r.magma),
                id: i,
                zero_value: magma_to_java_zero(r.magma),
                bytes_width: r.magma.byte_size() as i16,
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
                let register = reg_to_string(&cs.columns.registers[r], r).to_case(Case::Camel);
                Some(BesuColumn {
                    corset_name: c.handle.to_string(),
                    java_name: c.handle.name.to_case(Case::Camel),
                    appender: handle_to_appender(&c.handle),
                    tupe: magma_to_java_type(c.t),
                    register: register.clone(),
                    reg_id: r,
                    putter: match c.t.rm() {
                        RawMagma::Binary => format!("{}.put((byte) (b ? 1 : 0));", &register),
                        RawMagma::Nibble => format!("{}.put(b.toByte());", &register),
                        RawMagma::Byte => format!("{}.put(b.toByte());", &register),
                        RawMagma::Native | RawMagma::Integer(_) => format!(
                            r#"final byte[] bs = b.toArrayUnsafe();
    for (int i = bs.length; i < 32; i++) {{
      {0}.put((byte) 0);
    }}
    {0}.put(b.toArrayUnsafe());"#,
                            &register
                        ),
                        _ => unreachable!(),
                    },
                })
            } else {
                None
            }
        })
        .sorted_by(|a, b| a.appender.cmp(&b.appender))
        .collect::<Vec<_>>();

    let constants = cs
        .constants
        .iter()
        .map(|c| BesuConstant {
            name: crate::utils::purify(&c.0.name),
            value: if c.1.bits() <= 31 {
                c.1.to_string()
            } else if c.1.bits() <= 63 {
                format!("{}L", c.1)
            } else {
                format!("new BigInteger(\"{}\")", c.1)
            },
            tupe: (if c.1.bits() <= 31 {
                "int"
            } else if c.1.bits() <= 63 {
                "long"
            } else {
                "BigInteger"
            })
            .to_string(),
        })
        .sorted_by_cached_key(|c| c.name.to_owned())
        .collect::<Vec<_>>();

    let mut handlebars = Handlebars::new();
    handlebars.register_escape_fn(handlebars::no_escape);

    let template_data = TemplateData {
        module: package.to_owned(),
        module_prefix: package.to_case(Case::Pascal),
        constants,
        registers,
        columns,
    };

    let trace_columns_render = handlebars
        .render_template(TRACE_COLUMNS_TEMPLATE, &template_data)
        .expect("error rendering trace columns java template for Besu");

    match output_path {
        Some(f) => {
            if !Path::new(f).is_dir() {
                bail!("{} is not a directory", f.bold().yellow());
            }

            let trace_columns_java_filepath = Path::new(f).join("Trace.java");

            fill_file(trace_columns_java_filepath, trace_columns_render)
                .expect("error creating trace columns java file for Besu");
        }
        None => {
            println!("{trace_columns_render}");
        }
    }
    Ok(())
}
