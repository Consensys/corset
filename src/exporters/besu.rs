use std::fs::File;
use std::io::Write;
use std::path::Path;
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

/// Return the Java type used to encode the given Magma
fn magma_to_java_type(m: Magma) -> String {
    match m.rm() {
        RawMagma::None => unreachable!(),
        RawMagma::Binary => "Boolean",
        RawMagma::Nibble => "UnsignedByte",
        RawMagma::Byte => "UnsignedByte",
        RawMagma::Native => "Bytes",
        RawMagma::Integer(w) => match w {
            1 => "boolean",
            2..=15 => "short",
            16..=31 => "int",
            32..=63 => "long",
            _ => "Bytes",
        },
        RawMagma::Any => unreachable!(),
    }
    .to_string()
}

/// Generate the Java 0-value of the given Magma
fn magma_to_java_zero(m: Magma) -> String {
    match m.rm() {
        RawMagma::None => unreachable!(),
        RawMagma::Binary => "false",
        RawMagma::Nibble => "UnsignedByte.of(0)",
        RawMagma::Byte => "UnsignedByte.of(0)",
        RawMagma::Native => "Bytes.EMPTY",
        RawMagma::Integer(w) => match w {
            1 => "false",
            2..=31 => "0",
            32..=63 => "0L",
            _ => "Bytes.EMPTY",
        },
        RawMagma::Any => unreachable!(),
    }
    .to_string()
}

/// Return the chosen java byte width for the given Magma
fn magma_to_java_bytewidth(m: Magma) -> i16 {
    match m.rm() {
        RawMagma::Binary | RawMagma::Nibble | RawMagma::Byte => 1,
        RawMagma::Native => 32,
        RawMagma::Integer(w) => match w {
            1 => 1,
            2..=15 => 2,
            16..=31 => 4,
            32..=63 => 8,
            _ => 32,
        },
        _ => unreachable!(),
    }
}

/// Generate the Java code to add an element in a trace register
fn magma_to_java_putter(m: Magma, register: &str) -> String {
    match m.rm() {
        RawMagma::Binary => format!("{}.put((byte) (b ? 1 : 0));", &register),
        RawMagma::Nibble | RawMagma::Byte => {
            format!("{}.put(b.toByte());", &register)
        }
        RawMagma::Integer(w) => match w {
            1 => format!("{}.put((byte) (b ? 1 : 0));", &register),
            2..=15 => format!("{}.putShort(b);", &register),
            16..=31 => format!("{}.putInt(b);", &register),
            32..=63 => format!("{}.putLong(b);", &register),
            _ => {
                format!(
                    r#"final byte[] bs = b.toArrayUnsafe();
    for (int i = bs.length; i < 32; i++) {{
      {0}.put((byte) 0);
    }}
    {0}.put(b.toArrayUnsafe());"#,
                    &register,
                )
            }
        },
        RawMagma::Native => format!(
            r#"final byte[] bs = b.toArrayUnsafe();
    for (int i = bs.length; i < 32; i++) {{
      {0}.put((byte) 0);
    }}
    {0}.put(b.toArrayUnsafe());"#,
            &register,
        ),
        _ => unreachable!(),
    }
}

/// Return the conventional method name to add an element to a trace register
fn handle_to_appender(h: &Handle) -> String {
    match h.perspective.as_ref() {
        None => h.name.to_case(Case::Camel),
        Some(p) => perspectivize_name(h, p),
    }
}

fn perspectivize_name(h: &Handle, p: &str) -> String {
    format!(
        "p{}{}",
        p.to_case(Case::Pascal),
        h.name.to_case(Case::Pascal)
    )
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
                bytes_width: magma_to_java_bytewidth(r.magma),
            }
        })
        .sorted_by_key(|f| f.java_name.clone())
        .collect::<Vec<_>>();

    let columns = cs
        .columns
        .iter_cols()
        .filter_map(|c| {
            if matches!(c.kind, Kind::Commitment) {
                let r = c.register.unwrap();
                let register = reg_to_string(&cs.columns.registers[r], r).to_case(Case::Camel);
                Some(BesuColumn {
                    corset_name: c.handle.to_string(),
                    java_name: c.handle.name.to_case(Case::Camel),
                    appender: handle_to_appender(&c.handle),
                    tupe: magma_to_java_type(c.t),
                    register: register.clone(),
                    reg_id: r,
                    putter: magma_to_java_putter(c.t, &register),
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
                format!("0x{:x}", c.1)
            } else if c.1.bits() <= 63 {
                format!("0x{:x}L", c.1)
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

            File::create(&trace_columns_java_filepath)?
                .write_all(trace_columns_render.as_bytes())
                .with_context(|| anyhow!("writing to {:?}", trace_columns_java_filepath))?;
        }
        None => {
            println!("{trace_columns_render}");
        }
    }
    Ok(())
}
