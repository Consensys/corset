use anyhow::*;
use convert_case::{Case, Casing};
use handlebars::Handlebars;
use itertools::Itertools;
use serde::Serialize;
use std::io::Write;

use crate::compiler::*;

#[derive(Serialize)]
struct GoConstant {
    name: String,
    value: String,
}
#[derive(Serialize)]
struct GoColumn {
    reg_name: String,
    reg_id: usize,
    go_name: String,
}
#[derive(Serialize)]
struct TemplateData {
    module: String,
    columns: Vec<GoColumn>,
    constants: Vec<GoConstant>,
    registers: Vec<(usize, String)>,
}

pub fn render(cs: &ConstraintSet, package: &str, outfile: Option<&String>) -> Result<()> {
    const TEMPLATE: &str = include_str!("zkgeth.go");
    let columns = cs
        .columns
        .iter_cols()
        .filter_map(|c| {
            if matches!(c.kind, Kind::Commitment) {
                let r = c.register.unwrap();
                let register = super::reg_to_string(&cs.columns.registers[r], r);
                Some(GoColumn {
                    reg_name: register,
                    reg_id: r,
                    go_name: c.handle.mangled_name(),
                })
            } else {
                None
            }
        })
        .sorted_by(|a, b| a.reg_name.cmp(&b.reg_name))
        .collect::<Vec<_>>();

    let registers = cs
        .columns
        .registers
        .iter()
        .enumerate()
        .map(|(i, r)| (i, super::reg_to_string(r, i)))
        .collect::<Vec<_>>();

    let constants = cs
        .constants
        .iter()
        .map(|c| GoConstant {
            name: c.0.mangled_name().to_case(Case::ScreamingSnake),
            value: c.1.to_string(),
        })
        .sorted_by(|a, b| a.name.cmp(&b.name))
        .collect::<Vec<_>>();

    let r = Handlebars::new().render_template(
        TEMPLATE,
        &TemplateData {
            module: package.to_owned(),
            columns,
            registers,
            constants,
        },
    )?;

    if let Some(filename) = outfile.as_ref() {
        std::fs::File::create(filename)
            .with_context(|| format!("while creating `{}`", filename))?
            .write_all(r.as_bytes())
            .with_context(|| format!("while writing to `{}`", filename))?;
        super::gofmt(filename);
        Ok(())
    } else {
        println!("{}", r);
        Ok(())
    }
}
