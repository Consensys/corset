use std::{collections::HashMap, io::Write};

use anyhow::*;
use convert_case::{Case, Casing};
use handlebars::Handlebars;
use itertools::Itertools;
use serde::Serialize;

use crate::compiler::*;

#[derive(Serialize)]
struct GoConstant {
    name: String,
    value: String,
}
#[derive(Serialize)]
struct GoColumn {
    corset_name: String,
    go_name: String,
}
#[derive(Serialize)]
struct TemplateData {
    module: String,
    columns: Vec<GoColumn>,
    constants: Vec<GoConstant>,
}

pub fn render(cs: &ConstraintSet, package: &str, outfile: Option<&String>) -> Result<()> {
    const TEMPLATE: &'static str = include_str!("zkgeth.go");
    let columns = cs
        .modules
        .iter_cols()
        .filter_map(|(handle, c)| {
            if matches!(c.kind, Kind::Atomic) {
                Some(GoColumn {
                    corset_name: handle.name.to_string(),
                    go_name: handle.mangled_name(),
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
