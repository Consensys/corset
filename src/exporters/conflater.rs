use std::{cmp::Ordering, collections::HashMap, io::Write};

use crate::compiler::{ConstraintSet, Kind};
use anyhow::*;
use convert_case::{Case, Casing};
use handlebars::Handlebars;
use itertools::Itertools;
use serde::Serialize;

#[derive(Serialize)]
struct ConflaterModule {
    member_name: String,
    klass_name: String,
    original_name: String,
    columns: Vec<ConflaterColumn>,
}

#[derive(Serialize)]
struct ConflaterColumn {
    json_name: String,
    safe_name: String,
}

#[derive(Serialize)]
struct TemplateData {
    modules: Vec<ConflaterModule>,
}

pub fn render(cs: &ConstraintSet, outfile: Option<&String>) -> Result<()> {
    const TEMPLATE: &str = include_str!("conflater.kt");
    let mut modules: HashMap<String, Vec<ConflaterColumn>> = Default::default();
    for c in cs.columns.iter_cols() {
        if matches!(c.kind, Kind::Atomic) {
            modules
                .entry(c.handle.module.to_owned())
                .or_default()
                .push(ConflaterColumn {
                    json_name: c.handle.name.to_owned(),
                    safe_name: c.handle.name.to_case(Case::ScreamingSnake),
                });
        }
    }

    let modules = modules
        .into_iter()
        .sorted_by(|x, y| {
            // XXX it is *CAPITAL* for the hub to be the first in the list
            // so that it can update its stamps
            if x.0 == "hub" {
                Ordering::Less
            } else if y.0 == "hub" {
                Ordering::Greater
            } else {
                x.0.cmp(&y.0)
            }
        })
        .map(|(module, cols)| ConflaterModule {
            member_name: module.to_case(Case::Camel),
            klass_name: module.to_case(Case::Pascal),
            original_name: module,
            columns: cols
                .into_iter()
                .sorted_by_cached_key(|c| c.safe_name.to_owned())
                .collect(),
        })
        .collect::<Vec<_>>();

    let r = Handlebars::new().render_template(TEMPLATE, &TemplateData { modules })?;
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
