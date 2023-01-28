use std::{collections::HashMap, io::Write};

use anyhow::*;
use convert_case::{Case, Casing};
use itertools::Itertools;
use num_bigint::BigInt;

use crate::compiler::*;

#[derive(Debug)]
pub struct GoExporter {
    pub package: String,
    pub filename: Option<String>,
}
impl GoExporter {
    fn render_consts(&self, consts: &HashMap<Handle, BigInt>) -> String {
        consts
            .iter()
            .sorted_by_key(|(handle, _)| handle.to_string())
            .fold(String::new(), |mut ax, (handle, value)| {
                ax.push_str(&format!(
                    "const {} = {}\n",
                    handle.mangled_name().to_case(Case::ScreamingSnake),
                    value
                ));
                ax
            })
    }

    fn render_columns(&self, cs: &ConstraintSet) -> String {
        let mut r = format!(
            r#"
package {}

import (
    "github.com/ethereum/go-ethereum/zk-evm/zeroknowledge/witnessdata/column"
)
"#,
            self.package,
        );

        r += &self.render_consts(&cs.constants);

        r += "const (\n";
        r += &cs
            .modules
            .iter()
            .filter_map(|(handle, col)| {
                if let Kind::Atomic = col.kind {
                    Some(format!(
                        "{} column.Column = \"{}\"",
                        handle.mangled_name(),
                        handle.mangled_name()
                    ))
                } else {
                    None
                }
            })
            .sorted()
            .collect::<Vec<_>>()
            .join("\n");
        r += ")\n\n";

        // r += "var AllColumns = []column.Description{";
        // r.push_str(&format!(
        //     "\n{}\n",
        //     cs.modules
        //         .iter()
        //         .filter_map(|(handle, col)| if let Kind::Atomic = col.kind {
        //             Some(format!("{},", handle.mangled_name()))
        //         } else {
        //             None
        //         })
        //         .sorted()
        //         .collect::<Vec<_>>()
        //         .join("\n")
        // ));
        // r += "}\n\n";

        r
    }

    pub fn render(&mut self, cs: &ConstraintSet) -> Result<()> {
        let columns = self.render_columns(cs);

        if let Some(ref filename) = self.filename {
            std::fs::File::create(filename)
                .with_context(|| format!("while creating `{}`", filename))?
                .write_all(columns.as_bytes())
                .with_context(|| format!("while writing to `{}`", filename))?;
            super::gofmt(filename);
        } else {
            println!("{}", columns);
        };

        Ok(())
    }
}
