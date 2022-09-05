use crate::compiler::{Ast, AstNode};
use color_eyre::eyre::*;

use std::io::{BufWriter, Write};

// fn render_node
pub struct LatexExporter;

fn render_node(n: &AstNode) -> Result<String> {
    unimplemented!()
}

impl crate::transpilers::Transpiler<Ast> for LatexExporter {
    fn render<'a>(&self, asts: &[Ast], mut out: BufWriter<Box<dyn Write + 'a>>) -> Result<()> {
        let r = asts
            .iter()
            .flat_map(|a| a.exprs.iter().map(|n| render_node(n)))
            .collect::<Result<Vec<_>>>()?
            .join("\n");

        writeln!(out, "{}", r).with_context(|| eyre!("rendering result"))
    }
}
