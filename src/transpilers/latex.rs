use crate::compiler::{Ast, AstNode, Token};
use color_eyre::eyre::*;

use std::io::{BufWriter, Write};

// fn render_node
pub struct LatexExporter;

fn flatten(n: &AstNode) -> Vec<AstNode> {
    fn _flatten(ax: &mut Vec<AstNode>, n: &AstNode) {
        ax.push(n.clone());
        match &n.class {
            Token::Form(xs) => xs.iter().for_each(|x| _flatten(ax, x)),
            Token::DefColumns(xs) => xs.iter().for_each(|x| _flatten(ax, x)),
            Token::DefConstraint(_, x) => _flatten(ax, x),
            _ => return,
        }
    }

    let mut ax = vec![];
    _flatten(&mut ax, n);
    ax
}

fn render_node(n: &AstNode) -> Result<String> {
    match &n.class {
        Token::DefColumn(name) => Ok(format!("\\defcolumn{{{}}}", name)),
        _ => Ok(String::new()),
        x => unimplemented!("{:?}", x),
    }
}

impl crate::transpilers::Transpiler<Ast> for LatexExporter {
    fn render<'a>(&self, asts: &[Ast], mut out: BufWriter<Box<dyn Write + 'a>>) -> Result<()> {
        let r = asts
            .iter()
            .flat_map(|a| {
                a.exprs
                    .iter()
                    .flat_map(|n| flatten(n).into_iter().map(|n| render_node(&n)))
            })
            .collect::<Result<Vec<_>>>()?
            .join("\n");

        writeln!(out, "{}", r).with_context(|| eyre!("rendering result"))
    }
}
