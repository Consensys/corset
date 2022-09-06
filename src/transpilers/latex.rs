use crate::compiler::{Ast, AstNode, Token};
use color_eyre::eyre::*;
use convert_case::{Case, Casing};

use std::{
    cell::RefCell,
    io::{BufWriter, Write},
    rc::Rc,
};

#[derive(Default)]
pub struct LatexExporter {
    columns: Vec<Vec<String>>,
}

fn sanitize(s: &str) -> String {
    s.replace("_", "\\_")
}

fn dollarize(s: String, in_math: bool) -> String {
    if !in_math {
        format!("${}$", s)
    } else {
        s
    }
}
fn textize(s: String, in_math: bool) -> String {
    if in_math {
       format!("\\text{{{}}}", s)
    } else {
        s
    }
}
fn equize(s: String, in_math: bool) -> String {
    if in_math {
        format!("\\[{}\\]", s)
    } else {
        s
    }
}

impl LatexExporter {
    fn _flatten(&mut self, ax: &mut Vec<AstNode>, n: &AstNode) {
        ax.push(n.clone());
        match &n.class {
            Token::DefColumn(name) => self
                .columns
                .last_mut()
                .unwrap()
                .push(format!("\\text{{{}}}", name)),
            Token::Form(xs) => xs.iter().for_each(|x| self._flatten(ax, x)),
            Token::DefColumns(xs) => {
                self.columns.push(vec![]);
                xs.iter().for_each(|x| self._flatten(ax, x))
            }
            Token::DefConstraint(_, x) => self._flatten(ax, x),
            _ => return,
        }
    }
    fn flatten(&mut self, n: &AstNode) -> Vec<AstNode> {
        let mut ax = vec![];
        self._flatten(&mut ax, n);
        ax
    }
    fn render_form(&mut self, args: &[AstNode], in_maths: bool) -> Result<String> {
        if dbg!(args).is_empty() {
            return Ok("()".into());
        } else {
            let fname = if let Token::Symbol(name) = &args[0].class {
                name
            } else {
                unreachable!()
            };
            match fname.as_str() {
                "nth" => Ok(dollarize(format!(
                    "{}^{{{}}}",
                    self.render_node(&args[1], true)?,
                    self.render_node(&args[2], true)?,
                ), in_maths)),
                "=" | "eq" => Ok(dollarize(format!(
                    "{} = {}",
                    self.render_node(&args[1], true)?,
                    self.render_node(&args[2], true)?,
                ), in_maths)),
                "*" => Ok(dollarize(format!(
                    "{} \\times {}",
                    self.render_node(&args[1], true)?,
                    self.render_node(&args[2], true)?,
                ), in_maths)),
                "-" => Ok(dollarize(format!(
                    "{} - {}",
                    self.render_node(&args[1], true)?,
                    self.render_node(&args[2], true)?,
                ), in_maths)),
                "bin-if-one" => Ok(format!(
                    "IF {} = 1, THEN {} ENDIF",
                    self.render_node(&args[1], false)?,
                    self.render_node(&args[2], false)?
                )),
                "if-zero" => Ok(format!(
                    "IF {} = 0, THEN {} ENDIF",
                    self.render_node(&args[1], in_maths)?,
                    self.render_node(&args[2], in_maths)?
                )),
                x => Ok(dollarize(format!(
                    "{}({})",
                    self.render_node(&args[0], true)?,
                    &args[1..]
                        .iter()
                        .map(|a| self.render_node(a, true))
                        .collect::<Result<Vec<_>>>()?
                        .join(", ")
                ), in_maths)),
            }
        }
    }
    fn render_node(&mut self, n: &AstNode, in_maths: bool) -> Result<String> {
        match &n.class {
            Token::Value(x) => Ok(x.to_string()),
            Token::Symbol(name) => Ok(textize(sanitize(name), in_maths)),
            Token::DefConst(name, x) => {
                Ok(format!("\\text{{{}}} \\triangleq {}", sanitize(name), x))
            }

            Token::DefAliases(cols) => Ok(format!(
                "\\begin{{aliases}}Let \\[\\begin{{cases}} {} \\end{{cases}}\\]\\end{{aliases}}",
                cols.iter()
                    .map(|col| self.render_node(col, true))
                    .collect::<Result<Vec<_>>>()?
                    .join("\\\\")
            )),
            Token::DefAlias(from, to) => Ok(format!(
                "\\text{{{}}} \\triangleq \\text{{{}}}",
                sanitize(from),
                sanitize(to)
            )),

            Token::DefunAlias(from, to) => {
                // Ok(format!("$\\text{{{}}} \\triangleq \\text{{{}}}$", from, to))
                Ok(String::new())
            }

            Token::DefColumns(cols) => Ok(format!(
                "\\begin{{defcols}}Let \\[\\begin{{cases}} {} \\end{{cases}}\\]\\end{{defcols}}",
                cols.iter()
                    .map(|col| self.render_node(col, true))
                    .collect::<Result<Vec<_>>>()?
                    .join("\\\\")
            )),
            Token::DefColumn(name) => Ok(format!("\\text{{{}}}", sanitize(name))),
            Token::DefArrayColumn(name, range) => Ok(format!("{}{:?}", name, range)),

            Token::DefConstraint(name, body) => Ok(format!(
                "\n\\begin{{constraint}}[{}]\n{}\n\\end{{constraint}}\n",
                name.to_case(Case::Title),
                self.render_node(body, false)?,
            )),
            Token::Form(args) => self.render_form(&args, in_maths),
            Token::Defun(..) => Ok(String::new()),
            x => unimplemented!("{:?}", x),
        }
    }
}

impl crate::transpilers::Transpiler<Ast> for LatexExporter {
    fn render<'a>(&mut self, asts: &[Ast], mut out: BufWriter<Box<dyn Write + 'a>>) -> Result<()> {
        let s = Rc::new(RefCell::new(self));
        let r = asts
            .iter()
            .flat_map(|a| {
                a.exprs.iter().map(|n| {
                    s.borrow_mut().render_node(n, false)
                    // s.borrow_mut()
                    //     .flatten(n)
                    //     .into_iter()
                    //     .map(|n| s.borrow_mut().render_node(&n))
                })
            })
            .collect::<Result<Vec<_>>>()?
            .join("\n");
        let body = format!(
            r#"
\documentclass{{article}}

\usepackage{{amssymb}}
\usepackage{{amsmath}}
\usepackage{{theorem}}
\usepackage{{algorithmic}}

\theorembodyfont{{\rm}}
\newtheorem{{constraint}}{{Constraint}}
\newtheorem{{aliases}}{{Aliases}}
\newtheorem{{defcols}}{{Column Definitions}}

\begin{{document}}
{}

{}
\end{{document}}
"#,
            s.borrow()
                .columns
                .iter()
                .map(|cols| {
                    format!("\\begin{{itemize}}\n{}\n\\end{{itemize}}", cols.join("\n"))
                })
                .collect::<Vec<_>>()
                .join("\n"),
            r
        );

        writeln!(out, "{}", body.replace("\n\n", "\n")).with_context(|| eyre!("rendering result"))
    }
}
