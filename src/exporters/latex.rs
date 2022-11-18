use crate::compiler::{Ast, AstNode, Token};
use anyhow::{Context, Result};
use convert_case::{Case, Casing};

use std::{cell::RefCell, io::Write, rc::Rc};

#[derive(Default)]
pub struct LatexExporter {
    pub constraints_filename: Option<String>,
    pub columns_filename: Option<String>,
    pub render_columns: bool,
}

fn sanitize(s: &str) -> String {
    s.replace('_', "\\_")
}

fn wrap_env(body: String, env: &str) -> String {
    format!("\\begin{{{}}}\n{}\n\\end{{{}}}\n", env, body, env)
}

fn dollarize(s: String, in_math: bool) -> String {
    if !in_math {
        format!("\\[{}\\]", s)
    } else {
        s
    }
}
fn textize(s: String, _: bool) -> String {
    format!("\\text{{{}}}", s)
}

impl LatexExporter {
    fn render_parenthesized(&mut self, e: &AstNode, in_maths: bool) -> Result<String> {
        if matches!(e.class, Token::Symbol(_) | Token::Value(_)) {
            self.render_node(e, in_maths)
        } else {
            Ok(format!("({})", self.render_node(e, in_maths)?))
        }
    }

    fn _flatten(&mut self, ax: &mut Vec<AstNode>, n: &AstNode) {
        ax.push(n.clone());
        match &n.class {
            Token::List(xs) => xs.iter().for_each(|x| self._flatten(ax, x)),
            Token::DefColumns(xs) => xs.iter().for_each(|x| self._flatten(ax, x)),
            Token::DefConstraint(_, _, x) => self._flatten(ax, x),
            _ => (),
        }
    }
    fn render_form(&mut self, args: &[AstNode], in_maths: bool) -> Result<String> {
        if args.is_empty() {
            Ok("()".into())
        } else {
            let fname = if let Token::Symbol(name) = &args[0].class {
                name
            } else {
                unreachable!()
            };
            match fname.as_str() {
                "nth" => Ok(dollarize(
                    format!(
                        "{}^{{{}}}",
                        self.render_node(&args[1], true)?,
                        self.render_node(&args[2], true)?,
                    ),
                    in_maths,
                )),
                "=" | "eq" => Ok(dollarize(
                    format!(
                        "{} = {}",
                        self.render_node(&args[1], true)?,
                        self.render_node(&args[2], true)?,
                    ),
                    in_maths,
                )),
                "*" => Ok(dollarize(
                    format!(
                        "{} \\times {}",
                        self.render_parenthesized(&args[1], true)?,
                        self.render_parenthesized(&args[2], true)?,
                    ),
                    in_maths,
                )),
                "-" => Ok(dollarize(
                    format!(
                        "{} - {}",
                        self.render_parenthesized(&args[1], true)?,
                        self.render_parenthesized(&args[2], true)?,
                    ),
                    in_maths,
                )),
                "+" => Ok(dollarize(
                    format!(
                        "{} + {}",
                        self.render_parenthesized(&args[1], true)?,
                        self.render_parenthesized(&args[2], true)?,
                    ),
                    in_maths,
                )),
                "bin-if-one" => Ok(dollarize(
                    format!(
                        "{} = 1 \\Rightarrow {}",
                        self.render_node(&args[1], true)?,
                        self.render_node(&args[2], true)?
                    ),
                    in_maths,
                )),
                // "if-not-zero" => Ok(
                // format!(
                //     "\\left[{} \\neq 0\\right] \\Rightarrow \\left[{}\\right]",
                //     self.render_node(&args[1], in_maths)?,
                //     self.render_node(&args[2], in_maths)?
                //     )
                // ),
                "if-eq" => Ok(format!(
                    "\\IF{} = {}\\THEN\n{}",
                    self.render_node(&args[1], in_maths)?,
                    self.render_node(&args[2], in_maths)?,
                    if args[3].depth() > 1 {
                        format!("\\left[{}\\right]", self.render_node(&args[3], in_maths)?)
                    } else {
                        self.render_node(&args[3], in_maths)?
                    }
                )),
                "if-not-zero" => Ok(if args[2].depth() > 1 {
                    format!(
                        "\\IF{} \\neq 0\\THEN\n{}",
                        self.render_node(&args[1], in_maths)?,
                        if args[2].depth() > 1 {
                            format!("\\left[{}\\right]", self.render_node(&args[2], in_maths)?)
                        } else {
                            self.render_node(&args[2], in_maths)?
                        }
                    )
                } else {
                    format!(
                        "\\left[{} \\neq 0\\right] \\Rightarrow \\left[{}\\right]",
                        self.render_node(&args[1], in_maths)?,
                        self.render_node(&args[2], in_maths)?
                    )
                }),
                "if-zero" => Ok(format!(
                    "\\IF{} = 0\\THEN\n{}",
                    self.render_node(&args[1], in_maths)?,
                    if args[2].depth() > 1 {
                        format!("\\left[{}\\right]", self.render_node(&args[2], in_maths)?)
                    } else {
                        self.render_node(&args[2], in_maths)?
                    }
                )),
                "will-eq" => Ok(format!(
                    "{}[t+1] = {}",
                    self.render_node(&args[1], in_maths)?,
                    self.render_node(&args[2], in_maths)?
                )),
                "vanishes" => Ok(format!("{} = 0", self.render_node(&args[1], in_maths)?,)),
                "begin" => Ok(format!(
                    "\\begin{{cases}}{}\\end{{cases}}",
                    &args[1..]
                        .iter()
                        .map(|n| self.render_node(n, true))
                        .collect::<Result<Vec<_>>>()?
                        .join("\\\\")
                )),
                _ => Ok(format!(
                    "{}({})",
                    self.render_node(&args[0], true)?,
                    &args[1..]
                        .iter()
                        .map(|a| self.render_node(a, true))
                        .collect::<Result<Vec<_>>>()?
                        .join(", ")
                )),
            }
        }
    }
    fn render_node(&mut self, n: &AstNode, in_maths: bool) -> Result<String> {
        match &n.class {
            Token::Value(x) => Ok(x.to_string()),
            Token::Symbol(name) => Ok(textize(sanitize(name), in_maths)),
            Token::DefConsts(cs) => {
                let body = cs
                    .iter()
                    .map(|c| format!("\\text{{{}}} \\triangleq {:?}", sanitize(&c.0), c.1))
                    .collect::<Vec<_>>()
                    .join("\\\\\n");

                Ok(format!(
                    "\\begin{{defconsts}}\\[{}\\]\\end{{defconsts}}",
                    if cs.len() > 1 {
                        wrap_env(body, "cases")
                    } else {
                        body
                    }
                ))
            }
            Token::DefAliases(cols) => {
                let body = cols
                    .iter()
                    .map(|col| self.render_node(col, true))
                    .collect::<Result<Vec<_>>>()?
                    .join("\\\\");

                Ok(format!(
                    "\\begin{{aliases}}\\[{}\\]\\end{{aliases}}",
                    if cols.len() > 1 {
                        wrap_env(body, "cases")
                    } else {
                        body
                    }
                ))
            }
            Token::DefAlias(from, to) => Ok(dollarize(
                format!(
                    "\\text{{{}}} \\triangleq \\text{{{}}}",
                    sanitize(from),
                    sanitize(to)
                ),
                in_maths,
            )),
            // Token::DefunAlias(from, to) => {
            //     Ok(format!("$\\text{{{}}} \\triangleq \\text{{{}}}$", from, to))
            // }
            Token::DefColumns(cols) => {
                let body = cols
                    .iter()
                    .map(|col| self.render_node(col, true))
                    .collect::<Result<Vec<_>>>()?
                    .join("\\\\\n");
                Ok(format!(
                    "\\begin{{defcols}}\\[{}\\]\\end{{defcols}}",
                    if cols.len() > 1 {
                        wrap_env(body, "cases")
                    } else {
                        body
                    }
                ))
            }
            Token::DefColumn(name, t, _) => Ok(format!("\\text{{{} \\emph{{{:?}}}}}", name, t)),
            Token::DefArrayColumn(name, range, t) => {
                Ok(format!("\\text{{{}{:?} \\emph{{{:?}}}}}", name, range, t))
            }
            Token::DefConstraint(name, domain, body) => Ok(format!(
                "\n\\begin{{constraint}}[{} {}]\n\\begin{{gather*}}\n{}\n\\end{{gather*}}\n\\end{{constraint}}\n",
                name.to_case(Case::Title),
                if domain.is_none() {
                    "".to_owned()
                } else {
                    format!("({})",
                             domain
                             .as_ref()
                             .map(|d| d
                                  .iter()
                                  .map(|x| x.to_string())
                                  .collect::<Vec<_>>()
                                  .join(", "))
                             .unwrap_or_else(|| "".into())
                    )
                },
                self.render_node(body, false)?,
            )),
            Token::List(args) => self.render_form(args, in_maths),
            _ => Ok(String::new()),
        }
    }

    pub fn render(&mut self, asts: &[Ast]) -> Result<()> {
        let s = Rc::new(RefCell::new(self));
        let r = asts
            .iter()
            .flat_map(|a| a.exprs.iter().map(|n| s.borrow_mut().render_node(n, false)))
            .collect::<Result<Vec<_>>>()?
            .join("\n");
        let body = format!(
            r#"
\documentclass{{article}}

\usepackage{{amssymb}}
\usepackage{{amsmath}}
\usepackage{{theorem}}
\usepackage{{algorithmic}}
\usepackage{{breqn}}
\usepackage[dvipsnames]{{xcolor}}

\theorembodyfont{{\rm}}
\newtheorem{{constraint}}{{Constraint}}
\newtheorem{{aliases}}{{Aliases}}
\newtheorem{{defcols}}{{Column Definitions}}
\newtheorem{{defconsts}}{{Constant Definitions}}

\newcommand{{\IF}}{{\text{{\textcolor{{WildStrawberry}}{{IF~}}}}}}
\newcommand{{\THEN}}{{\text{{\textcolor{{Cerulean}}{{~THEN~}}}}}}

\begin{{document}}
{}

\end{{document}}
"#,
            r
        );

        let filename = s.borrow().constraints_filename.clone();
        if let Some(filename) = filename.as_ref() {
            std::fs::File::create(filename)
                .with_context(|| format!("while creating `{}`", filename))?
                .write_all(body.as_bytes())
                .with_context(|| format!("while writing to `{}`", filename))
        } else {
            println!("{}", body);
            Ok(())
        }
    }
}
