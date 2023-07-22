#![allow(dead_code)] // TODO
use crate::compiler::{Ast, AstNode, Token, Type};
use crate::structs::Handle;
use anyhow::*;
use convert_case::{Case, Casing};
use handlebars::Handlebars;
use itertools::Itertools;
use log::error;
use serde::Serialize;

use std::{fs::File, io::Write};

#[derive(Default)]
pub struct LatexExporter {
    pub constraints_filename: Option<String>,
    pub columns_filename: Option<String>,
    pub render_columns: bool,
}

fn sanitize(s: &str) -> String {
    s.replace('_', "\\_")
}
fn romanize(s: &str) -> String {
    s.replace('1', "I")
        .replace('2', "II")
        .replace('3', "III")
        .replace('4', "IV")
}

fn wrap_env(body: String, env: &str) -> String {
    format!("\\begin{{{}}}\n{}\n\\end{{{}}}\n", env, body, env)
}

fn as_col(s: &str) -> String {
    format!("\\rlp{}", romanize(&s.to_case(Case::Pascal)))
}

fn render_parenthesized(e: &AstNode, state: State) -> Result<String> {
    if matches!(e.class, Token::Symbol(_) | Token::Value(_)) {
        render_node(e, state)
    } else {
        Ok(format!("({})", render_node(e, state)?))
    }
}

fn make_op(op: &str, args: &[AstNode], state: State) -> String {
    if args.len() > 2 {
        format!("\\\\\\hspace{{{}cm}}{}", state.indent, op)
    } else {
        op.to_owned()
    }
}

fn render_op(op: &str, args: &[AstNode], state: State) -> Result<String> {
    Ok(args
        .iter()
        .map(|a| render_node(a, state))
        .collect::<Result<Vec<_>>>()?
        .into_iter()
        .join(op))
}

fn render_if(args: &[AstNode], value: &str, reverse: bool, state: State) -> Result<String> {
    let op = if args.len() > 2 { "eIf" } else { "If" };
    let eq = if reverse { "$\\neq$" } else { "=" };
    let cond = render_node(&args[0], state)?.replace('\n', " ");
    let tthen = render_node(&args[1], state)?;
    let eelse = if let Some(x) = args.get(2) {
        format!("{{{}}}\n", render_node(x, state.in_maths(false))?)
    } else {
        "".into()
    };

    Ok(format!(
        "\\{}{{{} {} {}}}\n{{{}}}\n{}",
        op, cond, eq, value, tthen, eelse
    ))
}

#[derive(Copy, Clone)]
struct State<'a> {
    in_maths: bool,
    indent: f32,
    columns: &'a [String],
}
impl State<'_> {
    fn in_maths(self, in_maths: bool) -> Self {
        State { in_maths, ..self }
    }
    fn indent(self) -> Self {
        State {
            indent: self.indent + 0.5,
            ..self
        }
    }
}

fn render_form(args: &[AstNode], state: State) -> Result<String> {
    if args.is_empty() {
        Ok("()".into())
    } else {
        let fname = if let Token::Symbol(name) = &args[0].class {
            name
        } else {
            error!("{:?} unimplemented in LaTeX", args);
            return Ok(Default::default());
        };
        match fname.as_str() {
            "nth" => Ok(format!(
                "{}[{}]",
                render_node(&args[1], state)?,
                render_node(&args[2], state)?,
            )),
            "=" | "eq" => Ok(format!(
                "{} = {}",
                render_node(&args[1], state)?,
                render_node(&args[2], state)?,
            )),
            "*" => Ok(format!(
                "{} $\\times$ {}",
                render_parenthesized(&args[1], state)?,
                render_parenthesized(&args[2], state)?,
            )),
            "+" => render_op(&make_op("+", &args[1..], state.indent()), &args[1..], state),
            "-" => render_op(&make_op("-", &args[1..], state.indent()), &args[1..], state),
            "^" => Ok(format!(
                "{}\\textsuperscript{{{}}}",
                render_parenthesized(&args[1], state)?,
                render_parenthesized(&args[2], state)?,
            )),
            "prev" => Ok(format!(
                "${}_{{i-1}}$",
                render_node(
                    &args[1],
                    State {
                        in_maths: true,
                        ..state
                    },
                )?
            )),
            "next" => Ok(format!(
                "${}_{{i+1}}$",
                render_node(&args[1], state.in_maths(true))?,
            )),
            "inc" => Ok(format!(
                "${}_{{i+1}} = {}_i + {}$",
                render_node(&args[1], state.in_maths(true))?,
                render_node(&args[1], state.in_maths(true))?,
                render_node(&args[2], state.in_maths(true))?,
            )),
            "remains-constant" => Ok(format!(
                "${}_{{i+1}} - {}_{{i}}$",
                render_node(&args[1], state.in_maths(true))?,
                render_node(&args[1], state.in_maths(true))?
            )),
            "did-change" => Ok(format!(
                "${}_{{i}} \neq {}_{{i-1}}$",
                render_node(&args[1], state.in_maths(true))?,
                render_node(&args[1], state.in_maths(true))?
            )),
            "didnt-change" => Ok(format!(
                "${}_{{i}} - {}_{{i-1}}$",
                render_node(&args[1], state.in_maths(true))?,
                render_node(&args[1], state.in_maths(true))?
            )),
            "if-eq-else" | "if-eq" => render_if(
                std::iter::once(&args[1])
                    .chain(args.iter().skip(3))
                    .cloned()
                    .collect::<Vec<_>>()
                    .as_slice(),
                &render_node(&args[2], state)?,
                false,
                state,
            ),
            "if-zero" => render_if(&args[1..], "0", false, state),
            "if-not-zero" => render_if(&args[1..], "0", true, state),
            "will-eq" => Ok(format!(
                "{}[i+1] = {}",
                render_node(&args[1], state)?,
                render_node(&args[2], state)?
            )),
            "byte-shift" => Ok(format!(
                "{} $\\gg$ {}",
                render_node(&args[1], state)?,
                render_node(&args[2], state)?
            )),
            "vanishes" => Ok(format!("{} = 0", render_node(&args[1], state)?,)),
            "let" => {
                let mut r = String::new();
                for xs in args[1]
                    .as_list()
                    .unwrap()
                    .iter()
                    .map(|x| x.as_list().unwrap())
                {
                    let k = &xs[0];
                    let v = &xs[1];
                    r += &format!(
                        "\\texttt{{{}}} $\\leftarrow$ {}\\;\n",
                        render_node(k, state)?,
                        render_node(v, state)?
                    );
                }
                r += &render_node(&args[2], state)?;
                Ok(r)
            }
            "begin" => Ok(args[1..]
                .iter()
                .map(|n| render_node(n, state.in_maths(true)))
                .collect::<Result<Vec<_>>>()?
                .join("\\;\n")),
            _ => Ok(format!(
                "\\FuncSty{{{}(}}\n{}\n\\FuncSty{{)}}",
                render_node(&args[0], state.in_maths(true))?,
                &args[1..]
                    .iter()
                    .map(|a| render_node(a, state.in_maths(false)))
                    .collect::<Result<Vec<_>>>()?
                    .join(", ")
            )),
        }
    }
}
fn with_env(env: &str, x: &str) -> String {
    format!("\\{}{{{}}}", env, x)
}

fn render_node(n: &AstNode, state: State) -> Result<String> {
    match &n.class {
        Token::Value(x) => Ok(x.to_string()),
        Token::Symbol(name) => Ok(if state.columns.contains(name) {
            as_col(name.as_str())
        } else {
            with_env("texttt", &sanitize(name))
        }),
        Token::DefConsts(cs) => {
            let body = cs
                .iter()
                .map(|c| format!("\\text{{{}}} \\triangleq {:?}", sanitize(c.0.as_symbol().unwrap()), c.1))
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
        Token::DefConstraint {
            name,
            domain,
            guard: _,
            perspective: _,
            body,
        } => Ok(format!(
            "\n\\begin{{constraint}}[{}{} {}]\n\\begin{{gather*}}\n{}\n\\end{{gather*}}\n\\end{{constraint}}\n",
            name.to_case(Case::Title),
            "",
            // perspective.as_ref().map(|p| format!(" (when {})", p)).unwrap_or_default(),
            if domain.is_none() {
                "".to_owned()
            } else {
                format!("({:?})", domain)
            },
            render_node(body, state.in_maths(false))?,
        )),
        Token::List(args) => render_form(args.into(), state),
        _ => Ok(String::new()),
    }
}

const CONSTRAINT_TEMPLATE: &str = include_str!("constraint.tex");
#[derive(Serialize)]
struct LatexTemplate {
    caption: String,
    content: String,
}
fn render_constraints(asts: &[Ast], columns: &[String]) -> Result<String> {
    let mut r = String::new();
    for constraint in asts.iter().flat_map(|ast| constraints(ast).into_iter()) {
        r += "\n";
        let state = State {
            in_maths: false,
            indent: 0.,
            columns,
        };
        r += &Handlebars::new().render_template(
            CONSTRAINT_TEMPLATE,
            &LatexTemplate {
                caption: constraint.h.name.to_owned(),
                content: render_node(&constraint.e, state)?,
            },
        )?;
        r += "\n";
    }
    Ok(r)
}

type LatexConst = (String, AstNode);
struct LatexConstraint {
    h: Handle,
    e: AstNode,
}
struct LatexColumn {
    name: String,
    t: Type,
    is_array: bool,
}

fn constraints(ast: &Ast) -> Vec<LatexConstraint> {
    let mut module = "<prelude>".to_string();

    ast.exprs
        .iter()
        .filter_map(|n| match &n.class {
            Token::DefModule(m) => {
                module = m.to_owned();
                None
            }
            Token::DefConstraint {
                name,
                domain: _domain,
                guard: _guard,
                body,
                ..
            } => {
                let h = Handle::new(&module, name);
                Some(LatexConstraint {
                    h,
                    e: *body.to_owned(),
                })
            }
            // Token::DefPermutation { from, to } => todo!(),
            // Token::DefPlookup {
            //     name,
            //     including,
            //     included,
            // } => todo!(),
            // Token::DefInrange(_, _) => todo!(),
            _ => None,
        })
        .collect()
}

fn consts(ast: &Ast) -> Vec<LatexConst> {
    fn _consts(n: &AstNode, consts: &mut Vec<LatexConst>) {
        if let Token::DefConsts(cs) = &n.class {
            for (name, exp) in cs.iter() {
                consts.push((name.as_symbol().unwrap().to_owned(), *exp.to_owned()))
            }
        } else {
            unreachable!()
        }
    }

    ast.exprs.iter().fold(Vec::new(), |mut consts, n| {
        _consts(n, &mut consts);
        consts
    })
}

fn columns(ast: &Ast) -> Vec<LatexColumn> {
    fn _columns(n: &AstNode, cols: &mut Vec<LatexColumn>) {
        match &n.class {
            Token::List(ns) | Token::DefAliases(ns) | Token::DefColumns(ns) => {
                for n in ns.iter() {
                    _columns(n, cols);
                }
            }
            Token::DefColumn { name, t, .. } => cols.push(LatexColumn {
                name: name.to_owned(),
                t: *t,
                is_array: false,
            }),
            Token::DefArrayColumn { name, t, .. } => cols.push(LatexColumn {
                name: name.to_owned(),
                t: *t,
                is_array: true,
            }),
            Token::DefAlias(from, _to) => cols.push(LatexColumn {
                name: from.to_owned(),
                t: Type::Void,
                is_array: false,
            }),
            _ => (),
        }
    }

    ast.exprs.iter().fold(Vec::new(), |mut cols, n| {
        _columns(n, &mut cols);
        cols
    })
}

fn render_columns(asts: &[Ast]) -> Result<(String, Vec<String>)> {
    let mut column_symbols = Vec::new();
    let mut r = String::new();
    for col in asts.iter().flat_map(|ast| columns(ast).into_iter()) {
        column_symbols.push(col.name.clone());
        let suffix = if col.name.to_lowercase().ends_with("stamppp") {
            "\\blacksquare"
        } else {
            ""
        };
        r += &format!(
            "\\newcommand{{{}}}{}{{\\col{{{}{}}}{}}}\n",
            as_col(&col.name),
            if col.is_array { "[1]" } else { "" },
            sanitize(&col.name),
            if col.is_array { "\\_#" } else { "" },
            suffix,
        );
    }
    Ok((r, column_symbols))
}

pub fn render(asts: &[Ast], constraints_file: Option<String>) -> Result<()> {
    if let Some(constraints_file) = constraints_file.as_ref() {
        let mut out = File::create(constraints_file)
            .with_context(|| anyhow!("while opening {}", constraints_file))?;
        out.write_all(
            r#"
\documentclass{article}
\usepackage{algorithm2e}
\usepackage{amsmath}

\newcommand{\col}[1]{
  \ifmmode
    \mathsf{#1}
  \else
    \textsf{#1}
  \fi
}


"#
            .as_bytes(),
        )?;
        let columns = render_columns(asts)?;
        out.write_all(columns.0.as_bytes())?;
        out.write_all("\n\n\\begin{document}\n".as_bytes())?;
        out.write_all(render_constraints(asts, &columns.1)?.as_bytes())?;
        out.write_all("\\end{document}".as_bytes())?;
    }
    Ok(())
}
