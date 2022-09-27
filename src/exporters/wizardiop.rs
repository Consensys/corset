use num_bigint::BigInt;
use std::{collections::HashMap, io::Write};

use convert_case::{Case, Casing};
use eyre::*;

use crate::{
    column::{Column, ColumnSet},
    compiler::*,
};

use super::goize;

const ARRAY_SEPARATOR: char = '_';
const MODULE_SEPARATOR: &str = "___";

fn shift(e: &Expression, i: isize) -> Expression {
    match e {
        Expression::Funcall { func, args } => match func {
            Builtin::Shift => {
                if let Expression::Const(j) = &args[1] {
                    Expression::Funcall {
                        func: Builtin::Shift,
                        args: vec![args[0].clone(), Expression::Const(BigInt::from(i) + j)],
                    }
                } else {
                    unreachable!()
                }
            }
            _ => Expression::Funcall {
                func: *func,
                args: args.iter().map(|a| shift(a, i)).collect(),
            },
        },
        Expression::Const(_) => e.clone(),
        Expression::Column(..) => Expression::Funcall {
            func: Builtin::Shift,
            args: vec![e.clone(), Expression::Const(BigInt::from(i))],
        },
        Expression::List(xs) => Expression::List(xs.iter().map(|x| shift(x, i)).collect()),
        Expression::ArrayColumn(_, _, _, _) => unreachable!(),
        Expression::Void => Expression::Void,
    }
}

fn make_chain(xs: &[Expression], operand: &str, surround: bool) -> String {
    let head = render_expression(&xs[0]);
    let tail = &xs[1..];
    if xs.len() > 2 {
        let tail = tail
            .iter()
            .map(|x| format!("{}({})", operand, render_expression(x)))
            .collect::<Vec<_>>()
            .join(".");
        let chain = format!("{}.{}", head, tail);
        if surround {
            format!("({})", chain)
        } else {
            chain
        }
    } else {
        format!("{}.{}({})", head, operand, render_expression(&xs[1]))
    }
}

fn render_expression(e: &Expression) -> String {
    match e {
        Expression::ArrayColumn(..) => unreachable!(),
        Expression::Const(x) => format!("symbolic.NewConstant(\"{}\")", x),
        Expression::Column(module, name, _, _) => {
            format!(
                "{}{}{}.AsVariable()",
                goize(module),
                MODULE_SEPARATOR,
                goize(&name.to_case(Case::ScreamingSnake))
            )
        }
        Expression::Funcall { func, args } => render_funcall(func, args),
        Expression::List(constraints) => constraints
            .iter()
            .map(render_expression)
            .map(|mut x| {
                if let Some(true) = x.chars().last().map(|c| c != ',') {
                    x.push(',');
                }
                x
            })
            .collect::<Vec<_>>()
            .join("\n"),
        Expression::Void => String::new(),
    }
}

fn render_funcall(func: &Builtin, args: &[Expression]) -> String {
    match func {
        Builtin::Add => make_chain(args, "Add", true),
        Builtin::Mul => make_chain(args, "Mul", false),
        Builtin::Sub => make_chain(args, "Sub", true),
        Builtin::Neg => format!("({}).Neg()", render_expression(&args[0])),
        Builtin::Shift => {
            let leaf = match &args[0] {
                Expression::Column(module, name, ..) => {
                    format!(
                        "{}{}{}",
                        goize(module),
                        MODULE_SEPARATOR,
                        goize(&name.to_case(Case::ScreamingSnake))
                    )
                }
                _ => unreachable!(),
            };
            format!(
                "({}).Shift({}).AsVariable()",
                leaf,
                if let Expression::Const(x) = &args[1] {
                    x
                } else {
                    unreachable!()
                }
            )
        }
        x => {
            unimplemented!("{:?}", x)
        }
    }
}

pub struct WizardIOP {
    pub out_filename: Option<String>,
    pub package: String,
}

impl WizardIOP {
    fn render_constraint(name: &str, domain: Option<Vec<isize>>, expr: &Expression) -> String {
        match expr {
            Expression::List(xs) => xs
                .iter()
                .enumerate()
                .map(|(i, x)| {
                    Self::render_constraint(&format!("{}_{}", name, i), domain.clone(), x)
                })
                .collect::<Vec<_>>()
                .join("\n"),
            _ => match domain {
                None => format!(
                    "build.GlobalConstraint(\"{}\", {})",
                    name.to_case(Case::ScreamingSnake),
                    render_expression(expr)
                ),
                Some(domain) => domain
                    .iter()
                    .map(|x| {
                        format!(
                            "build.LocalConstraint(\"{}\", {})",
                            name.to_case(Case::ScreamingSnake),
                            render_expression(&shift(expr, *x))
                        )
                    })
                    .collect::<Vec<_>>()
                    .join("\n"),
            },
        }
    }

    fn render_constraints(constraints: &[Constraint]) -> String {
        constraints
            .iter()
            .filter_map(|constraint| match constraint {
                Constraint::Vanishes { name, domain, expr } => {
                    Some(Self::render_constraint(name, domain.clone(), expr))
                }
                _ => None,
            })
            .collect::<Vec<String>>()
            .join("\n")
    }

    fn render_constants(consts: &HashMap<String, i64>) -> String {
        if consts.is_empty() {
            String::default()
        } else {
            format!(
                "const (\n{}\n)",
                consts.iter().fold(String::new(), |mut ax, (name, value)| {
                    ax.push_str(&format!("{} int = {}\n", name, value));
                    ax
                })
            )
        }
    }

    fn render_columns<T>(cols: &ColumnSet<T>) -> String {
        let mut r = String::new();
        for (module, m) in cols.cols.iter() {
            for (name, col) in m.iter() {
                match col {
                    Column::Atomic { .. } => r.push_str(&format!(
                        "{}{}{} := build.RegisterCommit(\"{}{}{}\", 2048)\n",
                        goize(module),
                        MODULE_SEPARATOR,
                        goize(&name.to_case(Case::ScreamingSnake)),
                        goize(module),
                        MODULE_SEPARATOR,
                        goize(&name.to_case(Case::ScreamingSnake))
                    )),
                    Column::Array { range, .. } => {
                        for i in range {
                            r.push_str(&format!(
                                "{}{}{}{}{} := build.RegisterCommit(\"{}{}{}{}{}\", 2048)\n",
                                goize(module),
                                MODULE_SEPARATOR,
                                goize(&name.to_case(Case::ScreamingSnake)),
                                ARRAY_SEPARATOR,
                                i,
                                goize(module),
                                MODULE_SEPARATOR,
                                goize(&name.to_case(Case::ScreamingSnake)),
                                ARRAY_SEPARATOR,
                                i
                            ))
                        }
                    }
                    Column::Composite { .. } => r.push_str(&format!(
                        "{}{}{} := build.RegisterCommit(\"{}{}{}\", 2048)\n",
                        goize(module),
                        MODULE_SEPARATOR,
                        goize(&name.to_case(Case::ScreamingSnake)),
                        goize(module),
                        MODULE_SEPARATOR,
                        goize(&name.to_case(Case::ScreamingSnake))
                    )),
                    _ => {}
                }
            }
        }

        r
    }

    pub fn render(&mut self, cs: &ConstraintSet) -> Result<()> {
        let consts = Self::render_constants(&cs.constants);
        let columns = Self::render_columns(&cs.columns);
        let constraints = Self::render_constraints(&cs.constraints);
        let plookups = "";

        let r = format!(
            r#"
package {}

import (
    "github.com/consensys/accelerated-crypto-monorepo/protocol/commitment"
    "github.com/consensys/accelerated-crypto-monorepo/protocol/wizard"
)

{}

func Define(build *wizard.Builder) {{
//
// Columns declarations
//
{}


//
// Constraints declarations
//
{}


//
// PLookups declarations
//
{}

}}
"#,
            &self.package, consts, columns, constraints, plookups,
        );

        if let Some(filename) = self.out_filename.as_ref() {
            std::fs::File::create(filename)
                .with_context(|| format!("while creating `{}`", filename))?
                .write_all(r.as_bytes())
                .with_context(|| format!("while writing to `{}`", filename))?;
            super::gofmt(filename);
        } else {
            println!("{}", r);
        }
        Ok(())
    }
}
