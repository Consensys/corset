use log::*;
use num_bigint::BigInt;
use std::{collections::HashMap, io::Write};

use convert_case::{Case, Casing};
use eyre::*;

use crate::{
    column::{Column, ColumnSet},
    compiler::*,
};

const ARRAY_SEPARATOR: char = '_';
const MODULE_SEPARATOR: &str = "___";

pub(crate) struct WizardIOP {
    pub out_filename: Option<String>,
    pub package: String,
}

impl WizardIOP {
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
                    args: args.iter().map(|a| Self::shift(a, i)).collect(),
                },
            },
            Expression::Const(_) => e.clone(),
            Expression::Column(..) | Expression::ArrayColumnElement(..) => Expression::Funcall {
                func: Builtin::Shift,
                args: vec![e.clone(), Expression::Const(BigInt::from(i))],
            },
            Expression::List(xs) => {
                Expression::List(xs.iter().map(|x| Self::shift(x, i)).collect())
            }
            Expression::ArrayColumn(_, _, _, _) => unreachable!(),
        }
    }

    fn render_funcall(func: &Builtin, args: &[Expression]) -> String {
        todo!()
    }

    fn render_expression(e: &Expression) -> String {
        match e {
            Expression::ArrayColumn(..) => unreachable!(),
            Expression::Const(x) => x.to_string(), // TODO
            Expression::Column(module, name, _, _) => {
                format!(
                    "{}{}{}",
                    module.to_case(Case::ScreamingSnake),
                    MODULE_SEPARATOR,
                    name.to_case(Case::Snake)
                )
            }
            Expression::ArrayColumnElement(module, name, i, _) => format!(
                "{}{}{}{}{}",
                module,
                MODULE_SEPARATOR,
                name.to_case(Case::ScreamingSnake),
                ARRAY_SEPARATOR,
                i,
            ),
            Expression::Funcall { func, args } => Self::render_funcall(func, args),
            Expression::List(constraints) => constraints
                .iter()
                .map(|x| Self::render_expression(x))
                .map(|mut x| {
                    if let Some(true) = x.chars().last().map(|c| c != ',') {
                        x.push(',');
                    }
                    x
                })
                .collect::<Vec<_>>()
                .join("\n"),
        }
    }

    fn render_constraint(
        name: &str,
        constraint: &Expression,
        domain: Option<Vec<isize>>,
    ) -> String {
        match constraint {
            Expression::List(_) => todo!(),
            _ => todo!(),
        }
    }

    fn render_constraints(constraints: &[Constraint]) -> String {
        constraints
            .iter()
            .filter_map(|constraint| match constraint {
                Constraint::Vanishes { name, domain, expr } => match domain {
                    None => Some(format!(
                        "build.GlobalConstraint(\"{}\", {})",
                        name.to_case(Case::ScreamingSnake),
                        Self::render_expression(expr)
                    )),
                    Some(domain) => Some(
                        domain
                            .iter()
                            .map(|x| {
                                format!(
                                    "build.LocalConstraint(\"{}\", {})",
                                    name.to_case(Case::ScreamingSnake),
                                    Self::render_expression(&Self::shift(expr, *x))
                                )
                            })
                            .collect::<Vec<_>>()
                            .join("\n"),
                    ),
                },
                _ => None,
            })
            .collect::<Vec<String>>()
            .join("\n")
    }

    fn render_constants(consts: &HashMap<String, i64>) -> String {
        if consts.is_empty() {
            return String::default();
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
                    Column::Atomic(content, _) => r.push_str(&format!(
                        "{}{}{} := build.RegisterCommit(\"{}\", TODO)\n",
                        module,
                        MODULE_SEPARATOR,
                        name.to_case(Case::ScreamingSnake),
                        name.to_case(Case::ScreamingSnake)
                    )),
                    Column::Array { range, content } => {
                        for i in range {
                            r.push_str(&format!(
                                "{}{}{}{}{} := build.RegisterCommit(\"{}{}{}{}{}\", TODO)\n",
                                module,
                                MODULE_SEPARATOR,
                                name.to_case(Case::ScreamingSnake),
                                ARRAY_SEPARATOR,
                                i,
                                module,
                                MODULE_SEPARATOR,
                                name.to_case(Case::ScreamingSnake),
                                ARRAY_SEPARATOR,
                                i
                            ))
                        }
                    }
                    _ => {}
                }
            }
        }

        r
    }

    pub fn render(&mut self, cs: &ConstraintsSet) -> Result<()> {
        let consts = Self::render_constants(&cs.constants);
        let columns = Self::render_columns(&cs.columns);
        let constraints = "";
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
