use log::*;
use std::io::Write;

use color_eyre::eyre::*;
use convert_case::{Case, Casing};

use crate::{
    column::{Column, ColumnSet},
    compiler::*,
};

const ARRAY_SEPARATOR: char = '_';

fn make_go_function(name: &str, prelude: &str, content: &str, postlude: &str, ret: &str) -> String {
    format!(
        r#"func {}() (r {}) {{
{}
{}
{}
return
}}
"#,
        name.to_case(Case::Camel),
        ret,
        prelude,
        content,
        postlude,
    )
}

fn gofmt(filename: &str) {
    info!("Running gofmt on {}... ", filename);
    let output = std::process::Command::new("gofmt")
        .args(["-w", filename])
        .output()
        .expect("failed to execute gofmt");
    if output.status.success() {
        info!("done.");
    } else {
        error!("failed:");
        error!("STDOUT:\n{}", std::str::from_utf8(&output.stdout).unwrap());
        error!("STDERR:\n{}", std::str::from_utf8(&output.stderr).unwrap());
    }
}

#[derive(Debug)]
pub(crate) struct GoExporter {
    pub fname: String,
    pub package: String,
    pub ce: String,

    pub constraints_filename: Option<String>,
    pub columns_filename: Option<String>,
    pub render_columns: bool,
}
impl GoExporter {
    fn make_chain(&self, xs: &[Expression], operand: &str, surround: bool) -> Result<String> {
        let head = self.render_node(&xs[0])?;
        let tail = &xs[1..];
        if xs.len() > 2 {
            let tail = tail
                .iter()
                .map(|x| self.render_node(x).map(|s| format!("{}({})", operand, s)))
                .collect::<Result<Vec<_>>>()?
                .join(".");
            let chain = format!("{}.{}", head, tail);
            Ok(if surround {
                format!("({})", chain)
            } else {
                chain
            })
        } else {
            Ok(format!(
                "{}.{}({})",
                head,
                operand,
                self.render_node(&xs[1])?
            ))
        }
    }

    pub fn render_node(&self, node: &Expression) -> Result<String> {
        let r = match node {
            Expression::ArrayColumn(..) => unreachable!(),
            Expression::Const(x) => Ok(format!("column.CONST_STRING(\"{}\")", x)),
            Expression::Column(_module, name, _, _) => Ok(format!(
                "{}[\"{}\"]",
                self.ce,
                name.to_case(Case::UpperSnake)
            )),
            Expression::ArrayColumnElement(name, i, _) => Ok(format!(
                "{}[{}{}{}.Name()]",
                self.ce,
                name.to_case(Case::UpperSnake),
                ARRAY_SEPARATOR,
                i
            )),
            Expression::Funcall { func, args } => self.render_funcall(func, args),
            Expression::List(constraints) => Ok(constraints
                .iter()
                .map(|x| self.render_node(x))
                .map(|x| {
                    x.map(|mut r| {
                        if let Some(true) = r.chars().last().map(|c| c != ',') {
                            r.push(',');
                        }
                        r
                    })
                })
                .collect::<Result<Vec<_>>>()?
                .join("\n")),
        }?;

        Ok(r)
    }
    pub fn render_funcall(&self, func: &Builtin, args: &[Expression]) -> Result<String> {
        let r = match func {
            Builtin::Add => self.make_chain(args, "Add", true),
            Builtin::Mul => self.make_chain(args, "Mul", false),
            Builtin::Sub => self.make_chain(args, "Sub", true),
            Builtin::Inv => Ok(format!("({}).Inv()", self.render_node(&args[0])?)),
            Builtin::Neg => Ok(format!("({}).Neg()", self.render_node(&args[0])?)),
            Builtin::Shift => Ok(format!(
                "({}).Shift({})",
                self.render_node(&args[0])?,
                if let Expression::Const(x) = &args[1] {
                    x
                } else {
                    unreachable!()
                }
            )),
            x => {
                unimplemented!("{:?}", x)
            }
        }?;
        Ok(r)
    }

    fn render_columns<T>(&self, cols: &ColumnSet<T>) -> String {
        let mut r = format!(
            r#"
package {}

import (
    "github.com/ethereum/go-ethereum/zk-evm/zeroknowledge/witnessdata/column"
    "github.com/ethereum/go-ethereum/zk-evm/zeroknowledge/witnessdata/module"
)

const (
"#,
            self.package,
        );

        for (_module, m) in cols.cols.iter() {
            for (name, col) in m.iter() {
                match col {
                    Column::Atomic(_, _) => r.push_str(&format!(
                        "{} column.Column = \"{}\"\n",
                        name.to_case(Case::ScreamingSnake),
                        name.to_case(Case::ScreamingSnake)
                    )),
                    Column::Array { range, content } => {
                        for i in range {
                            r.push_str(&format!(
                                "{}{}{} column.Column = \"{}{}{}\"\n",
                                name.to_case(Case::ScreamingSnake),
                                ARRAY_SEPARATOR,
                                i,
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
        r += ")\n\n";

        for (_module, m) in cols.cols.iter() {
            for (name, col) in m.iter() {
                match col {
                    Column::Atomic(..) => {}
                    Column::Array { .. } => {}
                    Column::Composite { value, exp } => todo!(),
                    Column::Interleaved { from, .. } => r.push_str(&format!(
                        "var {} = column.Interleaved{{{}}}\n",
                        name.to_case(Case::ScreamingSnake),
                        from.join(", ")
                    )),
                }
            }
        }

        r.push_str(&format!(
            "var AllColumns = column.BuildColumnList(\n{}\n)\n",
            cols.cols
                .values()
                .flat_map(|module| module.iter())
                .map(|(name, col)| match col {
                    Column::Atomic(_, _) =>
                        format!("{}.Name()", name.to_case(Case::ScreamingSnake)),
                    Column::Array { range, .. } => {
                        range
                            .iter()
                            .map(|i| {
                                format!(
                                    "{}{}{}.Name()",
                                    name.to_case(Case::ScreamingSnake),
                                    ARRAY_SEPARATOR,
                                    i,
                                )
                            })
                            .collect::<Vec<_>>()
                            .join("\n")
                    }
                    _ => "".into(),
                })
                .collect::<Vec<_>>()
                .join("\n")
        ));
        r.push_str(&format!(
            "\nvar {} = module.BuildColumnExpressions(AllColumns)\n",
            self.ce
        ));
        r
    }

    pub fn render(&mut self, cs: &ConstraintsSet) -> Result<()> {
        let columns = if self.render_columns {
            self.render_columns(&cs.columns)
        } else {
            String::new()
        };
        let constraints = cs
            .constraints
            .iter()
            .map(|c| match c {
                Constraint::Vanishes {
                    name,
                    domain: _,
                    expr,
                } => self
                    .render_node(expr)
                    .map(|mut r| {
                        if let Some(true) = r.chars().last().map(|c| c != ',') {
                            r.push(',');
                        }
                        r
                    })
                    .map(|r| {
                        make_go_function(
                            &name.to_case(Case::Snake),
                            "r = []column.Expression {",
                            &r,
                            "}",
                            "[]column.Expression",
                        )
                    }),
                x => Ok(format!("{:?}", x)),
            })
            .collect::<Result<Vec<_>>>()?
            .join("\n");

        let main_function = make_go_function(
            &self.fname.to_case(Case::Pascal),
            "",
            &cs.constraints
                .iter()
                .map(|c| {
                    match c {
                        Constraint::Vanishes { name, domain, .. } => {
                            match domain {
                                None => {
                                    format!(
                                        "r = append(r, constraint.NewGlobalConstraintList({}()...)...)",
                                        name.to_case(Case::Camel)
                                    )
                                }
                                Some(domain) => {
                                    format!(
                                        "r = append(r, constraint.NewLocalConstraintList([]int{{{}}}, {}()...)...)",
                                        domain.iter().map(|x| x.to_string()).collect::<Vec<_>>().join(", "),
                                        name.to_case(Case::Camel)
                                    )
                                }
                            }
                        },
                        Constraint::Plookup(parents, children)  => {
                            format!("// New Plookup\n// Parents:\n// {:?}\n// Children:\n// {:?}", parents, children)
                        }
                    }
                })
                .collect::<Vec<_>>()
                .join("\n"),
            "",
            "module.Constraints"
        );

        let r = format!(
            r#"
package {}

import (
    "github.com/ethereum/go-ethereum/zk-evm/zeroknowledge/witnessdata/column"
    "github.com/ethereum/go-ethereum/zk-evm/zeroknowledge/witnessdata/constraint"
    "github.com/ethereum/go-ethereum/zk-evm/zeroknowledge/witnessdata/module"
)

{}

{}
"#,
            self.package, constraints, main_function,
        );

        if let Some(ref filename) = self.columns_filename {
            if self.render_columns {
                std::fs::File::create(filename)
                    .with_context(|| format!("while creating `{}`", filename))?
                    .write_all(columns.as_bytes())
                    .with_context(|| format!("while writing to `{}`", filename))?;
                gofmt(filename);
            }
        } else {
            println!("{}", columns);
        };

        if let Some(filename) = self.constraints_filename.as_ref() {
            std::fs::File::create(filename)
                .with_context(|| format!("while creating `{}`", filename))?
                .write_all(r.as_bytes())
                .with_context(|| format!("while writing to `{}`", filename))?;
            gofmt(filename);
        } else {
            println!("{}", r);
        }
        Ok(())
    }
}
