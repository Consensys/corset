use crate::utils::*;
use color_eyre::eyre::*;
use convert_case::{Case, Casing};

use std::io::{BufWriter, Write};

fn make_go_function(name: &str, prelude: &str, content: &str, postlude: &str) -> String {
    format!(
        r#"func {}() (r []column.Expression) {{
{}
{}
{}
return
}}
"#,
        name.to_case(Case::Camel),
        prelude,
        content,
        postlude,
    )
}

#[derive(Debug)]
pub(crate) struct GoExporter {
    pub settings: crate::Args,
}
impl GoExporter {
    fn make_chain(&self, xs: &[Constraint], operand: &str, surround: bool) -> Result<String> {
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

    pub fn render_node(&self, node: &Constraint) -> Result<String> {
        let r = match node {
            Constraint::TopLevel { .. } => unreachable!(),
            Constraint::Const(x) => match x {
                0..=2 | 127 | 256 => Ok(format!("column.CONST_{}()", x)),
                x => Ok(format!("column.CONST_UINT64({})", x)),
            },
            Constraint::Column(name) => Ok(format!("CE[{}.Name()]", name)),
            Constraint::Funcall { func, args } => self.render_funcall(func, args),
            Constraint::List(constraints) => Ok(constraints
                .iter()
                .map(|x| self.render_node(x))
                .map(|x| {
                    x.map(|mut r| {
                        r.push(',');
                        r
                    })
                })
                .collect::<Result<Vec<_>>>()?
                .join("\n")),
        }?;

        Ok(r)
    }
    pub fn render_funcall(&self, func: &Builtin, args: &[Constraint]) -> Result<String> {
        let r = match func {
            Builtin::Add => self.make_chain(args, "Add", true),
            Builtin::Mul => self.make_chain(args, "Mul", false),
            Builtin::Sub => self.make_chain(args, "Sub", true),
            Builtin::IfZero => Ok(format!(
                "({}).IfZeroThen({})",
                self.render_node(&args[0])?,
                self.render_node(&args[1])?
            )),
            Builtin::Shift => Ok(format!(
                "({}).Shift({})",
                self.render_node(&args[0])?,
                if let Constraint::Const(x) = &args[1] {
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
}

impl crate::transpilers::Transpiler for GoExporter {
    fn render<'a>(
        &self,
        cs: &ConstraintsSet,
        mut out: BufWriter<Box<dyn Write + 'a>>,
    ) -> Result<()> {
        if cs.constraints.is_empty() {
            return Ok(());
        }

        let constraints = cs
            .constraints
            .iter()
            .map(|c| {
                if let Constraint::TopLevel { name, expr } = c {
                    self.render_node(expr)
                        .map(|mut r| {
                            if r.chars().last().unwrap() != ',' {
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
                            )
                        })
                } else {
                    unreachable!()
                }
            })
            .collect::<Result<Vec<_>>>()?
            .join("\n");

        let main_function = make_go_function(
            &self.settings.fname.to_case(Case::Pascal),
            "",
            &cs.constraints
                .iter()
                .map(|c| {
                    if let Constraint::TopLevel { name, .. } = c {
                        format!("r = append(r, {}()...)", name.to_case(Case::Camel))
                    } else {
                        unreachable!()
                    }
                })
                .collect::<Vec<_>>()
                .join("\n"),
            "",
        );

        let r = format!(
            r#"package {}

import "github.com/ethereum/go-ethereum/zk-evm/zeroknowledge/witnessdata/column"

{}

{}
"#,
            self.settings.package, constraints, main_function,
        );
        writeln!(out, "{}", r).with_context(|| eyre!("rendering result"))
    }
}
