use crate::parser::*;
use color_eyre::eyre::*;

use std::io::{BufWriter, Write};

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

impl crate::parser::Transpiler for GoExporter {
    fn render<'a>(
        &self,
        cs: &ConstraintsSet,
        mut out: BufWriter<Box<dyn Write + 'a>>,
    ) -> Result<()> {
        let body = cs
            .constraints
            .iter()
            .map(|c| self.render_node(c))
            .collect::<Result<Vec<_>>>()?
            .join(",\n")
            .replace(",,", ","); // Screw you, Go

        let r = format!(
            r#"
package {}

import (
"github.com/ethereum/go-ethereum/zk-evm/zeroknowledge/witnessdata/column"
)

func {}() (r []column.Expression) {{
r = []column.Expression {{
{}{}
}}
return
}}
"#,
            self.settings.package,
            self.settings.fname,
            body,
            if body.chars().last().unwrap() == ',' {
                ""
            } else {
                ","
            },
        );
        writeln!(out, "{}", r).with_context(|| eyre!("rendering result"))
    }
}
