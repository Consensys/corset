use crate::parser::*;
use color_eyre::eyre::*;

const INDENT: usize = 4;
#[derive(Debug)]
pub(crate) struct GoExporter {
    pub settings: crate::Args,
}
impl GoExporter {
    fn indented_block(&self, header: &str, content: &str, indent: usize) -> String {
        let I = "-".repeat(indent);
        format!(
            "{}{}{{\n{}{}\n{}}}",
            I,
            header,
            I,
            content.replace("\n", &format!("\n{}{}", I, I)),
            I
        )
    }

    fn make_chain(
        &self,
        xs: &[Constraint],
        operand: &str,
        surround: bool,
        indent: usize,
    ) -> Result<String> {
        let head = self.render_node(&xs[0], 0)?;
        let tail = &xs[1..];
        if xs.len() > 2 {
            let tail = tail
                .iter()
                .map(|x| {
                    self.render_node(x, 0)
                        .map(|s| format!("\n{}({})", operand, s))
                })
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
                self.render_node(&xs[1], 0)?
            ))
        }
    }
    pub fn render_node(&self, node: &Constraint, indent: usize) -> Result<String> {
        let r = match node {
            Constraint::Const(x) => match x {
                0..=2 | 127 | 256 => Ok(format!("column.CONST_{}()", x)),
                x @ _ => Ok(format!("column.CONST_UINT64({})", x)),
            },
            Constraint::Column(name) => Ok(format!("CE[{}.Name()]", name)),
            Constraint::Funcall { func, args } => self.render_funcall(func, args, indent),
            Constraint::List(constraints) => Ok(self.indented_block(
                "[]column.Expression",
                &constraints
                    .iter()
                    .map(|x| self.render_node(x, indent))
                    .map(|x| {
                        x.map(|mut r| {
                            r.push_str(",");
                            r
                        })
                    })
                    .collect::<Result<Vec<_>>>()?
                    .join("\n"),
                indent,
            )),
        }?;
        Ok(r)
    }
    pub fn render_funcall(
        &self,
        func: &Builtin,
        args: &[Constraint],
        indent: usize,
    ) -> Result<String> {
        let r = match func {
            Builtin::Add => self.make_chain(args, "Add", true, 0),
            Builtin::Mul => self.make_chain(args, "Mul", false, 0),
            Builtin::Sub => self.make_chain(args, "Sub", true, 0),
            Builtin::IfZero => {
                if args.len() != 2 {
                    Err(eyre!(
                        "IfZero should have two arguments; received {}: {}",
                        args.len(),
                        args.iter()
                            .map(|x| format!("{:?}", x))
                            .collect::<Vec<_>>()
                            .join("\n")
                    ))
                } else {
                    Ok(format!(
                        "({}).IfZero({})",
                        self.render_node(&args[0], 0)?,
                        self.render_node(&args[1], 0)?
                    ))
                }
            }
            Builtin::IfZeroElse => {
                if args.len() != 3 {
                    Err(eyre!(
                        "IfZeroElse should have three arguments; received {}: {}",
                        args.len(),
                        args.iter()
                            .map(|x| format!("{:?}", x))
                            .collect::<Vec<_>>()
                            .join("\n")
                    ))
                } else {
                    Ok(format!(
                        "({}).IfZeroElse(\n{},\n{},)",
                        self.render_node(&args[0], indent)?,
                        self.render_node(&args[1], indent + INDENT)?,
                        self.render_node(&args[2], indent + INDENT)?,
                    ))
                }
            }
            x @ _ => {
                unimplemented!("Unimplemented: {:?}", x)
            }
        }?;
        Ok(format!("{}{}", ".".repeat(indent), r))
    }
}

impl crate::parser::Transpiler for GoExporter {
    fn render(&self, cs: &ConstraintsSet) -> Result<String> {
        let prelude = format!(
            "package {}

import (
    \"github.com/ethereum/go-ethereum/zk-evm/zeroknowledge/witnessdata/column\"
)
",
            self.settings.package
        );

        let body = cs
            .constraints
            .iter()
            .map(|c| self.render_node(c, 2 * INDENT))
            .collect::<Result<Vec<_>>>()?
            .join("\n");

        let r = self.indented_block(
            &format!("func {}() []column.Expression ", &self.settings.name),
            &body,
            0,
        );
        Ok(format!("{}\n{}", prelude, r))
    }
}
