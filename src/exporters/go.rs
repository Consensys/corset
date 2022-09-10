use color_eyre::eyre::*;
use convert_case::{Case, Casing};

use crate::compiler::*;

use std::io::{BufWriter, Write};
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

#[derive(Debug)]
pub(crate) struct GoExporter {
    pub fname: String,
    pub package: String,
    pub ce: String,
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
            Expression::Const(x) => match x {
                0..=2 | 127 | 256 => Ok(format!("column.CONST_{}()", x)),
                x => Ok(format!("column.CONST_UINT64({})", x)),
            },
            Expression::Column(name, _) => Ok(format!("{}[\"{}\"]", self.ce, name)),
            Expression::ArrayColumnElement(name, i, _) => Ok(format!(
                "{}[{}{}{}.Name()]",
                self.ce, name, ARRAY_SEPARATOR, i
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
}

impl crate::exporters::Exporter<Constraint> for GoExporter {
    fn render<'a>(
        &mut self,
        cs: &[Constraint],
        mut out: BufWriter<Box<dyn Write + 'a>>,
    ) -> Result<()> {
        if cs.is_empty() {
            return Ok(());
        }

        let constraints = cs
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
            &cs.iter()
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
            r#"package {}

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
        writeln!(out, "{}", r).with_context(|| eyre!("rendering to Go"))
    }
}
