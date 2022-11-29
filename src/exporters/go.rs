use std::{collections::HashMap, io::Write};

use anyhow::*;
use convert_case::{Case, Casing};
use itertools::Itertools;
use num_traits::ToPrimitive;

use crate::compiler::*;

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
pub struct GoExporter {
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
            Expression::Const(x, _) => Ok(format!("column.CONST_STRING(\"{}\")", x)),
            Expression::Column(handle, _, _) => {
                Ok(format!("{}[{}.Name()]", self.ce, handle.mangled_name()))
            }
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
            Expression::Void => return Ok(String::new()),
        }?;

        Ok(r)
    }
    pub fn render_funcall(&self, func: &Builtin, args: &[Expression]) -> Result<String> {
        let r = match func {
            Builtin::Add => self.make_chain(args, "Add", true),
            Builtin::Mul => self.make_chain(args, "Mul", false),
            Builtin::Sub => self.make_chain(args, "Sub", true),
            Builtin::Exp => {
                let exp = args[1]
                    .pure_eval()
                    .with_context(|| anyhow!("Exponent `{}` is not evaluable", &args[1]))?
                    .to_usize()
                    .with_context(|| {
                        anyhow!("Exponent `{}` is too large", &args[1].pure_eval().unwrap())
                    })?;
                match exp {
                    0 => Ok("column.CONST_STRING(\"1\")".to_string()),
                    1 => self.render_node(&args[0]),
                    _ => self.make_chain(
                        &std::iter::repeat(args[0].clone())
                            .take(exp)
                            .collect::<Vec<_>>(),
                        "Mul",
                        false,
                    ),
                }
            }
            Builtin::Inv => Ok(format!("({}).Inv()", self.render_node(&args[0])?)),
            Builtin::Neg => Ok(format!("({}).Neg()", self.render_node(&args[0])?)),
            Builtin::Shift => Ok(format!(
                "({}).Shift({})",
                self.render_node(&args[0])?,
                &args[1].pure_eval()?,
            )),
            x => {
                unimplemented!("{:?}", x)
            }
        }?;
        Ok(r)
    }

    fn render_consts(&self, consts: &HashMap<Handle, i64>) -> String {
        consts
            .iter()
            .sorted_by_key(|(handle, _)| handle.to_string())
            .fold(String::new(), |mut ax, (handle, value)| {
                ax.push_str(&format!(
                    "const {} = {}\n",
                    handle.mangled_name().to_case(Case::ScreamingSnake),
                    value
                ));
                ax
            })
    }

    fn render_columns(&self, cs: &ConstraintSet) -> String {
        let mut r = format!(
            r#"
package {}

import (
    "github.com/ethereum/go-ethereum/zk-evm/zeroknowledge/witnessdata/column"
    "github.com/ethereum/go-ethereum/zk-evm/zeroknowledge/witnessdata/module"
)
"#,
            self.package,
        );

        r += "const (\n";

        r += &cs
            .modules
            .iter()
            .filter_map(|(handle, col)| match col.kind {
                Kind::Atomic => Some(format!(
                    "{} column.Column = \"{}\"",
                    handle.mangled_name(),
                    handle.mangled_name()
                )),
                Kind::Phantom => None,
                Kind::Composite(_) => None,
                Kind::Interleaved(_) => Some(format!(
                    "{} column.Column = \"{}\"",
                    handle.mangled_name(),
                    handle.mangled_name()
                )),
            })
            .sorted()
            .collect::<Vec<_>>()
            .join("\n");
        r += ")\n\n";

        for comp in cs.computations.iter() {
            match &comp {
                crate::column::Computation::Composite { .. } => (),
                crate::column::Computation::Interleaved { target, froms } => r.push_str(&format!(
                    "var {} = column.Interleaved{{\n{}\n}}\n",
                    target.name,
                    froms
                        .iter()
                        .map(|f| format!("{},", f.name))
                        .collect::<Vec<_>>()
                        .join("\n")
                )),
                crate::column::Computation::Sorted { froms, tos } => {
                    for (from, to) in froms.iter().zip(tos.iter()) {
                        r.push_str(&format!(
                            "var {}  = column.NewSorted({})\n",
                            to.mangled_name(),
                            from.mangled_name()
                        ))
                    }
                }
            }
        }

        r += "var AllColumns = []column.Description{";
        r.push_str(&format!(
            "\n{}\n",
            cs.modules
                .iter()
                .filter_map(|(handle, col)| match col.kind {
                    Kind::Atomic => Some(format!("{},", handle.mangled_name())),
                    Kind::Phantom => None,
                    Kind::Composite(_) => None,
                    Kind::Interleaved(_) => Some(format!("{},", handle.mangled_name())),
                })
                .sorted()
                .collect::<Vec<_>>()
                .join("\n")
        ));
        for comp in cs.computations.iter() {
            match &comp {
                crate::column::Computation::Composite { .. } => (),
                crate::column::Computation::Interleaved { target, .. } => {
                    r.push_str(&format!("{},\n", target.name))
                }
                crate::column::Computation::Sorted { tos, .. } => {
                    for to in tos.iter() {
                        r.push_str(&format!("{},\n", to.mangled_name()))
                    }
                }
            }
        }
        r += "}\n\n";

        r.push_str(&format!(
            "var InterleavedColumns = []column.Description{{\n{}\n}}\n",
            cs.modules
                .iter()
                .filter_map(|(handle, col)| match col.kind {
                    Kind::Atomic => None,
                    Kind::Phantom => None,
                    Kind::Composite(_) => None,
                    Kind::Interleaved(_) => Some(format!("{},", handle.mangled_name())),
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

    pub fn render(&mut self, cs: &ConstraintSet) -> Result<()> {
        let columns = if self.render_columns {
            self.render_columns(cs)
        } else {
            String::new()
        };
        let constraints = cs
            .constraints
            .iter()
            .sorted_by_key(|c| c.name())
            .filter_map(|c| match c {
                Constraint::Vanishes {
                    name,
                    domain: _,
                    expr,
                } => Some(
                    self.render_node(expr)
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
                ),
                Constraint::Plookup(..) => None,
                Constraint::Permutation(..) => None,
                Constraint::InRange(..) => None,
            })
            .collect::<Result<Vec<_>>>()?
            .join("\n");

        let main_function = make_go_function(
            &self.fname.to_case(Case::Pascal),
            "",
            &cs.constraints
                .iter()
                .sorted_by_key(|c| c.name())
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
                        Constraint::Plookup(name, parents, children)  => {
                            format!("// New Plookup {}\n// Parents:\n// {:?}\n// Children:\n// {:?}", name, parents, children)
                        },
                        Constraint::Permutation(name, from, to) =>
                            format!("// Permutation {}\n// Parents:\n// {:?}\n// Children:\n// {:?}", name, from, to),
                        Constraint::InRange(.. ) => "".into()
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

{}
"#,
            self.package,
            self.render_consts(&cs.constants),
            constraints,
            main_function,
        );

        if let Some(ref filename) = self.columns_filename {
            if self.render_columns {
                std::fs::File::create(filename)
                    .with_context(|| format!("while creating `{}`", filename))?
                    .write_all(columns.as_bytes())
                    .with_context(|| format!("while writing to `{}`", filename))?;
                super::gofmt(filename);
            }
        } else {
            println!("{}", columns);
        };

        if let Some(filename) = self.constraints_filename.as_ref() {
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
