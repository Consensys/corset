use itertools::Itertools;
use log::*;
use num_bigint::BigInt;
use num_traits::ToPrimitive;
use pairing_ce::{bn256::Fr, ff::PrimeField};
use std::{collections::HashSet, io::Write};

use anyhow::*;
use convert_case::{Case, Casing};

use crate::{compiler::*, pretty::Pretty, structs::Handle};

// const SIZE: usize = 4_194_304;

fn shift(e: &Node, i: isize) -> Node {
    if i == 0 {
        e.to_owned()
    } else {
        match e.e() {
            Expression::Funcall { func, args } => match func {
                Intrinsic::Shift => {
                    let value = args[1].pure_eval().unwrap() + i;
                    Intrinsic::Shift
                        .call(&[
                            args[0].clone(),
                            Expression::Const(value.clone(), Fr::from_str(&value.to_string()))
                                .into(),
                        ])
                        .unwrap()
                }
                _ => Expression::Funcall {
                    func: *func,
                    args: args.iter().map(|a| shift(a, i)).collect(),
                }
                .into(),
            },
            Expression::Const(..) => e.clone(),
            Expression::Column { .. } => Intrinsic::Shift
                .call(&[
                    e.clone(),
                    Expression::Const(BigInt::from(i), Fr::from_str(&i.to_string())).into(),
                ])
                .unwrap(),
            Expression::List(xs) => {
                Expression::List(xs.iter().map(|x| shift(x, i)).collect()).into()
            }
            Expression::ArrayColumn { .. } => unreachable!(),
            Expression::Void => Expression::Void.into(),
        }
    }
}

fn make_chain(cs: &ConstraintSet, xs: &[Node], operand: &str, surround: bool) -> String {
    let head = render_expression(cs, &xs[0]);
    if xs.len() > 1 {
        let tail = &xs[1..];
        if xs.len() > 2 {
            let tail = tail
                .iter()
                .map(|x| format!("{}({})", operand, render_expression(cs, x)))
                .collect::<Vec<_>>()
                .join(".");
            let chain = format!("{}.{}", head, tail);
            if surround {
                format!("({})", chain)
            } else {
                chain
            }
        } else {
            format!("{}.{}({})", head, operand, render_expression(cs, &xs[1]))
        }
    } else {
        head
    }
}

/// Render an expression, panicking if it is not a handle
fn render_handle(cs: &ConstraintSet, e: &Node) -> String {
    match e.e() {
        Expression::Column { handle, .. } => reg_mangle(cs, handle).unwrap(),
        _ => unreachable!(),
    }
}

fn render_expression(cs: &ConstraintSet, e: &Node) -> String {
    match e.e() {
        Expression::ArrayColumn { .. } => unreachable!(),
        Expression::Const(x, _) => format!("symbolic.NewConstant(\"{}\")", x),
        Expression::Column { handle, .. } => {
            format!("{}.AsVariable()", reg_mangle(cs, handle).unwrap())
        }
        Expression::Funcall { func, args } => render_funcall(cs, func, args),
        Expression::List(constraints) => constraints
            .iter()
            .map(|e| render_expression(cs, e))
            .map(|mut x| {
                if let Some(true) = x.chars().last().map(|c| c != ',') {
                    x.push(',');
                }
                x
            })
            .collect::<Vec<_>>()
            .join("\n"),
        Expression::Void => {
            warn!("Rendering VOID expression");
            "symbolic.NewConstant(\"0\")".into()
        }
    }
}

fn render_funcall(cs: &ConstraintSet, func: &Intrinsic, args: &[Node]) -> String {
    match func {
        Intrinsic::Add => make_chain(cs, args, "Add", true),
        Intrinsic::Mul => make_chain(cs, args, "Mul", false),
        Intrinsic::Sub => make_chain(cs, args, "Sub", true),
        Intrinsic::Exp => {
            let exp = args[1]
                .pure_eval()
                .unwrap_or_else(|_| panic!("Exponent `{}` is not evaluable", &args[1]))
                .to_usize()
                .unwrap_or_else(|| {
                    panic!("Exponent `{}` is too large", &args[1].pure_eval().unwrap())
                });
            match exp {
                0 => "column.CONST_STRING(\"1\")".to_string(),
                1 => render_expression(cs, &args[0]),
                _ => make_chain(
                    cs,
                    &std::iter::repeat(args[0].clone())
                        .take(exp)
                        .collect::<Vec<_>>(),
                    "Mul",
                    false,
                ),
            }
        }
        Intrinsic::Neg => format!("({}).Neg()", render_expression(cs, &args[0])),
        Intrinsic::Shift => {
            let leaf = match &args[0].e() {
                Expression::Column { handle, .. } => reg_mangle(cs, handle).unwrap(),
                _ => unreachable!(),
            };
            format!(
                "({}).Shift({}).AsVariable()",
                leaf,
                args[1].pure_eval().unwrap(),
            )
        }
        x => {
            unimplemented!("{:?}", x)
        }
    }
}

fn render_constraints(cs: &ConstraintSet) -> String {
    cs.constraints
        .iter()
        .sorted_by_key(|c| c.name())
        .map(|constraint| match constraint {
            Constraint::Vanishes {
                handle,
                domain,
                expr,
            } => render_constraint(cs, &handle.to_string(), domain.clone(), expr),
            Constraint::Plookup {
                handle,
                including,
                included,
            } => format!(
                "build.Inclusion(\"{}\", []zkevm.Handle{{{}}}, []zkevm.Handle{{{}}})",
                handle,
                including
                    .iter()
                    .map(|h| render_handle(cs, h))
                    .collect::<Vec<_>>()
                    .join(", "),
                included
                    .iter()
                    .map(|h| render_handle(cs, h))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Constraint::Permutation {
                handle,
                from,
                to,
                signs: _,
            } => format!(
                "build.Permutation(\"{}\", []zkevm.Handle{{{}}}, []zkevm.Handle{{{}}})",
                handle.mangle().to_case(Case::Snake),
                from.iter()
                    .map(|c| reg_mangle(cs, c).unwrap())
                    .collect::<Vec<_>>()
                    .join(", "),
                to.iter()
                    .map(|h| reg_mangle(cs, h).unwrap())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Constraint::InRange { handle, exp, max } => format!(
                "build.Range(\"{}\", {}, {})",
                handle.mangle().to_case(Case::Snake),
                render_handle(cs, exp),
                max.pretty()
            ),
        })
        .collect::<Vec<String>>()
        .join("\n")
}

fn make_size(h: &Handle, sizes: &mut HashSet<String>) -> String {
    let r = format!("SIZE_{}", h.mangled_module());
    sizes.insert(r.clone());
    r
}

fn reg_mangle(cs: &ConstraintSet, c: &ColumnRef) -> Result<String> {
    let reg_id = cs
        .columns
        .get_col(c)?
        .register
        .ok_or_else(|| anyhow!("column {} has no backing register", c.pretty()))?;
    let reg = &cs
        .columns
        .registers
        .get(reg_id)
        .ok_or_else(|| anyhow!("register {} for column {} does not exist", reg_id, c))?;
    Ok(reg
        .handle
        .as_ref()
        .map(|h| h.mangle())
        .unwrap_or_else(|| Handle::new("", reg_id.to_string()).mangle()))
}

fn reg(cs: &ConstraintSet, c: &Handle) -> Result<Handle> {
    let reg_id = cs
        .columns
        .by_handle(c)?
        .register
        .ok_or_else(|| anyhow!("column {} has no backing register", c.pretty()))?;
    let reg = &cs
        .columns
        .registers
        .get(reg_id)
        .ok_or_else(|| anyhow!("register {} for column {} does not exist", reg_id, c))?;
    Ok(reg
        .handle
        .as_ref()
        .cloned()
        .unwrap_or_else(|| Handle::new(&c.module, reg_id.to_string())))
}

fn render_columns(cs: &ConstraintSet, sizes: &mut HashSet<String>) -> String {
    let mut r = String::new();
    for (h, column) in cs
        .columns
        .iter()
        // Interleaved columns should appear after their sources
        .sorted_by_cached_key(|c| {
            (
                if todo!() {
                    // if !matches!(c.1.kind, Kind::Interleaved { .. }) {
                    0
                } else {
                    1
                },
                c.1.handle.mangle(),
            )
        })
    {
        match &column.kind {
            Kind::Atomic | Kind::Composite(_) | Kind::Phantom => {
                if column.used {
                    let size_multiplier = cs.length_multiplier(&h);
                    r += &format!(
                        "{} := build.RegisterCommit(\"{}\", {})\n",
                        reg_mangle(cs, &h).unwrap(),
                        reg(cs, &column.handle).unwrap(),
                        if size_multiplier == 1 {
                            make_size(&column.handle, sizes)
                        } else {
                            format!("{} * {}", size_multiplier, make_size(&column.handle, sizes))
                        }
                    )
                }
            } // Kind::Interleaved { froms: sources } => {
              //     r += &format!(
              //         "{} := zkevm.Interleave({})\n",
              //         reg_mangle(cs, &h).unwrap(),
              //         sources
              //             .iter()
              //             .map(|c| reg_mangle(cs, c).unwrap())
              //             .collect::<Vec<_>>()
              //             .join(", ")
              //     );
              // }
        }
        todo!("cf just above")
    }

    r
}

fn render_constraint(
    cs: &ConstraintSet,
    name: &str,
    domain: Option<Vec<isize>>,
    expr: &Node,
) -> String {
    match expr.e() {
        Expression::List(xs) => xs
            .iter()
            .enumerate()
            .map(|(i, x)| render_constraint(cs, &format!("{}_{}", name, i), domain.clone(), x))
            .collect::<Vec<_>>()
            .join("\n"),
        _ => match domain {
            None => format!(
                "build.GlobalConstraint(\"{}\", {})",
                name,
                render_expression(cs, expr)
            ),
            Some(domain) => domain
                .iter()
                .map(|x| {
                    format!(
                        "build.LocalConstraint(\"{}\", {})",
                        name,
                        render_expression(cs, &shift(expr, *x))
                    )
                })
                .collect::<Vec<_>>()
                .join("\n"),
        },
    }
}

pub struct WizardIOP {
    pub out_filename: Option<String>,
    pub package: String,

    pub sizes: HashSet<String>,
}

impl WizardIOP {
    pub fn render(&mut self, cs: &ConstraintSet) -> Result<()> {
        let columns = render_columns(cs, &mut self.sizes);
        let constraints = render_constraints(cs);

        let r = format!(
            r#"
package {}


import (
    "github.com/consensys/accelerated-crypto-monorepo/symbolic"
    "github.com/consensys/accelerated-crypto-monorepo/zkevm"
)

// const (
// SIZE = []
// []
// )

func ZkEVMDefine(build *zkevm.Builder) {{
//
// Columns declarations
//
{}


//
// Constraints declarations
//
{}
}}
"#,
            &self.package, columns, constraints,
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
