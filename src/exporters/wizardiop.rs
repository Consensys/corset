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
            Expression::Column(..) => Intrinsic::Shift
                .call(&[
                    e.clone(),
                    Expression::Const(BigInt::from(i), Fr::from_str(&i.to_string())).into(),
                ])
                .unwrap(),
            Expression::List(xs) => {
                Expression::List(xs.iter().map(|x| shift(x, i)).collect()).into()
            }
            Expression::ArrayColumn(..) => unreachable!(),
            Expression::Void => Expression::Void.into(),
        }
    }
}

fn make_chain(xs: &[Node], operand: &str, surround: bool) -> String {
    let head = render_expression(&xs[0]);
    if xs.len() > 1 {
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
    } else {
        head
    }
}

/// Render an expression, panicking if it is not a handle
fn render_handle(e: &Node) -> String {
    match e.e() {
        Expression::Column(handle, ..) => handle.mangle(),
        _ => unreachable!(),
    }
}

fn render_expression(e: &Node) -> String {
    match e.e() {
        Expression::ArrayColumn(..) => unreachable!(),
        Expression::Const(x, _) => format!("symbolic.NewConstant(\"{}\")", x),
        Expression::Column(handle, ..) => format!("{}.AsVariable()", handle.mangle()),
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
        Expression::Void => {
            warn!("Rendering VOID expression");
            "symbolic.NewConstant(\"0\")".into()
        }
    }
}

fn render_funcall(func: &Intrinsic, args: &[Node]) -> String {
    match func {
        Intrinsic::Add => make_chain(args, "Add", true),
        Intrinsic::Mul => make_chain(args, "Mul", false),
        Intrinsic::Sub => make_chain(args, "Sub", true),
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
                1 => render_expression(&args[0]),
                _ => make_chain(
                    &std::iter::repeat(args[0].clone())
                        .take(exp)
                        .collect::<Vec<_>>(),
                    "Mul",
                    false,
                ),
            }
        }
        Intrinsic::Neg => format!("({}).Neg()", render_expression(&args[0])),
        Intrinsic::Shift => {
            let leaf = match &args[0].e() {
                Expression::Column(handle, ..) => handle.mangle(),
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

fn render_constraints(constraints: &[Constraint]) -> String {
    constraints
        .iter()
        .sorted_by_key(|c| c.name())
        .map(|constraint| match constraint {
            Constraint::Vanishes {
                handle,
                domain,
                expr,
            } => render_constraint(&handle.to_string(), domain.clone(), expr),
            Constraint::Plookup {
                handle,
                including,
                included,
            } => format!(
                "build.Inclusion(\"{}\", []zkevm.Handle{{{}}}, []zkevm.Handle{{{}}})",
                handle,
                including
                    .iter()
                    .map(render_handle)
                    .collect::<Vec<_>>()
                    .join(", "),
                included
                    .iter()
                    .map(render_handle)
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
                    .map(Handle::mangle)
                    .collect::<Vec<_>>()
                    .join(", "),
                to.iter().map(Handle::mangle).collect::<Vec<_>>().join(", ")
            ),
            Constraint::InRange { handle, exp, max } => format!(
                "build.Range(\"{}\", {}, {})",
                handle.mangle().to_case(Case::Snake),
                render_handle(exp),
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

fn render_columns(cs: &ConstraintSet, sizes: &mut HashSet<String>) -> String {
    let mut r = String::new();
    for (handle, column) in cs
        .modules
        .iter_cols()
        // Interleaved columns should appear after their sources
        .sorted_by_cached_key(|(h, c)| {
            (
                if !matches!(c.kind, Kind::Interleaved(..)) {
                    0
                } else {
                    1
                },
                h.mangle(),
            )
        })
    {
        match column.kind {
            Kind::Atomic | Kind::Composite(_) | Kind::Phantom => {
                if column.used {
                    let size_multiplier = cs.length_multiplier(&handle);
                    r += &format!(
                        "{} := build.RegisterCommit(\"{}\", {})\n",
                        handle.mangle(),
                        handle,
                        if size_multiplier == 1 {
                            make_size(&handle, sizes)
                        } else {
                            format!("{}*{}", size_multiplier, make_size(&handle, sizes))
                        }
                    )
                }
            }
            Kind::Interleaved(_, ref froms) => {
                r += &format!(
                    "{} := zkevm.Interleave({})\n",
                    handle.mangle(),
                    froms
                        .as_ref()
                        .unwrap()
                        .iter()
                        .map(Handle::mangle)
                        .collect::<Vec<_>>()
                        .join(", ")
                );
            }
        }
    }

    r
}

fn render_constraint(name: &str, domain: Option<Vec<isize>>, expr: &Node) -> String {
    match expr.e() {
        Expression::List(xs) => xs
            .iter()
            .enumerate()
            .map(|(i, x)| render_constraint(&format!("{}_{}", name, i), domain.clone(), x))
            .collect::<Vec<_>>()
            .join("\n"),
        _ => match domain {
            None => format!(
                "build.GlobalConstraint(\"{}\", {})",
                name,
                render_expression(expr)
            ),
            Some(domain) => domain
                .iter()
                .map(|x| {
                    format!(
                        "build.LocalConstraint(\"{}\", {})",
                        name,
                        render_expression(&shift(expr, *x))
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
        let constraints = render_constraints(&cs.constraints);

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
            &self.package,
            // SIZE,
            // self.sizes
            //     .iter()
            //     .sorted()
            //     .fold(String::new(), |ax, s| ax + &format!("{} = SIZE\n", s)),
            columns,
            constraints,
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
