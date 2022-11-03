use num_bigint::BigInt;
use pairing_ce::{bn256::Fr, ff::PrimeField};
use std::{collections::HashMap, io::Write};

use convert_case::{Case, Casing};
use eyre::*;

use crate::{column::ColumnSet, compiler::*};

const SIZE: usize = 4_194_304;

fn shift(e: &Expression, i: isize) -> Expression {
    match e {
        Expression::Funcall { func, args } => match func {
            Builtin::Shift => {
                let value = args[1].pure_eval() + i;
                Expression::Funcall {
                    func: Builtin::Shift,
                    args: vec![
                        args[0].clone(),
                        Expression::Const(value.clone(), Fr::from_str(&value.to_string())),
                    ],
                }
            }
            _ => Expression::Funcall {
                func: *func,
                args: args.iter().map(|a| shift(a, i)).collect(),
            },
        },
        Expression::Const(..) => e.clone(),
        Expression::Column(..) => Expression::Funcall {
            func: Builtin::Shift,
            args: vec![
                e.clone(),
                Expression::Const(BigInt::from(i), Fr::from_str(&i.to_string())),
            ],
        },
        Expression::List(xs) => Expression::List(xs.iter().map(|x| shift(x, i)).collect()),
        Expression::ArrayColumn(..) => unreachable!(),
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
        Expression::Const(x, _) => format!("symbolic.NewConstant(\"{}\")", x),
        Expression::Column(handle, _, _) => format!("{}.AsVariable()", handle.mangle()),
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
                Expression::Column(handle, ..) => handle.mangle(),
                _ => unreachable!(),
            };
            format!("({}).Shift({}).AsVariable()", leaf, args[1].pure_eval(),)
        }
        x => {
            unimplemented!("{:?}", x)
        }
    }
}

fn render_constraints(constraints: &[Constraint]) -> String {
    constraints
        .iter()
        .map(|constraint| match constraint {
            Constraint::Vanishes { name, domain, expr } => {
                render_constraint(name, domain.clone(), expr)
            }
            Constraint::Plookup(name, from, to) => format!(
                "build.Inclusion(\"{}\", []zkevm.Handle{{{}}}, []zkevm.Handle{{{}}})",
                name,
                from.iter()
                    .map(render_expression)
                    .collect::<Vec<_>>()
                    .join(", "),
                to.iter()
                    .map(render_expression)
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Constraint::Permutation(name, from, to) => format!(
                "build.Permutation(\"{}\", []zkevm.Handle{{{}}}, []zkevm.Handle{{{}}})",
                name.to_case(Case::Snake),
                from.iter()
                    .map(Handle::mangle)
                    .collect::<Vec<_>>()
                    .join(", "),
                to.iter().map(Handle::mangle).collect::<Vec<_>>().join(", ")
            ),
            Constraint::InRange(name, from, range) => format!(
                "build.Range(\"{}\", {}, {})",
                name.to_case(Case::Snake),
                render_expression(from),
                range
            ),
        })
        .collect::<Vec<String>>()
        .join("\n")
}

fn render_constants(consts: &HashMap<Handle, i64>) -> String {
    if consts.is_empty() {
        String::default()
    } else {
        format!(
            "const (\n{}\n)",
            consts
                .iter()
                .fold(String::new(), |mut ax, (handle, value)| {
                    ax.push_str(&format!("{} int = {}\n", handle.mangle(), value));
                    ax
                })
        )
    }
}

fn render_columns<T: Clone>(cols: &ColumnSet<T>) -> String {
    let mut r = String::new();
    for (module, m) in cols.cols.iter() {
        for (name, c) in m.iter() {
            let name = Handle::new(module, name).mangle();
            match c.kind {
                Kind::Atomic | Kind::Composite(_) | Kind::Phantom => {
                    r += &format!("{} := build.RegisterCommit(\"{}\", SIZE)\n", name, name)
                }
                _ => (),
            }
        }
    }
    for (module, m) in cols.cols.iter() {
        for (name, c) in m.iter() {
            let name = Handle::new(module, name).mangle();
            match c.kind {
                Kind::Interleaved(ref froms) => {
                    r += &format!(
                        "{} := zkevm.Interleave({})\n",
                        name,
                        froms
                            .iter()
                            .map(Handle::mangle)
                            .collect::<Vec<_>>()
                            .join(", ")
                    );
                }
                _ => (),
            }
        }
    }

    r
}

fn render_constraint(name: &str, domain: Option<Vec<isize>>, expr: &Expression) -> String {
    match expr {
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
}

impl WizardIOP {
    pub fn render(&mut self, cs: &ConstraintSet) -> Result<()> {
        let consts = render_constants(&cs.constants);
        let columns = render_columns(&cs.modules);
        let constraints = render_constraints(&cs.constraints);

        let r = format!(
            r#"
package {}


import (
    "github.com/consensys/accelerated-crypto-monorepo/example/zkevm"
    "github.com/consensys/accelerated-crypto-monorepo/protocol/commitment"
    "github.com/consensys/accelerated-crypto-monorepo/symbolic"
)

const SIZE = {}

{}

func Define(build *zkevm.Builder) {{
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
            &self.package, SIZE, consts, columns, constraints,
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
