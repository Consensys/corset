use crate::column::Computation;
use crate::compiler::codetyper::Tty;
use crate::compiler::{Constraint, ConstraintSet, Expression, Intrinsic, Node};
use crate::pretty::Pretty;
use crate::structs::Handle;
use anyhow::*;
use itertools::Itertools;
use num_traits::ToPrimitive;
use owo_colors::{colored::Color, OwoColorize};
use std::cmp::Ordering;

fn priority(a: Intrinsic, b: Intrinsic) -> Ordering {
    match (a, b) {
        (Intrinsic::Add, Intrinsic::Add) => Ordering::Equal,
        (Intrinsic::Add, Intrinsic::Sub) => Ordering::Less,
        (Intrinsic::Add, Intrinsic::Mul) => Ordering::Less,
        (Intrinsic::Sub, Intrinsic::Mul) => Ordering::Less,
        (Intrinsic::Sub, Intrinsic::Add) => Ordering::Equal,
        (Intrinsic::Mul, Intrinsic::Add) => Ordering::Greater,
        (Intrinsic::Mul, Intrinsic::Sub) => Ordering::Greater,
        (Intrinsic::Mul, Intrinsic::Mul) => Ordering::Equal,
        (Intrinsic::Sub, Intrinsic::Sub) => Ordering::Equal,
        _ => unimplemented!("{a}/{b}"),
    }
}

fn pretty_expr(cs: &ConstraintSet, n: &Node, prev: Option<Intrinsic>, tty: &mut Tty) {
    const INDENT: usize = 4;
    let colors = [
        Color::Red,
        Color::Green,
        Color::Yellow,
        Color::Magenta,
        Color::Cyan,
        Color::Blue,
        Color::BrightRed,
        Color::BrightGreen,
        Color::BrightYellow,
        Color::BrightMagenta,
        Color::BrightCyan,
        Color::BrightBlue,
    ];
    let c = colors[tty.depth() % colors.len()];
    match n.e() {
        Expression::Funcall { func: f, args } => match f {
            Intrinsic::Add | Intrinsic::Sub | Intrinsic::Mul => {
                if prev.map(|p| priority(*f, p)).unwrap_or(Ordering::Equal) == Ordering::Less {
                    tty.write("(");
                }
                let mut args = args.iter().peekable();
                while let Some(a) = args.next() {
                    pretty_expr(cs, a, Some(*f), tty);
                    if args.peek().is_some() {
                        tty.write(format!(" {} ", f));
                    }
                }
                if prev.map(|p| priority(*f, p)).unwrap_or(Ordering::Equal) == Ordering::Less {
                    tty.write(")");
                }
            }
            Intrinsic::Exp => {
                pretty_expr(cs, &args[0], Some(*f), tty);
                tty.write("^");
                pretty_expr(cs, &args[1], Some(*f), tty);
            }
            Intrinsic::Shift => {
                pretty_expr(cs, &args[0], None, tty);
                let subponent = args[1].pure_eval().unwrap().to_i64().unwrap();
                tty.write(if subponent > 0 { "₊" } else { "" }.to_string());
                tty.write(crate::pretty::subscript(&subponent.to_string()));
            }
            Intrinsic::Neg => {
                tty.write("-");
                pretty_expr(cs, &args[0], prev, tty);
            }
            Intrinsic::Inv => {
                tty.write("INV");
                pretty_expr(cs, &args[0], prev, tty);
            }
            Intrinsic::Nth => unreachable!(),
            Intrinsic::Begin => todo!(),
            Intrinsic::IfZero => {
                tty.write("ifzero ".color(c).to_string());
                pretty_expr(cs, &args[0], Some(Intrinsic::Mul), tty);
                tty.shift(INDENT);
                tty.cr();
                pretty_expr(cs, &args[1], None, tty);
                if let Some(a) = args.get(2) {
                    tty.unshift();
                    tty.cr();
                    tty.write("else".color(c).to_string());
                    tty.shift(INDENT);
                    tty.cr();
                    pretty_expr(cs, a, prev, tty);
                }
                tty.unshift();
                tty.cr();
                tty.write("endif".color(c).to_string());
            }
            Intrinsic::IfNotZero => {
                tty.write("ifnotzero ".color(c).to_string());
                pretty_expr(cs, &args[0], Some(Intrinsic::Mul), tty);
                tty.shift(INDENT);
                tty.cr();
                pretty_expr(cs, &args[1], None, tty);
                if let Some(a) = args.get(2) {
                    tty.unshift();
                    tty.cr();
                    tty.write("else".color(c).to_string());
                    tty.shift(INDENT);
                    tty.cr();
                    pretty_expr(cs, a, prev, tty);
                }
                tty.unshift();
                tty.cr();
                tty.write("endif".color(c).to_string());
            }
        },
        Expression::Const(x, _) => tty.write(x.to_string()),
        Expression::Column { handle, .. } => tty.write(handle.to_string()),
        Expression::List(xs) => {
            tty.write("{".color(c).to_string());
            tty.shift(INDENT);
            tty.cr();
            let mut xs = xs.iter().peekable();
            while let Some(x) = xs.next() {
                pretty_expr(cs, x, None, tty);
                if xs.peek().is_some() {
                    tty.cr();
                }
            }
            tty.unshift();
            tty.cr();
            tty.write("}".color(c).to_string());
        }
        Expression::ArrayColumn { .. } => unreachable!(),
        Expression::Void => unreachable!(),
    }
}

fn render_constraints(cs: &ConstraintSet, only: Option<&Vec<String>>, skip: &[String]) {
    println!("\n{}", "=== Constraints ===".bold().yellow());
    for c in cs.constraints.iter() {
        if !skip.contains(&c.name()) && only.map(|o| o.contains(&c.name())).unwrap_or(true) {
            match c {
                Constraint::Vanishes {
                    handle,
                    domain: _,
                    expr,
                } => {
                    let mut tty = Tty::new();
                    pretty_expr(cs, expr, None, &mut tty);
                    println!("\n{}", handle.pretty());
                    println!("{}", tty.page_feed());
                }
                Constraint::Plookup {
                    including,
                    included,
                    ..
                } => {
                    println!(
                        "{{{}}} ⊂ {{{}}}",
                        included
                            .iter()
                            .map(|n| n.pretty())
                            .collect::<Vec<_>>()
                            .join(", "),
                        including
                            .iter()
                            .map(|n| n.pretty())
                            .collect::<Vec<_>>()
                            .join(", "),
                    )
                }
                Constraint::Permutation { .. } => (),
                Constraint::InRange { handle, exp, max } => {
                    let mut tty = Tty::new();
                    pretty_expr(cs, exp, None, &mut tty);
                    println!("\n{}", handle.pretty());
                    println!("{} < {}", tty.page_feed(), max);
                }
            }
        }
    }
}

fn render_columns(cs: &ConstraintSet) {
    println!("\n{}", "=== Columns ===".bold().yellow());
    for (r, col) in cs.columns.iter().sorted_by_key(|c| c.1.register) {
        println!(
            "{}{:>70}   {:>20}{}",
            r.as_id(),
            format!(
                "{}{}",
                col.handle
                    .perspective
                    .as_ref()
                    .map(|p| format!(" ({})", p))
                    .unwrap_or_default(),
                &col.handle,
            ),
            format!("{} × {:?}", cs.length_multiplier(&r), col.t),
            col.register
                .map(|r| format!(
                    " ∈ {}/{}",
                    r,
                    cs.columns.registers[r]
                        .handle
                        .as_ref()
                        .map(|h| h.to_string())
                        .unwrap_or(format!("r{}", r))
                ))
                .unwrap_or_default()
        );
    }
}

fn render_computations(cs: &ConstraintSet) {
    println!("\n{}", "=== Computations ===".bold().yellow());
    for comp in cs.computations.iter() {
        match comp {
            Computation::Composite { target, exp } => {
                println!("{} = {}", target.pretty(), exp.pretty())
            }
            Computation::Interleaved { target, froms } => {
                println!(
                    "{} ⪡ {}",
                    cs.handle(target).pretty(),
                    froms.iter().map(|c| cs.handle(c).pretty()).join(", ")
                )
            }
            Computation::Sorted { froms, tos, signs } => println!(
                "[{}] ⇳ [{}]",
                tos.iter().map(|c| cs.handle(c).pretty()).join(" "),
                froms
                    .iter()
                    .zip(signs.iter())
                    .map(|(c, s)| format!(
                        "{} {}",
                        if *s { '↓' } else { '↑' },
                        cs.handle(c).pretty()
                    ))
                    .join(" "),
            ),
            Computation::CyclicFrom { target, froms, .. } => println!(
                "{} ↻ {}",
                froms.iter().map(|c| cs.handle(c).pretty()).join(", "),
                target
            ),
            Computation::SortingConstraints { sorted, .. } => println!(
                "Sorting constraints for {}",
                sorted.iter().map(|c| cs.handle(c).pretty()).join(", ")
            ),
        }
    }
}

fn render_perspectives(cs: &ConstraintSet) {
    println!("\n{}", "=== Perspectives ===".bold().yellow());
    for (module, persps) in cs.perspectives.iter() {
        for (name, expr) in persps.iter() {
            println!(
                "{}: {}",
                Handle::new(module, name).pretty(),
                expr.pretty_with_handle(cs)
            )
        }
    }
}

pub fn debug(
    cs: &ConstraintSet,
    show_constraints: bool,
    show_columns: bool,
    show_computations: bool,
    show_perspectives: bool,
    only: Option<&Vec<String>>,
    skip: &[String],
) -> Result<()> {
    if show_constraints {
        render_constraints(cs, only, skip);
    }
    if show_columns {
        render_columns(cs);
    }
    if show_computations {
        render_computations(cs);
    }
    if show_perspectives {
        render_perspectives(cs);
    }
    Ok(())
}
