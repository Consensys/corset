use crate::column::Computation;
use crate::compiler::codetyper::Tty;
use crate::compiler::{Constraint, ConstraintSet, Expression, Intrinsic, Node};
use crate::pretty::Pretty;
use crate::structs::Handle;
use anyhow::*;
use itertools::Itertools;
use num_traits::ToPrimitive;
use owo_colors::XtermColors;
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
        (Intrinsic::Sub, Intrinsic::Exp) => Ordering::Less,
        _ => unimplemented!("{a}/{b}"),
    }
}

fn pretty_expr(n: &Node, prev: Option<Intrinsic>, tty: &mut Tty, show_types: bool) {
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
                    pretty_expr(a, Some(*f), tty, show_types);
                    if args.peek().is_some() {
                        tty.write(format!(" {} ", f));
                    }
                }
                if prev.map(|p| priority(*f, p)).unwrap_or(Ordering::Equal) == Ordering::Less {
                    tty.write(")");
                }
            }
            Intrinsic::Exp => {
                pretty_expr(&args[0], Some(*f), tty, show_types);
                tty.write("^");
                pretty_expr(&args[1], Some(*f), tty, show_types);
            }
            Intrinsic::Shift => {
                pretty_expr(&args[0], None, tty, show_types);
                let subponent = args[1].pure_eval().unwrap().to_i64().unwrap();
                tty.write(if subponent > 0 { "₊" } else { "" });
                tty.write(crate::pretty::subscript(&subponent.to_string()));
            }
            Intrinsic::Neg => {
                tty.write("-");
                pretty_expr(&args[0], prev, tty, show_types);
            }
            Intrinsic::Inv => {
                tty.write("INV(");
                pretty_expr(&args[0], prev, tty, show_types);
                tty.write(")");
            }
            Intrinsic::Nth => unreachable!(),
            Intrinsic::Begin => todo!(),
            Intrinsic::IfZero => {
                tty.write("if-zero ".color(c).bold().to_string());
                pretty_expr(&args[0], Some(Intrinsic::Mul), tty, show_types);
                tty.shift(INDENT);
                tty.cr();
                pretty_expr(&args[1], None, tty, show_types);
                if let Some(a) = args.get(2) {
                    tty.unshift();
                    tty.cr();
                    tty.write("else".color(c).bold().to_string());
                    tty.shift(INDENT);
                    tty.cr();
                    pretty_expr(a, prev, tty, show_types);
                }
                tty.unshift();
                tty.cr();
                tty.write("endif".color(c).bold().to_string());
            }
            Intrinsic::IfNotZero => {
                tty.write("if-not-zero ".color(c).bold().to_string());
                pretty_expr(&args[0], Some(Intrinsic::Mul), tty, show_types);
                tty.shift(INDENT);
                tty.cr();
                pretty_expr(&args[1], None, tty, show_types);
                if let Some(a) = args.get(2) {
                    tty.unshift();
                    tty.cr();
                    tty.write("else".color(c).bold().to_string());
                    tty.shift(INDENT);
                    tty.cr();
                    pretty_expr(a, prev, tty, show_types);
                }
                tty.unshift();
                tty.cr();
                tty.write("endif".color(c).bold().to_string());
            }
        },
        Expression::Const(x, _) => tty.write(x.to_string()),
        Expression::Column { handle, .. } => {
            let color = handle.to_string().chars().fold(0, |ax, c| ax + c as usize) % 255 + 1;
            tty.write(
                handle
                    .to_string()
                    .color(XtermColors::from(color as u8))
                    .to_string(),
            )
        }
        Expression::List(xs) => {
            tty.write("{".color(c).to_string());
            tty.shift(INDENT);
            tty.cr();
            let mut xs = xs.iter().peekable();
            while let Some(x) = xs.next() {
                pretty_expr(x, None, tty, show_types);
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
    if show_types {
        tty.write(format!(":{}", n.t()));
    }
}

fn render_constraints(
    cs: &ConstraintSet,
    only: Option<&Vec<String>>,
    skip: &[String],
    show_types: bool,
) {
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
                    pretty_expr(expr, None, &mut tty, show_types);
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
                    pretty_expr(exp, None, &mut tty, false);
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
            "{:4}{:>80} {:>20}{}",
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

pub(crate) struct DebugSettings {
    pub constraints: bool,
    pub columns: bool,
    pub computations: bool,
    pub perspectives: bool,
    pub types: bool,
}

pub(crate) fn debug(
    cs: &ConstraintSet,
    settings: DebugSettings,
    only: Option<&Vec<String>>,
    skip: &[String],
) -> Result<()> {
    if settings.constraints {
        render_constraints(cs, only, skip, settings.types);
    }
    if settings.columns {
        render_columns(cs);
    }
    if settings.computations {
        render_computations(cs);
    }
    if settings.perspectives {
        render_perspectives(cs);
    }
    Ok(())
}
