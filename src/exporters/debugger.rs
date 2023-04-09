use crate::compiler::codetyper::Tty;
use crate::compiler::{Constraint, ConstraintSet, Expression, Intrinsic, Node};
use crate::pretty::Pretty;
use anyhow::*;
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

fn pretty_expr(n: &Node, prev: Option<Intrinsic>, tty: &mut Tty) {
    const INDENT: usize = 4;
    match n.e() {
        Expression::Funcall { func: f, args } => match f {
            Intrinsic::Add | Intrinsic::Sub | Intrinsic::Mul => {
                if prev.map(|p| priority(*f, p)).unwrap_or(Ordering::Equal) == Ordering::Less {
                    tty.write("(");
                }
                let mut args = args.iter().peekable();
                while let Some(a) = args.next() {
                    pretty_expr(a, Some(*f), tty);
                    if args.peek().is_some() {
                        tty.write(format!(" {} ", f));
                    }
                }
                if prev.map(|p| priority(*f, p)).unwrap_or(Ordering::Equal) == Ordering::Less {
                    tty.write(")");
                }
            }
            Intrinsic::Exp => {
                pretty_expr(&args[0], Some(*f), tty);
                tty.write("^");
                pretty_expr(&args[1], Some(*f), tty);
            }
            Intrinsic::Shift => {
                pretty_expr(&args[0], None, tty);
                tty.write("[");
                pretty_expr(&args[1], None, tty);
                tty.write("]");
            }
            Intrinsic::Neg => {
                tty.write("-");
                pretty_expr(&args[0], prev, tty);
            }
            Intrinsic::Inv => {
                tty.write("INV");
                pretty_expr(&args[0], prev, tty);
            }
            Intrinsic::Not => unreachable!(),
            Intrinsic::Nth => unreachable!(),
            Intrinsic::Eq => {
                pretty_expr(&args[0], None, tty);
                tty.write(" == ");
                pretty_expr(&args[1], None, tty);
            }
            Intrinsic::Begin => todo!(),
            Intrinsic::IfZero => {
                tty.write("ifzero ");
                pretty_expr(&args[0], Some(Intrinsic::Mul), tty);
                tty.shift(INDENT);
                tty.cr();
                pretty_expr(&args[1], None, tty);
                if let Some(a) = args.get(2) {
                    tty.unshift();
                    tty.cr();
                    tty.write("else");
                    tty.shift(INDENT);
                    tty.cr();
                    pretty_expr(a, prev, tty);
                }
                tty.unshift();
                tty.cr();
                tty.write("endif");
            }
            Intrinsic::IfNotZero => {
                tty.write("ifnotzero ");
                pretty_expr(&args[0], Some(Intrinsic::Mul), tty);
                tty.shift(INDENT);
                tty.cr();
                pretty_expr(&args[1], None, tty);
                if let Some(a) = args.get(2) {
                    tty.unshift();
                    tty.cr();
                    tty.write("else");
                    tty.shift(INDENT);
                    tty.cr();
                    pretty_expr(a, prev, tty);
                }
                tty.unshift();
                tty.cr();
                tty.write("endif");
            }
        },
        Expression::Const(x, _) => tty.write(x.to_string()),
        Expression::Column { handle: h, .. } => tty.write(&h.name),
        Expression::List(xs) => {
            tty.write("{");
            tty.shift(INDENT);
            tty.cr();
            let mut xs = xs.iter().peekable();
            while let Some(x) = xs.next() {
                pretty_expr(x, None, tty);
                if xs.peek().is_some() {
                    tty.cr();
                }
            }
            tty.unshift();
            tty.cr();
            tty.write("}");
        }
        Expression::ArrayColumn { .. } => unreachable!(),
        Expression::Void => unreachable!(),
    }
}

fn render_constraints(cs: &[Constraint]) {
    for c in cs.iter() {
        match c {
            Constraint::Vanishes {
                handle,
                domain: _,
                expr,
            } => {
                let mut tty = Tty::new();
                pretty_expr(expr, None, &mut tty);
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
                pretty_expr(exp, None, &mut tty);
                println!("\n{}", handle.pretty());
                println!("{} < {}", tty.page_feed(), max);
            }
        }
    }
}

pub fn debug(cs: &ConstraintSet) -> Result<()> {
    render_constraints(&cs.constraints);
    Ok(())
}
