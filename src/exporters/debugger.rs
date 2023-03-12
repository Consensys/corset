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
        (Intrinsic::Add, Intrinsic::Exp) => todo!(),
        (Intrinsic::Add, Intrinsic::Shift) => todo!(),
        (Intrinsic::Add, Intrinsic::Neg) => todo!(),
        (Intrinsic::Add, Intrinsic::Inv) => todo!(),
        (Intrinsic::Add, Intrinsic::Not) => todo!(),
        (Intrinsic::Add, Intrinsic::Nth) => todo!(),
        (Intrinsic::Add, Intrinsic::Eq) => todo!(),
        (Intrinsic::Add, Intrinsic::Begin) => todo!(),
        (Intrinsic::Add, Intrinsic::IfZero) => todo!(),
        (Intrinsic::Add, Intrinsic::IfNotZero) => todo!(),
        (Intrinsic::Sub, Intrinsic::Add) => todo!(),
        (Intrinsic::Sub, Intrinsic::Sub) => todo!(),
        (Intrinsic::Sub, Intrinsic::Mul) => Ordering::Less,
        (Intrinsic::Sub, Intrinsic::Exp) => todo!(),
        (Intrinsic::Sub, Intrinsic::Shift) => todo!(),
        (Intrinsic::Sub, Intrinsic::Neg) => todo!(),
        (Intrinsic::Sub, Intrinsic::Inv) => todo!(),
        (Intrinsic::Sub, Intrinsic::Not) => todo!(),
        (Intrinsic::Sub, Intrinsic::Nth) => todo!(),
        (Intrinsic::Sub, Intrinsic::Eq) => todo!(),
        (Intrinsic::Sub, Intrinsic::Begin) => todo!(),
        (Intrinsic::Sub, Intrinsic::IfZero) => todo!(),
        (Intrinsic::Sub, Intrinsic::IfNotZero) => todo!(),
        (Intrinsic::Mul, Intrinsic::Add) => Ordering::Greater,
        (Intrinsic::Mul, Intrinsic::Sub) => Ordering::Greater,
        (Intrinsic::Mul, Intrinsic::Mul) => Ordering::Equal,
        (Intrinsic::Mul, Intrinsic::Exp) => todo!(),
        (Intrinsic::Mul, Intrinsic::Shift) => todo!(),
        (Intrinsic::Mul, Intrinsic::Neg) => todo!(),
        (Intrinsic::Mul, Intrinsic::Inv) => todo!(),
        (Intrinsic::Mul, Intrinsic::Not) => todo!(),
        (Intrinsic::Mul, Intrinsic::Nth) => todo!(),
        (Intrinsic::Mul, Intrinsic::Eq) => todo!(),
        (Intrinsic::Mul, Intrinsic::Begin) => todo!(),
        (Intrinsic::Mul, Intrinsic::IfZero) => todo!(),
        (Intrinsic::Mul, Intrinsic::IfNotZero) => todo!(),
        (Intrinsic::Exp, Intrinsic::Add) => todo!(),
        (Intrinsic::Exp, Intrinsic::Sub) => todo!(),
        (Intrinsic::Exp, Intrinsic::Mul) => todo!(),
        (Intrinsic::Exp, Intrinsic::Exp) => todo!(),
        (Intrinsic::Exp, Intrinsic::Shift) => todo!(),
        (Intrinsic::Exp, Intrinsic::Neg) => todo!(),
        (Intrinsic::Exp, Intrinsic::Inv) => todo!(),
        (Intrinsic::Exp, Intrinsic::Not) => todo!(),
        (Intrinsic::Exp, Intrinsic::Nth) => todo!(),
        (Intrinsic::Exp, Intrinsic::Eq) => todo!(),
        (Intrinsic::Exp, Intrinsic::Begin) => todo!(),
        (Intrinsic::Exp, Intrinsic::IfZero) => todo!(),
        (Intrinsic::Exp, Intrinsic::IfNotZero) => todo!(),
        (Intrinsic::Shift, Intrinsic::Add) => todo!(),
        (Intrinsic::Shift, Intrinsic::Sub) => todo!(),
        (Intrinsic::Shift, Intrinsic::Mul) => todo!(),
        (Intrinsic::Shift, Intrinsic::Exp) => todo!(),
        (Intrinsic::Shift, Intrinsic::Shift) => todo!(),
        (Intrinsic::Shift, Intrinsic::Neg) => todo!(),
        (Intrinsic::Shift, Intrinsic::Inv) => todo!(),
        (Intrinsic::Shift, Intrinsic::Not) => todo!(),
        (Intrinsic::Shift, Intrinsic::Nth) => todo!(),
        (Intrinsic::Shift, Intrinsic::Eq) => todo!(),
        (Intrinsic::Shift, Intrinsic::Begin) => todo!(),
        (Intrinsic::Shift, Intrinsic::IfZero) => todo!(),
        (Intrinsic::Shift, Intrinsic::IfNotZero) => todo!(),
        (Intrinsic::Neg, Intrinsic::Add) => todo!(),
        (Intrinsic::Neg, Intrinsic::Sub) => todo!(),
        (Intrinsic::Neg, Intrinsic::Mul) => todo!(),
        (Intrinsic::Neg, Intrinsic::Exp) => todo!(),
        (Intrinsic::Neg, Intrinsic::Shift) => todo!(),
        (Intrinsic::Neg, Intrinsic::Neg) => todo!(),
        (Intrinsic::Neg, Intrinsic::Inv) => todo!(),
        (Intrinsic::Neg, Intrinsic::Not) => todo!(),
        (Intrinsic::Neg, Intrinsic::Nth) => todo!(),
        (Intrinsic::Neg, Intrinsic::Eq) => todo!(),
        (Intrinsic::Neg, Intrinsic::Begin) => todo!(),
        (Intrinsic::Neg, Intrinsic::IfZero) => todo!(),
        (Intrinsic::Neg, Intrinsic::IfNotZero) => todo!(),
        (Intrinsic::Inv, Intrinsic::Add) => todo!(),
        (Intrinsic::Inv, Intrinsic::Sub) => todo!(),
        (Intrinsic::Inv, Intrinsic::Mul) => todo!(),
        (Intrinsic::Inv, Intrinsic::Exp) => todo!(),
        (Intrinsic::Inv, Intrinsic::Shift) => todo!(),
        (Intrinsic::Inv, Intrinsic::Neg) => todo!(),
        (Intrinsic::Inv, Intrinsic::Inv) => todo!(),
        (Intrinsic::Inv, Intrinsic::Not) => todo!(),
        (Intrinsic::Inv, Intrinsic::Nth) => todo!(),
        (Intrinsic::Inv, Intrinsic::Eq) => todo!(),
        (Intrinsic::Inv, Intrinsic::Begin) => todo!(),
        (Intrinsic::Inv, Intrinsic::IfZero) => todo!(),
        (Intrinsic::Inv, Intrinsic::IfNotZero) => todo!(),
        (Intrinsic::Not, Intrinsic::Add) => todo!(),
        (Intrinsic::Not, Intrinsic::Sub) => todo!(),
        (Intrinsic::Not, Intrinsic::Mul) => todo!(),
        (Intrinsic::Not, Intrinsic::Exp) => todo!(),
        (Intrinsic::Not, Intrinsic::Shift) => todo!(),
        (Intrinsic::Not, Intrinsic::Neg) => todo!(),
        (Intrinsic::Not, Intrinsic::Inv) => todo!(),
        (Intrinsic::Not, Intrinsic::Not) => todo!(),
        (Intrinsic::Not, Intrinsic::Nth) => todo!(),
        (Intrinsic::Not, Intrinsic::Eq) => todo!(),
        (Intrinsic::Not, Intrinsic::Begin) => todo!(),
        (Intrinsic::Not, Intrinsic::IfZero) => todo!(),
        (Intrinsic::Not, Intrinsic::IfNotZero) => todo!(),
        (Intrinsic::Nth, Intrinsic::Add) => todo!(),
        (Intrinsic::Nth, Intrinsic::Sub) => todo!(),
        (Intrinsic::Nth, Intrinsic::Mul) => todo!(),
        (Intrinsic::Nth, Intrinsic::Exp) => todo!(),
        (Intrinsic::Nth, Intrinsic::Shift) => todo!(),
        (Intrinsic::Nth, Intrinsic::Neg) => todo!(),
        (Intrinsic::Nth, Intrinsic::Inv) => todo!(),
        (Intrinsic::Nth, Intrinsic::Not) => todo!(),
        (Intrinsic::Nth, Intrinsic::Nth) => todo!(),
        (Intrinsic::Nth, Intrinsic::Eq) => todo!(),
        (Intrinsic::Nth, Intrinsic::Begin) => todo!(),
        (Intrinsic::Nth, Intrinsic::IfZero) => todo!(),
        (Intrinsic::Nth, Intrinsic::IfNotZero) => todo!(),
        (Intrinsic::Eq, Intrinsic::Add) => todo!(),
        (Intrinsic::Eq, Intrinsic::Sub) => todo!(),
        (Intrinsic::Eq, Intrinsic::Mul) => todo!(),
        (Intrinsic::Eq, Intrinsic::Exp) => todo!(),
        (Intrinsic::Eq, Intrinsic::Shift) => todo!(),
        (Intrinsic::Eq, Intrinsic::Neg) => todo!(),
        (Intrinsic::Eq, Intrinsic::Inv) => todo!(),
        (Intrinsic::Eq, Intrinsic::Not) => todo!(),
        (Intrinsic::Eq, Intrinsic::Nth) => todo!(),
        (Intrinsic::Eq, Intrinsic::Eq) => todo!(),
        (Intrinsic::Eq, Intrinsic::Begin) => todo!(),
        (Intrinsic::Eq, Intrinsic::IfZero) => todo!(),
        (Intrinsic::Eq, Intrinsic::IfNotZero) => todo!(),
        (Intrinsic::Begin, Intrinsic::Add) => todo!(),
        (Intrinsic::Begin, Intrinsic::Sub) => todo!(),
        (Intrinsic::Begin, Intrinsic::Mul) => todo!(),
        (Intrinsic::Begin, Intrinsic::Exp) => todo!(),
        (Intrinsic::Begin, Intrinsic::Shift) => todo!(),
        (Intrinsic::Begin, Intrinsic::Neg) => todo!(),
        (Intrinsic::Begin, Intrinsic::Inv) => todo!(),
        (Intrinsic::Begin, Intrinsic::Not) => todo!(),
        (Intrinsic::Begin, Intrinsic::Nth) => todo!(),
        (Intrinsic::Begin, Intrinsic::Eq) => todo!(),
        (Intrinsic::Begin, Intrinsic::Begin) => todo!(),
        (Intrinsic::Begin, Intrinsic::IfZero) => todo!(),
        (Intrinsic::Begin, Intrinsic::IfNotZero) => todo!(),
        (Intrinsic::IfZero, Intrinsic::Add) => todo!(),
        (Intrinsic::IfZero, Intrinsic::Sub) => todo!(),
        (Intrinsic::IfZero, Intrinsic::Mul) => todo!(),
        (Intrinsic::IfZero, Intrinsic::Exp) => todo!(),
        (Intrinsic::IfZero, Intrinsic::Shift) => todo!(),
        (Intrinsic::IfZero, Intrinsic::Neg) => todo!(),
        (Intrinsic::IfZero, Intrinsic::Inv) => todo!(),
        (Intrinsic::IfZero, Intrinsic::Not) => todo!(),
        (Intrinsic::IfZero, Intrinsic::Nth) => todo!(),
        (Intrinsic::IfZero, Intrinsic::Eq) => todo!(),
        (Intrinsic::IfZero, Intrinsic::Begin) => todo!(),
        (Intrinsic::IfZero, Intrinsic::IfZero) => todo!(),
        (Intrinsic::IfZero, Intrinsic::IfNotZero) => todo!(),
        (Intrinsic::IfNotZero, Intrinsic::Add) => todo!(),
        (Intrinsic::IfNotZero, Intrinsic::Sub) => todo!(),
        (Intrinsic::IfNotZero, Intrinsic::Mul) => todo!(),
        (Intrinsic::IfNotZero, Intrinsic::Exp) => todo!(),
        (Intrinsic::IfNotZero, Intrinsic::Shift) => todo!(),
        (Intrinsic::IfNotZero, Intrinsic::Neg) => todo!(),
        (Intrinsic::IfNotZero, Intrinsic::Inv) => todo!(),
        (Intrinsic::IfNotZero, Intrinsic::Not) => todo!(),
        (Intrinsic::IfNotZero, Intrinsic::Nth) => todo!(),
        (Intrinsic::IfNotZero, Intrinsic::Eq) => todo!(),
        (Intrinsic::IfNotZero, Intrinsic::Begin) => todo!(),
        (Intrinsic::IfNotZero, Intrinsic::IfZero) => todo!(),
        (Intrinsic::IfNotZero, Intrinsic::IfNotZero) => todo!(),
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
                        tty.write(format!(" {} ", f.to_string()));
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
        Expression::Column(h, _, _) => tty.write(&h.name),
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
        Expression::ArrayColumn(_, _) => unreachable!(),
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
            Constraint::Plookup { .. } => (),
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
