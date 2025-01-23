use crate::column::Computation;
use crate::compiler::codetyper::Tty;
use crate::compiler::{Constraint, ConstraintSet, Expression, Intrinsic, Node};
use crate::constants;
use crate::pretty::Pretty;
use crate::structs::Handle;
use anyhow::*;
use convert_case::{Case, Casing};
use ellipse::Ellipse;
use itertools::Itertools;
use owo_colors::XtermColors;
use owo_colors::{colored::Color, OwoColorize};
use std::cmp::Ordering;

fn priority(a: Intrinsic, b: Intrinsic) -> Ordering {
    match (a, b) {
        (Intrinsic::Add | Intrinsic::VectorAdd, Intrinsic::Add | Intrinsic::VectorAdd) => {
            Ordering::Equal
        }
        (Intrinsic::Add | Intrinsic::VectorAdd, Intrinsic::Sub | Intrinsic::VectorSub) => {
            Ordering::Less
        }
        (Intrinsic::Add | Intrinsic::VectorAdd, Intrinsic::Mul | Intrinsic::VectorMul) => {
            Ordering::Less
        }
        (Intrinsic::Sub | Intrinsic::VectorSub, Intrinsic::Mul | Intrinsic::VectorMul) => {
            Ordering::Less
        }
        (Intrinsic::Sub | Intrinsic::VectorSub, Intrinsic::Add | Intrinsic::VectorAdd) => {
            Ordering::Equal
        }
        (Intrinsic::Mul | Intrinsic::VectorMul, Intrinsic::Add | Intrinsic::VectorAdd) => {
            Ordering::Greater
        }
        (Intrinsic::Mul | Intrinsic::VectorMul, Intrinsic::Sub | Intrinsic::VectorSub) => {
            Ordering::Greater
        }
        (Intrinsic::Mul | Intrinsic::VectorMul, Intrinsic::Mul | Intrinsic::VectorMul) => {
            Ordering::Equal
        }
        (Intrinsic::Sub | Intrinsic::VectorSub, Intrinsic::Sub | Intrinsic::VectorSub) => {
            Ordering::Equal
        }
        (Intrinsic::Sub | Intrinsic::VectorSub, Intrinsic::Exp) => Ordering::Less,
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
            Intrinsic::Add
            | Intrinsic::Sub
            | Intrinsic::Mul
            | Intrinsic::VectorAdd
            | Intrinsic::VectorSub
            | Intrinsic::VectorMul => {
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
            Intrinsic::Neg => {
                tty.write("-");
                pretty_expr(&args[0], prev, tty, show_types);
            }
            Intrinsic::Inv => {
                tty.write("INV(");
                pretty_expr(&args[0], prev, tty, show_types);
                tty.write(")");
            }
            Intrinsic::Normalize => {
                tty.write("NORM(");
                pretty_expr(&args[0], prev, tty, show_types);
                tty.write(")");
            }
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
                tty.write("if-non-zero".color(c).bold().to_string());
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
        Expression::Const(x) => tty.write(x.to_string()),
        Expression::Column { handle, shift, .. } | Expression::ExoColumn { handle, shift, .. } => {
            let color = handle
                .to_string_short()
                .chars()
                .fold(0, |ax, c| ax + c as usize)
                % 255
                + 1;
            tty.write(
                handle
                    .to_string_short()
                    .color(XtermColors::from(color as u8))
                    .to_string(),
            );
            if *shift != 0 {
                tty.write(if *shift > 0 { "₊" } else { "" });
                tty.write(crate::pretty::subscript(&shift.to_string()));
            }
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
                    domain,
                    expr,
                } => {
                    let mut tty = Tty::new().with_guides();
                    println!(
                        "\n{}{} :=",
                        handle.pretty(),
                        if let Some(domain) = domain {
                            domain.to_string()
                        } else {
                            String::new()
                        }
                    );
                    pretty_expr(expr, None, &mut tty, show_types);
                    println!("{}", tty.page_feed());
                }
                Constraint::Lookup {
                    handle,
                    including,
                    included,
                } => {
                    println!("\n{}", handle.pretty());
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
                Constraint::Permutation {
                    handle, from, to, ..
                } => {
                    println!("\n{}", handle.pretty());
                    println!(
                        "[{}] perm. [{}]",
                        to.iter().map(|c| c.pretty()).join(", "),
                        from.iter().map(|c| c.pretty()).join(", ")
                    )
                }
                Constraint::InRange { handle, exp, max } => {
                    let mut tty = Tty::new().with_guides();
                    pretty_expr(exp, None, &mut tty, false);
                    println!("\n{}", handle.pretty());
                    println!("{} < {}", tty.page_feed(), max);
                }
                Constraint::Normalization {
                    handle,
                    reference,
                    inverted,
                    ..
                } => {
                    println!("\n{} :=", handle.pretty());
                    if reference.bit_size() > constants::FIELD_BITSIZE {
                        println!("TODO XXX");
                    } else {
                        println!(
                            "|{}| == {} × {}",
                            reference.pretty(),
                            reference.pretty(),
                            inverted.pretty()
                        );
                        println!(
                            "{}×(1 - {}×{}) = 0",
                            reference.pretty(),
                            reference.pretty(),
                            inverted.pretty()
                        );
                        println!(
                            "{}×(1 - {}×{}) = 0",
                            inverted.pretty(),
                            reference.pretty(),
                            inverted.pretty()
                        );
                    }
                }
            }
        }
    }
}

fn render_modules(cs: &ConstraintSet) {
    println!("\n{}", "=== Modules ===".bold().yellow());
    for (module, spilling) in cs.columns.spilling.iter().sorted_by_key(|s| s.0) {
        println!("{}: spilling {}", module, spilling);
    }
}

fn render_constants(cs: &ConstraintSet) {
    println!("\n{}", "=== Constants ===".bold().yellow());
    for (name, value) in cs
        .constants
        .iter()
        .sorted_by_key(|s| (&s.0.module, &s.0.name))
    {
        println!("{} := 0x{}", name.pretty(), value.to_str_radix(16));
    }
}

fn render_columns(cs: &ConstraintSet) {
    println!("\n{}", "=== Columns ===".bold().yellow());

    println!(
        "{:>4}{:>80}{:>6}{:>4}{:>50}",
        "ID", "Name", "Type", "×", "Reg."
    );
    for (r, col) in cs.columns.iter().sorted_by_key(|c| c.1.register) {
        println!(
            "{:>4}{:>80}{:>6}{:>4}{:>50}",
            r.as_id(),
            col.handle.to_string().as_str().truncate_ellipse(75),
            col.t.to_string(),
            cs.length_multiplier(&r),
            col.register
                .map(|r| format!(
                    "r{}/{}ι{}",
                    r,
                    cs.columns.registers[r]
                        .handle
                        .as_ref()
                        .map(|h| h.to_string())
                        .unwrap_or("?".into()),
                    cs.columns.registers[r].width()
                ))
                .unwrap_or_default()
                .as_str()
                .truncate_ellipse(45)
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
                "{} ≜ ↻ {}",
                target.pretty(),
                froms.iter().map(|c| cs.handle(c).pretty()).join(", "),
            ),
            Computation::SortingConstraints { sorted, .. } => println!(
                "Sorting constraints for {}",
                sorted.iter().map(|c| cs.handle(c).pretty()).join(", ")
            ),
            Computation::ExoOperation {
                op,
                sources,
                target,
            } => println!(
                "{} ≜ {} {} {}",
                target.pretty(),
                sources[0].pretty(),
                op,
                sources[1].pretty(),
            ),
            Computation::ExoConstant { value, target } => {
                println!("{} := {}", target.pretty(), value)
            }
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

fn render_registers(cs: &ConstraintSet) {
    println!("\n{}", "=== Registers ===".bold().yellow());
    for (i, r) in cs.columns.registers.iter().enumerate() {
        print!("r{}\t{}", i, r.magma);

        match &r.handle {
            Some(h) => {
                println!("\t{}", h);
            }
            _ => {
                println!();
            }
        }
    }
}

fn render_stats(cs: &ConstraintSet) {
    println!("\n{}", "=== Stats ===".bold().yellow());
    print_constraint_stats(cs);
    print_assignment_stats(cs);
    print_register_count(cs);
    print_register_stats(cs, "    ", 1, 1);
    print_register_stats(cs, "    ", 2, 4);
    print_register_stats(cs, "    ", 5, 8);
    print_register_stats(cs, "   ", 9, 16);
    print_register_stats(cs, "  ", 17, 32);
    print_register_stats(cs, "  ", 33, 64);
    print_register_stats(cs, " ", 65, 128);
    print_register_stats(cs, "", 129, 256);
}

fn print_constraint_stats(cs: &ConstraintSet) {
    let mut vanishes = 0;
    let mut lookups = 0;
    let mut permutations = 0;
    let mut ranges = 0;
    for c in &cs.constraints {
        match c {
            Constraint::Vanishes { expr, .. } => {
                // Vanishing constraints are typically packaged up as
                // one.
                vanishes += subconstraint_count(expr.e());
            }
            Constraint::Lookup { .. } => {
                lookups += 1;
            }
            Constraint::Permutation { .. } => {
                permutations += 1;
            }
            Constraint::InRange { .. } => {
                ranges += 1;
            }
            Constraint::Normalization { .. } => {
                vanishes += 2;
            }
        }
    }
    //
    println!("             Constraints = {vanishes}");
    println!("                 Lookups = {lookups}");
    println!("            Permutations = {permutations}");
    println!("                   Range = {ranges}");
}

fn print_assignment_stats(cs: &ConstraintSet) {
    let mut composites = 0;
    let mut interleaved = 0;
    let mut lex_sorting = 0;
    let mut sorted = 0;
    let mut other = 0;

    for c in &cs.computations.computations {
        match c {
            Computation::Composite { .. } => {
                composites += 1;
            }
            Computation::Interleaved { .. } => {
                interleaved += 1;
            }
            Computation::Sorted { .. } => {
                sorted += 1;
            }
            Computation::SortingConstraints { .. } => {
                lex_sorting += 1;
            }
            _ => {
                other += 1;
            }
        }
    }

    println!("        Computed Columns = {composites}");
    println!("           Interleavings = {interleaved}");
    println!(" Lexicographic Orderings = {lex_sorting}");
    println!("     Sorted Permutations = {sorted}");
    println!("                   Other = {other}");
}

fn subconstraint_count(expr: &Expression) -> usize {
    match expr {
        Expression::List(es) => es.len(),
        _ => 1,
    }
}

fn print_register_count(cs: &ConstraintSet) {
    let mut count = 0;

    for (_, _) in cs.columns.registers.iter().enumerate() {
        count += 1;
    }
    //
    println!("           Columns (all) = {count}");
}
fn print_register_stats(cs: &ConstraintSet, padding: &str, low_width: usize, high_width: usize) {
    let mut count = 0;
    print!("{padding}");
    for (_, r) in cs.columns.registers.iter().enumerate() {
        let bw = r.magma.bit_size();
        if bw >= low_width && bw <= high_width {
            count += 1;
        }
    }
    //
    println!(" Columns ({low_width}..{high_width} bits) = {count}");
}

fn render_spilling(cs: &ConstraintSet) {
    println!("\n{}", "=== Spilling ===".bold().yellow());
    for (module, spilling) in cs.columns.spilling.iter() {
        println!("{:>10}: {:>4}", module.blue().bold(), spilling);
    }
}

fn render_spilling_toml(cs: &ConstraintSet) {
    println!("# Automatically generated via `corset debug -s --toml`");
    println!("[spillings]");
    for (module, spilling) in cs.columns.spilling.iter() {
        // Convert name to screaming snake case.
        let name = module.to_case(Case::UpperSnake);
        //
        println!("{:>10} = {:>4}", name, spilling);
    }
}

pub(crate) struct DebugSettings {
    pub modules: bool,
    pub constraints: bool,
    pub constants: bool,
    pub columns: bool,
    pub computations: bool,
    pub perspectives: bool,
    pub registers: bool,
    pub types: bool,
    pub spilling: bool,
    pub stats: bool,
    pub toml: bool,
}

pub(crate) fn debug(
    cs: &ConstraintSet,
    settings: DebugSettings,
    only: Option<&Vec<String>>,
    skip: &[String],
) -> Result<()> {
    if settings.modules {
        render_modules(cs);
    }
    if settings.constants {
        render_constants(cs);
    }
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
    if settings.registers {
        render_registers(cs);
    }
    if settings.stats {
        render_stats(cs);
    }
    if settings.spilling && settings.toml {
        render_spilling_toml(cs);
    } else if settings.spilling {
        render_spilling(cs);
    }
    Ok(())
}
