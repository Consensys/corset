use handlebars::Handlebars;
use itertools::Itertools;
use log::*;
use num_traits::ToPrimitive;
use serde::Serialize;
use std::{collections::HashSet, io::Write, unreachable};

use anyhow::*;
use convert_case::{Case, Casing};

use crate::{column::Computation, compiler::*, pretty::Pretty, structs::Handle};

const TEMPLATE: &str = include_str!("wizardiop.go");

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

fn render_shift(shift: isize) -> String {
    if shift == 0 {
        "".into()
    } else {
        format!(".Shift({})", shift)
    }
}

/// Render an expression, panicking if it is not a handle
fn render_handle(cs: &ConstraintSet, e: &Node) -> String {
    match e.e() {
        Expression::Column { handle, .. } => {
            if cs.columns.register(handle).unwrap().width() > 1 {
                panic!("unable to render exo-columns");
            }
            reg_mangle(cs, handle).unwrap()
        }
        _ => unreachable!("{:?}", e.e()),
    }
}

fn render_maybe_exo_handle(cs: &ConstraintSet, e: &Node) -> String {
    match e.e() {
        Expression::Column { handle, .. } => reg_mangle(cs, handle).unwrap(),
        Expression::ExoColumn { handle, .. } => {
            let register = cs.columns.register(handle).unwrap();
            let width = register.width();
            (0..width)
                .map(|i| reg_mangle_ith(cs, handle, i).unwrap())
                .join(", ")
        }
        _ => unreachable!("{:?}", e.e()),
    }
}

fn render_expression(cs: &ConstraintSet, e: &Node) -> String {
    match e.e() {
        Expression::ArrayColumn { .. } => unreachable!(),
        Expression::Const(x) => format!("symbolic.NewConstant(\"{}\")", x),
        Expression::Column { handle, shift, .. } => {
            format!(
                "{}{}.AsVariable()",
                reg_mangle(cs, handle).unwrap(),
                render_shift(*shift as isize)
            )
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
        // ExoColumn are supposed to trickle up to the top level of a constraint
        // expression and can not appear *within* an expression
        Expression::ExoColumn { .. } => unreachable!(),
    }
}

fn render_funcall(cs: &ConstraintSet, func: &Intrinsic, args: &[Node]) -> String {
    match func {
        Intrinsic::Add => make_chain(cs, args, "Add", true),
        Intrinsic::Mul => make_chain(cs, args, "Mul", false),
        Intrinsic::Sub | Intrinsic::VectorSub => make_chain(cs, args, "Sub", true), // TODO: drop later
        Intrinsic::Exp => {
            let exp = args[1]
                .pure_eval()
                .unwrap_or_else(|_| {
                    panic!("Exponent `{}` is not evaluable at compile time", &args[1])
                })
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
        x => {
            unimplemented!("{:?}/{:?}", x, args)
        }
    }
}

fn render_constraints(cs: &ConstraintSet) -> Vec<String> {
    cs.constraints
        .iter()
        .sorted_by_key(|c| c.name())
        .flat_map(|constraint| match constraint {
            Constraint::Vanishes {
                handle,
                domain,
                expr,
            } => render_constraint(cs, &handle.to_string(), domain.clone(), expr),
            Constraint::Lookup {
                handle,
                including,
                included,
            } => vec![format!(
                "build.Inclusion(\"{}\", []Handle{{{}}}, []Handle{{{}}})",
                handle,
                including
                    .iter()
                    .map(|h| render_maybe_exo_handle(cs, h))
                    .collect::<Vec<_>>()
                    .join(", "),
                included
                    .iter()
                    .map(|h| render_maybe_exo_handle(cs, h))
                    .collect::<Vec<_>>()
                    .join(", ")
            )],
            Constraint::Permutation {
                handle, from, to, ..
            } => vec![format!(
                "build.Permutation(\"{}\", []Handle{{{}}}, []Handle{{{}}})",
                handle.mangle().to_case(Case::Snake),
                from.iter()
                    .map(|c| reg_mangle(cs, c).unwrap())
                    .collect::<Vec<_>>()
                    .join(", "),
                to.iter()
                    .map(|h| reg_mangle(cs, h).unwrap())
                    .collect::<Vec<_>>()
                    .join(", ")
            )],
            Constraint::InRange { handle, exp, max } => vec![format!(
                "build.Range(\"{}\", {}, {})",
                handle.mangle().to_case(Case::Snake),
                render_handle(cs, exp),
                max.pretty()
            )],
            Constraint::Normalization {
                handle,
                reference,
                inverted,
            } => {
                let mut r = Vec::new();
                let x = reference.clone();
                let inv_x = Node::column().handle(inverted.clone()).build();
                let x_times_inv_x = Intrinsic::Mul.call(&[x.clone(), inv_x.clone()]).unwrap();
                let one = Node::from_isize(1);

                // X × (1 - X × /X)
                r.append(&mut render_constraint(
                    cs,
                    &format!("{}#1", handle),
                    None,
                    &Intrinsic::Mul
                        .call(&[
                            x.clone(),
                            Intrinsic::Sub
                                .call(&[one.clone(), x_times_inv_x.clone()])
                                .unwrap(),
                        ])
                        .unwrap(),
                ));
                // /X × (1 - X × /X)
                r.append(&mut render_constraint(
                    cs,
                    &format!("{}#2", handle),
                    None,
                    &Intrinsic::Mul
                        .call(&[
                            inv_x.clone(),
                            Intrinsic::Sub
                                .call(&[one.clone(), x_times_inv_x.clone()])
                                .unwrap(),
                        ])
                        .unwrap(),
                ));

                r
            }
        })
        .collect()
}

fn make_size(h: &Handle, sizes: &mut HashSet<String>) -> String {
    let r = format!(
        "build.Settings.Traces.{}",
        h.mangled_module().to_case(Case::Pascal)
    );
    sizes.insert(r.clone());
    r
}

fn reg_mangle(cs: &ConstraintSet, c: &ColumnRef) -> Result<String> {
    let reg_id = cs
        .columns
        .column(c)?
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

fn reg_mangle_ith(cs: &ConstraintSet, c: &ColumnRef, i: usize) -> Result<String> {
    let reg_id = cs
        .columns
        .column(c)?
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
        .map(|h| h.mangle_ith(i))
        .unwrap_or_else(|| Handle::new("", format!("{}_#{}", reg_id, i)).mangle()))
}

#[derive(Serialize, Debug)]
struct WiopColumn {
    go_id: String,
    json_register: String,
    size: String,
}
#[derive(Serialize)]
struct WiopInterleaved {
    go_id: String,
    interleaving: String,
}

fn render_columns(cs: &ConstraintSet, sizes: &mut HashSet<String>) -> Vec<WiopColumn> {
    let mut regs = Vec::new();
    // Determine set of registers allocated to any column which is
    // actually used in a constraint somewhere.
    for (cref, col) in cs.columns.iter() {
        // Determine whether this is an interleaved column (or not).
        let interleaved = cs
            .computations
            .computation_for(&cref)
            .map(|c| c.is_interleaved())
            == Some(true);
        // Only declare columns which are not interleaved, and which
        // are actually used.  Note: interleaved columns are declared
        // separately.
        if !interleaved && col.used {
            // Determine the column size multiplier
            let size_multiplier = cs.length_multiplier(&cref);
            // Store regid and multiplier
            regs.push((col.register.unwrap(), size_multiplier));
        }
    }
    // Sort and remove duplicates (to avoid declaring them twice).
    regs.sort();
    regs.dedup();
    // Construct column declarations
    let mut w_cols = Vec::new();
    // Each unique register requires a declaration.
    for (rid, multiplier) in regs {
        // Access register info
        let register = &cs.columns.registers[rid];
        //
        if register.width() > 1 {
            // Should be unreachable since we are not using exo
            // columns at this time.
            unreachable!()
        } else {
            let handle: &Handle = register.handle.as_ref().unwrap();
            w_cols.push(WiopColumn {
                go_id: handle.mangle(),
                json_register: handle.to_string(),
                size: if multiplier == 1 {
                    make_size(handle, sizes)
                } else {
                    format!("{} * {}", multiplier, make_size(handle, sizes))
                },
            });
        }
    }
    // Sort by go_id so that columns from the same module are grouped
    // together.  This is not strictly necessarily, but is helpful in
    // diagnosing problems.  Also it ensures the column order is
    // deterministic.
    w_cols.sort_by(|l, r| l.go_id.cmp(&r.go_id));
    //
    w_cols
}

fn render_interleaved(cs: &ConstraintSet, _sizes: &mut HashSet<String>) -> Vec<WiopInterleaved> {
    cs.columns
        .iter()
        .filter(|col| {
            cs.computations
                .computation_for(&col.0)
                .map(|comp| comp.is_interleaved())
                == Some(true)
        })
        .sorted_by_cached_key(|col| col.1.handle.mangle())
        .filter_map(|(h, column)| {
            if column.used {
                Some(WiopInterleaved {
                    go_id: reg_mangle(cs, &h).unwrap(),
                    interleaving: if let Some(Computation::Interleaved { froms, .. }) =
                        cs.computations.computation_for(&h)
                    {
                        froms
                            .iter()
                            .map(|c| reg_mangle(cs, c).unwrap())
                            .collect::<Vec<_>>()
                            .join(", ")
                    } else {
                        unreachable!()
                    },
                })
            } else {
                None
            }
        })
        .collect()
}

fn render_constraint(
    cs: &ConstraintSet,
    name: &str,
    domain: Option<Domain<isize>>,
    expr: &Node,
) -> Vec<String> {
    match expr.e() {
        Expression::List(xs) => xs
            .iter()
            .enumerate()
            .flat_map(|(i, x)| render_constraint(cs, &format!("{}#{}", name, i), domain.clone(), x))
            .collect(),
        Expression::ExoColumn { handle, shift, .. } => {
            let register = cs.columns.register_of(handle);

            (0..register.width())
                .map(|i| {
                    let reg_name = reg_mangle_ith(cs, handle, i).unwrap();
                    match &domain {
                        None => {
                            format!(
                                "build.GlobalConstraint(\"{}/{}\", {}{})",
                                name,
                                i,
                                reg_name,
                                render_shift(*shift as isize)
                            )
                        }
                        Some(domain) => domain
                            .iter()
                            .map(|x| {
                                format!(
                                    "build.LocalConstraint(\"{}/{}\", {}{}.AsVariable())",
                                    name,
                                    i,
                                    reg_name,
                                    render_shift(*shift as isize + x),
                                )
                            })
                            .collect::<Vec<_>>()
                            .join("\n"),
                    }
                })
                .collect()
        }
        _ => match domain {
            None => vec![format!(
                "build.GlobalConstraint(\"{}\", {})",
                name,
                render_expression(cs, expr)
            )],
            Some(domain) => domain
                .iter()
                .map(|x| {
                    format!(
                        "build.LocalConstraint(\"{}\", {})",
                        name,
                        render_expression(cs, &expr.clone().shift(x.try_into().unwrap()))
                    )
                })
                .collect::<Vec<_>>(),
        },
    }
}

pub fn render(cs: &ConstraintSet, out_filename: &Option<String>) -> Result<()> {
    #[derive(Serialize)]
    struct TemplateData {
        columns: Vec<WiopColumn>,
        interleaved: Vec<WiopInterleaved>,
        constraints: Vec<String>,
    }
    let mut sizes: HashSet<String> = HashSet::new();

    let mut hb = Handlebars::new();
    hb.set_dev_mode(true);
    hb.set_strict_mode(true);

    let r = hb.render_template(
        TEMPLATE,
        &TemplateData {
            columns: render_columns(cs, &mut sizes),
            interleaved: render_interleaved(cs, &mut sizes),
            constraints: render_constraints(cs),
        },
    )?;

    if let Some(filename) = out_filename.as_ref() {
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
