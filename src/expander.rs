use num_bigint::BigInt;
use num_traits::{ToPrimitive, Zero};

use crate::{
    column::{ColumnSet, Computation},
    compiler::{
        Builtin, ComputationTable, Constraint, ConstraintSet, Expression, Handle, Kind, Magma,
        Node, Type,
    },
};
use anyhow::Result;

fn invert_expr(e: &Node) -> Node {
    Builtin::Inv.call(&[e.to_owned()])
}

fn validate_inv(cs: &mut Vec<Node>, x_expr: &Node, inv_x_col: &Handle) {
    cs.push(Builtin::Mul.call(&[
        x_expr.clone(),
        Builtin::Sub.call(&[
            Builtin::Mul.call(&[
                x_expr.clone(),
                Node {
                    _e: Expression::Column(
                        inv_x_col.clone(),
                        Kind::Composite(Box::new(Builtin::Inv.call(&[x_expr.clone()]))),
                    ),
                    _t: Some(Type::Column(Magma::Integer)),
                },
            ]),
            Node::one(),
        ]),
    ]));
    cs.push(Builtin::Mul.call(&[
        Node {
            _e: Expression::Column(
                inv_x_col.clone(),
                Kind::Composite(Box::new(Builtin::Inv.call(&[x_expr.clone()]))),
            ),
            _t: Some(Type::Column(Magma::Integer)),
        },
        Builtin::Sub.call(&[
            Builtin::Mul.call(&[
                x_expr.clone(),
                Node {
                    _e: Expression::Column(
                        inv_x_col.clone(),
                        Kind::Composite(Box::new(Builtin::Inv.call(&[x_expr.clone()]))),
                    ),
                    _t: Some(Type::Column(Magma::Integer)),
                },
            ]),
            Node::one(),
        ]),
    ]));
}

fn validate_computation(cs: &mut Vec<Node>, x_expr: &Node, x_col: &Handle) {
    cs.push(Builtin::Sub.call(&[
        x_expr.clone(),
        Node {
            _e: Expression::Column(x_col.to_owned(), Kind::Composite(Box::new(x_expr.clone()))),
            _t: Some(Type::Column(Magma::Integer)),
        },
    ]))
}

fn expression_to_name(e: &Node, prefix: &str) -> String {
    format!("{}_{}", prefix, e).replace(' ', "_")
}

fn wrap(ex: Node) -> Node {
    match ex.e() {
        Expression::List(_) => ex,
        _ => Node::from_expr(Expression::List(vec![ex])),
    }
}

fn flatten_list(mut e: Node) -> Node {
    match e.e_mut() {
        Expression::List(ref mut xs) => {
            if xs.len() == 1 {
                flatten_list(xs.pop().unwrap())
            } else {
                e
            }
        }
        _ => e,
    }
}

fn do_expand_ifs(e: &mut Node) {
    match e.e_mut() {
        Expression::List(es) => {
            for e in es.iter_mut() {
                do_expand_ifs(e);
            }
        }
        Expression::Funcall { func, args, .. } => {
            for e in args.iter_mut() {
                do_expand_ifs(e);
            }
            if matches!(func, Builtin::IfZero | Builtin::IfNotZero) {
                let cond = args[0].clone();
                // If the condition reduces to a constant, we can determine the result
                if let Ok(constant_cond) = cond.pure_eval() {
                    match func {
                        Builtin::IfZero => {
                            if constant_cond.is_zero() {
                                *e = args[1].clone();
                            } else {
                                *e = flatten_list(args.get(2).cloned().unwrap_or_else(Node::zero));
                            }
                        }
                        Builtin::IfNotZero => {
                            if !constant_cond.is_zero() {
                                *e = args[1].clone();
                            } else {
                                *e = flatten_list(args.get(2).cloned().unwrap_or_else(Node::zero));
                            }
                        }
                        _ => unreachable!(),
                    }
                } else {
                    let conds = {
                        let cond_not_zero = cond.clone();
                        // If the condition is binary, cond_zero = 1 - x...
                        let cond_zero = if args[0].t().is_bool() {
                            Builtin::Sub.call(&[Node::one(), cond])
                        } else {
                            // ...otherwise, cond_zero = 1 - x.INV(x)
                            Builtin::Sub.call(&[
                                Node::one(),
                                Builtin::Mul.call(&[cond.clone(), Builtin::Inv.call(&[cond])]),
                            ])
                        };
                        match func {
                            Builtin::IfZero => [cond_zero, cond_not_zero],
                            Builtin::IfNotZero => [cond_not_zero, cond_zero],
                            _ => unreachable!(),
                        }
                    };

                    // Order the then/else blocks
                    let then_else = vec![args.get(1), args.get(2)]
                        .into_iter()
                        .enumerate()
                        // Only keep the non-empty branches
                        .filter_map(|(i, ex)| ex.map(|ex| (i, ex)))
                        // Ensure branches are wrapped in lists
                        .map(|(i, ex)| (i, wrap(ex.clone())))
                        // Map the corresponding then/else operations on the branches
                        .flat_map(|(i, exs)| {
                            if let Expression::List(exs) = exs.e() {
                                exs.iter()
                                    .map(|ex: &Node| {
                                        ex.flat_map(&|e| {
                                            Builtin::Mul.call(&[conds[i].clone(), e.clone()])
                                        })
                                    })
                                    .collect::<Vec<_>>()
                            } else {
                                unreachable!()
                            }
                        })
                        .flatten()
                        .collect::<Vec<_>>();
                    *e = if then_else.len() == 1 {
                        then_else[0].clone()
                    } else {
                        Node::from_expr(Expression::List(then_else))
                    }
                };
            }
        }
        _ => (),
    }
}

/// For all Builtin::Inv encountered, create a new column and the associated constraints
/// pre-computing and proving the inverted column.
fn do_expand_inv<T: Clone + Ord>(
    e: &mut Node,
    cols: &mut ColumnSet<T>,
    comps: &mut ComputationTable,
    new_cs: &mut Vec<Node>,
) -> Result<()> {
    match e.e_mut() {
        Expression::List(es) => {
            for e in es.iter_mut() {
                do_expand_inv(e, cols, comps, new_cs)?;
            }
            Ok(())
        }
        Expression::Funcall { func, args, .. } => {
            for e in args.iter_mut() {
                do_expand_inv(e, cols, comps, new_cs)?;
            }
            if matches!(func, Builtin::Inv) {
                let module = &args[0].module().unwrap();
                let inverted_expr = &mut args[0];
                let inverted_handle = Handle::new(module, expression_to_name(inverted_expr, "INV"));
                if cols.get(&inverted_handle).is_err() {
                    validate_inv(new_cs, inverted_expr, &inverted_handle);
                    cols.insert_column(
                        &inverted_handle,
                        Type::Column(Magma::Integer),
                        true,
                        Kind::Composite(()),
                        true,
                    )?;
                    comps.insert(
                        &inverted_handle,
                        Computation::Composite {
                            target: inverted_handle.clone(),
                            exp: invert_expr(inverted_expr),
                        },
                    )?;
                }
                *e = Node {
                    _e: Expression::Column(inverted_handle.clone(), Kind::Atomic),
                    _t: Some(Type::Column(Magma::Integer)),
                }
            }
            Ok(())
        }
        _ => Ok(()),
    }
}

fn do_expand_expr<T: Clone + Ord>(
    e: &Node,
    cols: &mut ColumnSet<T>,
    comps: &mut ComputationTable,
    new_cs: &mut Vec<Node>,
) -> Result<Node> {
    match e.e() {
        Expression::Column(..) => Ok(e.clone()),
        _ => {
            let module = e.module().unwrap();
            let new_handle = Handle::new(module, expression_to_name(e, "EXPAND"));
            validate_computation(new_cs, e, &new_handle);
            cols.insert_column(
                &new_handle,
                Type::Column(Magma::Integer),
                true,
                Kind::Phantom,
                true,
            )?;

            let _ = comps.insert(
                &new_handle,
                Computation::Composite {
                    target: new_handle.clone(),
                    exp: e.clone(),
                },
            );
            Ok(Node {
                _e: Expression::Column(new_handle, Kind::Phantom),
                _t: Some(Type::Column(Magma::Integer)),
            })
        }
    }
}

fn do_lower_shifts(e: &mut Node, depth: isize) {
    match e.e_mut() {
        Expression::Funcall { func, args } => {
            if matches!(func, Builtin::Shift) {
                let shift = args[1].pure_eval().unwrap().to_isize().unwrap();
                *e = args[0].clone();
                do_lower_shifts(e, depth + shift);
            } else {
                for x in args.iter_mut() {
                    do_lower_shifts(x, depth)
                }
            }
        }
        Expression::Const(_, _) => (),
        Expression::Column(..) => {
            if depth != 0 {
                let column = e.clone();
                *e = Builtin::Shift.call(&[
                    column,
                    Node::from_expr(Expression::Const(BigInt::from(depth), None)),
                ])
            }
        }
        Expression::ArrayColumn(..) => (),
        Expression::List(xs) => {
            for x in xs.iter_mut() {
                do_lower_shifts(x, depth)
            }
        }
        Expression::Void => (),
    }
}

pub fn lower_shifts(cs: &mut ConstraintSet) {
    for c in cs.constraints.iter_mut() {
        match c {
            Constraint::Vanishes { expr: e, .. } => {
                do_lower_shifts(e, 0);
            }
            Constraint::Plookup(_name, parents, children) => {
                for e in parents.iter_mut().chain(children.iter_mut()) {
                    do_lower_shifts(e, 0);
                }
            }
            Constraint::Permutation(..) => (),
            Constraint::InRange(_, e, _) => {
                do_lower_shifts(e, 0);
            }
        }
    }
}

pub fn expand_ifs(cs: &mut ConstraintSet) {
    for c in cs.constraints.iter_mut() {
        if let Constraint::Vanishes { expr: e, .. } = c {
            do_expand_ifs(e); // Ifs create Inv and must be expanded first
        }
    }
}

pub fn expand_invs(cs: &mut ConstraintSet) -> Result<()> {
    let mut new_cs_inv = vec![];
    for c in cs.constraints.iter_mut() {
        if let Constraint::Vanishes { expr: e, .. } = c {
            do_expand_inv(e, &mut cs.modules, &mut cs.computations, &mut new_cs_inv)?;
        }
    }
    if !new_cs_inv.is_empty() {
        cs.constraints.push(Constraint::Vanishes {
            handle: Handle::new("RESERVED", "INV_CONSTRAINTS"),
            domain: None,
            expr: Box::new(Expression::List(new_cs_inv).into()),
        });
    }

    cs.update_ids();
    Ok(())
}

pub fn expand_constraints(cs: &mut ConstraintSet) -> Result<()> {
    let mut new_cs_exps = vec![];
    for c in cs.constraints.iter_mut() {
        match c {
            Constraint::Plookup(_name, parents, children) => {
                for e in parents.iter_mut().chain(children.iter_mut()) {
                    *e =
                        do_expand_expr(e, &mut cs.modules, &mut cs.computations, &mut new_cs_exps)?;
                }
            }
            Constraint::InRange(_, e, _) => {
                *e = do_expand_expr(e, &mut cs.modules, &mut cs.computations, &mut new_cs_exps)?;
            }
            _ => (),
        }
    }
    if !new_cs_exps.is_empty() {
        cs.constraints.push(Constraint::Vanishes {
            handle: Handle::new("RESERVED", "EXPANSION_CONSTRAINTS"),
            domain: None,
            expr: Box::new(Expression::List(new_cs_exps).into()),
        });
    }

    cs.update_ids();

    Ok(())
}
