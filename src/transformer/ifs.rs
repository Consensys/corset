use num_traits::Zero;

use crate::compiler::{Builtin, Constraint, ConstraintSet, Expression, Node};

use super::{flatten_list, wrap};

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

pub fn expand_ifs(cs: &mut ConstraintSet) {
    for c in cs.constraints.iter_mut() {
        if let Constraint::Vanishes { expr: e, .. } = c {
            do_expand_ifs(e); // Ifs create Inv and must be expanded first
        }
    }
}
