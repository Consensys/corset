use anyhow::Result;
use num_traits::Zero;

use crate::compiler::{Conditioning, Constraint, ConstraintSet, Expression, Intrinsic, Node};

use super::{flatten_list, wrap};

fn do_expand_ifs(e: &mut Node) -> Result<()> {
    match e.e_mut() {
        Expression::List(es) => {
            for e in es.iter_mut() {
                do_expand_ifs(e)?;
            }
        }
        Expression::Funcall { func, args, .. } => {
            for e in args.iter_mut() {
                do_expand_ifs(e)?;
            }
            if matches!(func, Intrinsic::IfZero | Intrinsic::IfNotZero) {
                let cond = args[0].clone();
                let if_not_zero = matches!(func, Intrinsic::IfNotZero);
                assert!(if if_not_zero {
                    matches!(cond.t().c(), Conditioning::Boolean | Conditioning::None)
                } else {
                    matches!(cond.t().c(), Conditioning::Loobean | Conditioning::None)
                });

                // If the condition reduces to a constant, we can determine the result
                if let Ok(constant_cond) = cond.pure_eval() {
                    if if_not_zero {
                        if !constant_cond.is_zero() {
                            *e = args[1].clone();
                        } else {
                            *e = flatten_list(args.get(2).cloned().unwrap_or_else(Node::zero));
                        }
                    } else {
                        if constant_cond.is_zero() {
                            *e = args[1].clone();
                        } else {
                            *e = flatten_list(args.get(2).cloned().unwrap_or_else(Node::zero));
                        }
                    }
                } else {
                    let conds = {
                        let cond_not_zero = cond.clone();
                        let cond_zero = Intrinsic::Sub
                            .call(&[Node::one(), Intrinsic::Normalize.call(&[cond.clone()])?])?;
                        if if_not_zero {
                            [cond_not_zero, cond_zero]
                        } else {
                            [cond_zero, cond_not_zero]
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
                                            Intrinsic::Mul
                                                .call(&[conds[i].clone(), e.clone()])
                                                .unwrap()
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

    Ok(())
}

/// Pull `if` conditionals out of nested positions and into top-most
/// positions.  Specifically, something like this:
///
/// ```lisp
/// (defconstraint test () (+ (if A B) C))
/// ```
///
/// Has the nested `if` raised into the following position:
///
/// ```lisp
/// (defconstraint test () (if A (+ B C)))
/// ```
///
/// The purpose of this is to sanitize the structure of `if`
/// conditions to make their subsequent translation easier.
///
/// **NOTE:** the algorithm implemented here is not particular
/// efficient, and can result in unnecessary cloning of expressions.
fn raise_ifs(mut e: Node) -> Node {
    match e.e_mut() {
        Expression::Funcall { func, ref mut args } => {
            *args = args.iter_mut().map(|a| raise_ifs(a.clone())).collect();
            // This is a sanity check, though I'm not sure how it can
            // arise.
            assert!(args.iter().fold(true, |b,e| b&&!matches!(e.e(), Expression::Void)));
            //
            match func {
                Intrinsic::Add
                    | Intrinsic::Sub
                    | Intrinsic::Mul
                    | Intrinsic::VectorAdd
                    | Intrinsic::VectorSub
                    | Intrinsic::VectorMul => {    
                        for (i, a) in args.iter().enumerate() {
                            if let Expression::Funcall {
                                func: func_if @ (Intrinsic::IfZero | Intrinsic::IfNotZero),
                                args: args_if,
                            } = a.e()
                            {
                                let cond = args_if[0].clone();                                
                                // Pull out true-branch:
                                //   (func a b (if cond c d) e)
                                //   ==> (if cond (func a b c e))
                                let mut then_args = args.clone();
                                then_args[i] = args_if[1].clone();
                                let new_then = func.call(&then_args).unwrap();
                                let mut new_args = vec![cond, new_then];
                                // Pull out false branch (if applicable):
                                //   (func a b (if cond then else) c)
                                //   ==> (if !cond (func a b d e))                                
                                if let Some(arg_else) = args_if.get(2).cloned() {
                                    let mut else_args = args.clone();
                                    else_args[i] = arg_else;
                                    new_args.push(func.call(&else_args).unwrap());
                                }
                                // Repeat this until ifs pulled out
                                // from all argument positions.
                                return raise_ifs(func_if.call(&new_args).unwrap().with_type(a.t()));
                            }
                        }
                        e
                    }
                Intrinsic::IfZero
                    | Intrinsic::IfNotZero
                    | Intrinsic::Neg
                    | Intrinsic::Inv
                    | Intrinsic::Normalize
                    | Intrinsic::Exp
                    | Intrinsic::Begin => e,
            }
        }
        Expression::List(xs) => {
            for x in xs.iter_mut() {
                *x = raise_ifs(x.clone());
            }
            e
        }
        _ => e,
    }
}

/// Responsible for lowering `if` expressions into a multiplication
/// over the normalised condition.  For example, this constraint:
///
/// ```lisp
/// (defconstraint test () (if A B))
/// ```
///
/// Would be compiled as follows:
///
/// ```
/// (1 - ~A) * B
/// ```
///
/// Where `~A` is the normalised values of `A` (i.e. is `0` when `A=0`
/// otherwise is `1`).
///
/// **NOTE:** When the `if` condition is a constant expression, then
/// it is evaluated at compile time and the entire `if` expression is
/// eliminated.
pub fn expand_ifs(cs: &mut ConstraintSet) {
    for c in cs.constraints.iter_mut() {
        if let Constraint::Vanishes { expr, .. } = c {
            *expr = Box::new(raise_ifs(*expr.clone()));
        }
    }
    for c in cs.constraints.iter_mut() {
        if let Constraint::Vanishes { expr: e, .. } = c {
            do_expand_ifs(e).unwrap();
        }
    }
}
