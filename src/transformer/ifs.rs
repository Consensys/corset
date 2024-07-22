use anyhow::Result;
use num_traits::Zero;

use crate::compiler::{Constraint, ConstraintSet, Expression, Intrinsic, Node};

use super::flatten_list;

/// Expand if conditions, assuming they are roughly in "top-most"
/// positions.  That is, we can have arbitrary nested if `List` and
/// `IfZero` / `IfNotZero` but nothing else.  The simplest example is
/// something like this:
///
/// ```
/// (if (vanishes! A) B C)
/// ```
///
/// Which is translated into a list of two constraints:
///
/// ```
/// {
///  (1 - NORM(A)) * B
///  A * C
/// }
/// ```
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
                    // Construct condition for then branch, and
                    // condition for else branch.
                    let conds = {
                        // Multiplier for if-non-zero branch.
                        let cond_not_zero = cond.clone();
                        // Multiplier for if-zero branch.
                        let cond_zero = Intrinsic::Sub.unchecked_call(&[
                            Node::one(),
                            Intrinsic::Normalize.unchecked_call(&[cond.clone()])?,
                        ])?;
                        // Set ordering based on function itself.
                        if if_not_zero {
                            [cond_not_zero, cond_zero]
                        } else {
                            [cond_zero, cond_not_zero]
                        }
                    };
                    // Apply condition to body.
                    let then_else: Node = match (args.get(1), args.get(2)) {
                        (Some(e), None) => {
                            let then_cond = conds[0].clone();
                            Intrinsic::Mul
                                .unchecked_call(&[then_cond, e.clone()])
                                .unwrap()
                        }
                        (None, Some(e)) => {
                            let else_cond = conds[1].clone();
                            Intrinsic::Mul
                                .unchecked_call(&[else_cond, e.clone()])
                                .unwrap()
                        }
                        (_, _) => unreachable!(),
                    };
                    // Finally, replace existing node.
                    *e = then_else.clone();
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
            assert!(args
                .iter()
                .fold(true, |b, e| b && !matches!(e.e(), Expression::Void)));
            //
            match func {
                Intrinsic::Neg
                | Intrinsic::Inv
                | Intrinsic::Normalize
                | Intrinsic::Exp
                | Intrinsic::Add
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
                            let new_then = func.unchecked_call(&then_args).unwrap();
                            let mut new_args = vec![cond, new_then];
                            // Pull out false branch (if applicable):
                            //   (func a b (if cond c d) e)
                            //   ==> (if !cond (func a b d e))
                            if let Some(arg_else) = args_if.get(2).cloned() {
                                let mut else_args = args.clone();
                                else_args[i] = arg_else;
                                new_args.push(func.unchecked_call(&else_args).unwrap());
                            }
                            // Repeat this until ifs pulled out
                            // from all argument positions.
                            return raise_ifs(
                                func_if.unchecked_call(&new_args).unwrap().with_type(a.t()),
                            );
                        }
                    }
                    e
                }
                Intrinsic::IfZero | Intrinsic::IfNotZero | Intrinsic::Begin => e,
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

/// Pull `lists` out of nested positions and into top-most
/// positions.  Specifically, something like this:
///
/// ```lisp
/// (defconstraint test () (if A (begin B C)))
/// ```
///
/// Has the nested `list` raised into the following position:
///
/// ```lisp
/// (defconstraint test () (begin (if A B) (if A C)))
/// ```
///
/// The purpose of this is to sanitize the structure of expressions
/// conditions to make their subsequent translation easier.
fn raise_lists(node: &Node) -> Vec<Node> {
    match node.e() {
        Expression::List(xs) => {
            let mut exprs = Vec::new();
            for x in xs {
                exprs.extend(raise_lists(x));
            }
            exprs
        }
        Expression::Funcall { func, args } if args.len() > 0 => {
            match func {
                Intrinsic::IfZero if args.len() > 2 => {
                    let mut out = Vec::new();
                    // if-then
                    raise_binary(&args[0], &args[1], func, &mut out);
                    // if-else
                    raise_binary(&args[0], &args[2], &Intrinsic::IfNotZero, &mut out);
                    // done
                    out
                }
                Intrinsic::IfNotZero if args.len() > 2 => {
                    let mut out = Vec::new();
                    // if-then
                    raise_binary(&args[0], &args[1], func, &mut out);
                    // if-else
                    raise_binary(&args[0], &args[2], &Intrinsic::IfZero, &mut out);
                    // done
                    out
                }
                Intrinsic::Begin => unreachable!(),
                _ => {
                    // More challenging because we have to compute the cross
                    // product.
                    let mut out = Vec::new();
                    raise_intrinsic(args, func, &mut out, &mut Vec::new());
                    out
                }
            }
        }
        _ => vec![node.clone()],
    }
}

/// Enumerate all atomic invocations of this intrinsic by expanding
/// the cross-product of all arguments.  To understand this, consider:
///
/// ```lisp
/// (* (begin A B) (begin X Y))
/// ```
///
/// This is considered "non-atomic" because it contains lists within.
/// This is expanded into the following distinct invocations:
///
/// ```lisp
/// (* A X)
/// (* B X)
/// (* A Y)
/// (* B Y)
/// ```
///
/// This method is responsible for enumerating the argument
/// combinations.
fn raise_intrinsic(args: &[Node], f: &Intrinsic, out: &mut Vec<Node>, acc: &mut Vec<Node>) {
    let n = acc.len();
    //
    if n == args.len() {
        out.push(Node::from_expr(f.raw_call(acc)));
    } else {
        // Raise nth expression
        let raised_args = raise_lists(&args[n]);
        // Continue
        for e in raised_args {
            acc.push(e);
            raise_intrinsic(args, f, out, acc);
            acc.pop();
        }
    }
    // Done
}

/// Special case of `raise_intrinsic` for binary operands.
fn raise_binary(lhs: &Node, rhs: &Node, f: &Intrinsic, out: &mut Vec<Node>) {
    let raised_lhs = raise_lists(lhs);
    let raised_rhs = raise_lists(rhs);
    // Simple cross product
    for l in raised_lhs {
        for r in &raised_rhs {
            let l_r_expr = f.raw_call(&[l.clone(), r.clone()]);
            out.push(Node::from_expr(l_r_expr));
        }
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
/// (1 - NORM(A)) * B
/// ```
///
/// Where `NORM(A)` is the normalised values of `A` (i.e. is `0` when
/// `A=0` otherwise is `1`).
///
/// **NOTE:** When the `if` condition is a constant expression, then
/// it is evaluated at compile time and the entire `if` expression is
/// eliminated.
pub fn expand_ifs(cs: &mut ConstraintSet) {
    // Raise lists
    for c in cs.constraints.iter_mut() {
        if let Constraint::Vanishes { expr, .. } = c {
            let mut exprs = raise_lists(&*expr);
            // Construct new expression
            let nexpr = if exprs.len() == 1 {
                // Optimise case where only a single expression, as we
                // don't need a list in this case.
                exprs.pop().unwrap()
            } else {
                // When there are multiple expressions, use a list.
                Node::from_expr(Expression::List(exprs))
            };
            // Replace old expression with new
            *expr = Box::new(nexpr);
        }
    }
    // Raise ifs
    for c in cs.constraints.iter_mut() {
        if let Constraint::Vanishes { expr, .. } = c {
            let nexpr = raise_ifs(*expr.clone());
            // Replace old expression with new
            *expr = Box::new(nexpr);
        }
    }
    for c in cs.constraints.iter_mut() {
        if let Constraint::Vanishes { expr: e, .. } = c {
            do_expand_ifs(e).unwrap();
        }
    }
}
