#![allow(dead_code)]
use anyhow::*;
use serde::{Deserialize, Serialize};

use crate::errors::CompileError;

use super::parser::{AstNode, Token};
use super::{max_type, Expression, Magma, Node, Type};

/// A form is an applicable that operates directly on the AST
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Form {
    For,
    Let,
    Debug,
    Todo,
    Reduce,
}

/// A builtin is a regular applicable that acts on already reduced arguments
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Builtin {
    Len,
    Shift,
}
impl std::fmt::Display for Builtin {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Builtin::Len => "len",
                Builtin::Shift => "shift",
            }
        )
    }
}

/// An intrinsic is a function that can appear in the final compiled form
/// of an expression
#[derive(Debug, PartialEq, Eq, Clone, Copy, Serialize, Deserialize)]
pub enum Intrinsic {
    Add,
    Sub,
    Mul,
    Exp,
    Neg,
    Inv,
    Normalize,

    Begin,

    IfZero,
    IfNotZero,
}
impl Intrinsic {
    pub fn call(self, args: &[Node]) -> Result<Node> {
        self.validate_args(args)?;
        Ok(Node::from_expr(self.raw_call(args)))
    }

    pub fn raw_call(self, args: &[Node]) -> Expression {
        Expression::Funcall {
            func: self,
            args: args.to_owned(),
        }
    }

    pub fn typing(&self, argtype: &[Type]) -> Type {
        match self {
            Intrinsic::Inv => argtype[0],
            Intrinsic::Normalize => argtype[0].with_magma(Magma::Boolean),
            Intrinsic::Add | Intrinsic::Sub | Intrinsic::Neg => {
                // Boolean is a corner case, as it is not stable under these operations
                match max_type(argtype) {
                    Type::Scalar(Magma::Boolean) => Type::Scalar(Magma::Native),
                    Type::Column(Magma::Boolean) => Type::Column(Magma::Native),
                    x => x,
                }
            }
            Intrinsic::Exp => argtype[0],
            Intrinsic::Mul => argtype.iter().max().cloned().unwrap_or(Type::INFIMUM),
            Intrinsic::IfZero | Intrinsic::IfNotZero => {
                argtype[1].max(argtype.get(2).cloned().unwrap_or(Type::INFIMUM))
            }
            Intrinsic::Begin => Type::List(max_type(argtype).magma()),
        }
    }
}
impl std::fmt::Display for Intrinsic {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Intrinsic::Add => "+",
                Intrinsic::Sub => "-",
                Intrinsic::Mul => "*",
                Intrinsic::Exp => "^",
                Intrinsic::Neg => "-",
                Intrinsic::Inv => "inv",
                Intrinsic::Normalize => "~",
                Intrinsic::Begin => "begin",
                Intrinsic::IfZero => "if-zero",
                Intrinsic::IfNotZero => "if-not-zero",
            }
        )
    }
}

pub enum Arity {
    AtLeast(usize),
    Monadic,
    Dyadic,
    Exactly(usize),
    Between(usize, usize),
}
impl Arity {
    fn make_error(&self, l: usize) -> String {
        fn arg_count(x: usize) -> String {
            format!("{} argument{}", x, if x > 1 { "s" } else { "" })
        }
        match self {
            Arity::AtLeast(x) => format!("expected at least {}, but received {}", arg_count(*x), l),
            Arity::Monadic => format!("expected {}, but received {}", arg_count(1), l),
            Arity::Dyadic => format!("expected {}, but received {}", arg_count(2), l),
            Arity::Exactly(x) => format!("expected {}, but received {}", arg_count(*x), l),
            Arity::Between(x, y) => format!(
                "expected between {} and {}, but received {}",
                arg_count(*x),
                arg_count(*y),
                l
            ),
        }
    }

    fn validate(&self, l: usize) -> Result<()> {
        if match self {
            Arity::AtLeast(x) => l >= *x,
            Arity::Monadic => l == 1,
            Arity::Dyadic => l == 2,
            Arity::Exactly(x) => l == *x,
            Arity::Between(x, y) => l >= *x && l <= *y,
        } {
            Ok(())
        } else {
            bail!(self.make_error(l))
        }
    }
}
/// The `FuncVerifier` trait defines a function that can check that
/// it is called with valid arguments
pub trait FuncVerifier<T: Clone> {
    /// The arity of the function
    fn arity(&self) -> Arity;

    /// Returns `Ok(())` if the arguments are of correct arity; `Err` otherwise
    fn validate_arity(&self, args: &[T]) -> Result<()> {
        self.arity().validate(args.len())
    }

    /// Returns `Ok(())` if the arguments are of correct type; `Err` otherwise
    fn validate_types(&self, args: &[T]) -> Result<()>;

    /// Checks that the arguments are of correct arity and type
    fn validate_args(&self, args: &[T]) -> Result<()> {
        self.validate_arity(args)
            .and_then(|_| self.validate_types(args))
    }
}

impl FuncVerifier<Node> for Builtin {
    fn arity(&self) -> Arity {
        match self {
            Builtin::Len => Arity::Monadic,
            Builtin::Shift => Arity::Dyadic,
        }
    }

    fn validate_types(&self, args: &[Node]) -> Result<()> {
        let args_t = args.iter().map(|a| a.t()).collect::<Vec<_>>();
        let expected_t: &[&[Type]] = match self {
            Builtin::Len => &[&[Type::ArrayColumn(Magma::Any)]],
            Builtin::Shift => &[&[Type::Column(Magma::Any)], &[Type::Scalar(Magma::Any)]],
        };

        if super::compatible_with_repeating(expected_t, &args_t) {
            Ok(())
        } else {
            bail!(CompileError::TypeError(
                self.to_string(),
                expected_t,
                args_t
            ))
        }
    }
}

impl FuncVerifier<AstNode> for Form {
    fn arity(&self) -> Arity {
        match self {
            Form::For => Arity::Exactly(3),
            Form::Debug => Arity::AtLeast(1),
            Form::Todo => Arity::AtLeast(0),
            Form::Let => Arity::Dyadic,
            Form::Reduce => Arity::Dyadic,
        }
    }
    fn validate_types(&self, args: &[AstNode]) -> Result<()> {
        match self {
            Form::For => {
                if matches!(args[0].class, Token::Symbol(_)) {
                    Ok(())
                } else {
                    bail!(
                        "`{:?}` expects [SYMBOL ITERABLE EXPR] but received {:?}",
                        self,
                        args
                    )
                }
            }
            Form::Debug => Ok(()),
            Form::Todo => Ok(()),
            Form::Let => {
                if let Result::Ok(pairs) = args[0].as_list() {
                    for pair in pairs {
                        if let Result::Ok(pair) = pair.as_list() {
                            if !(pair.len() == 2 && matches!(pair[0].class, Token::Symbol(_))) {
                                bail!("LET expects a pair of bindings, found `{:?}`", pair)
                            }
                        } else {
                            bail!("LET expects a pair of bindings, found `{:?}`", pair)
                        }
                    }
                    Ok(())
                } else {
                    bail!("LET expects a list of bindings, found `{:?}`", args[0])
                }
            }
            Form::Reduce => {
                if args[0].as_symbol().is_err() {
                    bail!("REDUCE expects a symbol, found `{:?}`", args[0])
                }
                Ok(())
            }
        }
    }
}
