use eyre::*;
use std::collections::HashMap;

use super::parser::{AstNode, Token};
use crate::utils::*;

lazy_static::lazy_static! {
    pub static ref BUILTINS: HashMap<&'static str, Function> = maplit::hashmap!{
        "nth" => Function {
            name: "nth".into(),
            class: FunctionClass::Builtin(Builtin::Nth),
        },

        "for" => Function {
            name: "for".into(),
            class: FunctionClass::SpecialForm(Form::For),
        },


        // monadic
        "inv" => Function{
            name: "inv".into(),
            class: FunctionClass::Builtin(Builtin::Inv)
        },
        "neg" => Function{
            name: "neg".into(),
            class: FunctionClass::Builtin(Builtin::Neg)
        },

        // Dyadic
        "if-zero" => Function{
            name: "if-zero".into(),
            class: FunctionClass::Builtin(Builtin::IfZero),
        },

        "shift" => Function{
            name: "shift".into(),
            class: FunctionClass::Builtin(Builtin::Shift),
        },


        // polyadic
        "+" => Function {
            name: "+".into(),
            class: FunctionClass::Builtin(Builtin::Add)
        },
        "*" => Function {
            name: "*".into(),
            class: FunctionClass::Builtin(Builtin::Mul)
        },
        "-" => Function {
            name: "-".into(),
            class: FunctionClass::Builtin(Builtin::Sub)
        },

        "begin" => Function{name: "begin".into(), class: FunctionClass::Builtin(Builtin::Begin)},

        // Special form for now, see later if implementing map...
        "branch-if-zero" => Function {
            name:"branch-if-zero".into(),
            class: FunctionClass::Builtin(Builtin::BranchIfZero)
        },

        "branch-if-zero-else" => Function {
            name:"branch-if-zero-else".into(),
            class: FunctionClass::Builtin(Builtin::BranchIfZeroElse)
        },

        "branch-if-not-zero" => Function {
            name:"branch-if-not-zero".into(),
            class: FunctionClass::Builtin(Builtin::BranchIfNotZero)
        },

        "branch-if-not-zero-else" => Function {
            name:"branch-if-not-zero-else".into(),
            class: FunctionClass::Builtin(Builtin::BranchIfNotZeroElse)
        },
    };
}

// Form have a special treatment and do not evaluate all their arguments
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Form {
    For,
}

pub enum Arity {
    AtLeast(usize),
    AtMost(usize),
    Even,
    Odd,
    Monadic,
    Dyadic,
    Exactly(usize),
}
impl Arity {
    fn make_error(&self, l: usize) -> String {
        fn arg_count(x: usize) -> String {
            format!("{} argument{}", x, if x > 1 { "s" } else { "" })
        }
        match self {
            Arity::AtLeast(x) => format!("expected at least {}, but received {}", arg_count(*x), l),
            Arity::AtMost(x) => format!("expected at most {}, but received {}", arg_count(*x), l),
            Arity::Even => format!("expected an even numer of arguments, but received {}", l),
            Arity::Odd => format!("expected an odd numer of arguments, but received {}", l),
            Arity::Monadic => format!("expected {}, but received {}", arg_count(1), l),
            Arity::Dyadic => format!("expected {}, but received {}", arg_count(2), l),
            Arity::Exactly(x) => format!("expected {}, but received {}", arg_count(*x), l),
        }
    }

    fn validate(&self, l: usize) -> Result<()> {
        match self {
            Arity::AtLeast(x) => l >= *x,
            Arity::AtMost(x) => l <= *x,
            Arity::Even => l % 2 == 0,
            Arity::Odd => l % 2 == 1,
            Arity::Monadic => l == 1,
            Arity::Dyadic => l == 2,
            Arity::Exactly(x) => l == *x,
        }
        .then(|| ())
        .ok_or_else(|| eyre!(self.make_error(l)))
    }
}
pub trait FuncVerifier<T> {
    fn arity(&self) -> Arity;

    fn validate_types(&self, args: &[T]) -> Result<()>;

    fn validate_arity(&self, args: &[T]) -> Result<()> {
        self.arity().validate(args.len())
    }

    fn validate_args(&self, args: Vec<T>) -> Result<Vec<T>> {
        self.validate_arity(&args)
            .and_then(|_| self.validate_types(&args))
            .and(Ok(args))
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub class: FunctionClass,
}
#[derive(Debug, Clone)]
pub enum FunctionClass {
    UserDefined(Defined),
    SpecialForm(Form),
    Builtin(Builtin),
    Alias(String),
}

#[derive(Debug, Clone)]
pub struct Defined {
    pub args: Vec<String>,
    pub body: AstNode,
}
impl FuncVerifier<Constraint> for Defined {
    fn arity(&self) -> Arity {
        Arity::Exactly(self.args.len())
    }

    fn validate_types(&self, _args: &[Constraint]) -> Result<()> {
        Ok(())
    }
}

impl FuncVerifier<AstNode> for Form {
    fn arity(&self) -> Arity {
        match self {
            Form::For => Arity::Exactly(3),
        }
    }
    fn validate_types(&self, args: &[AstNode]) -> Result<()> {
        match self {
            Form::For => {
                if matches!(args[0].class, Token::Symbol(_))
                    && matches!(args[1].class, Token::Range(_))
                    && matches!(args[2].class, Token::Form { .. })
                {
                    Ok(())
                } else {
                    Err(eyre!(
                        "`{:?}` expects [SYMBOL VALUE] but received {:?}",
                        self,
                        args
                    ))
                }
            }
        }
    }
}

impl FuncVerifier<Constraint> for Builtin {
    fn arity(&self) -> Arity {
        match self {
            Builtin::Add => Arity::AtLeast(2),
            Builtin::Sub => Arity::AtLeast(2),
            Builtin::Mul => Arity::AtLeast(2),
            Builtin::Neg => Arity::Monadic,
            Builtin::Inv => Arity::Monadic,
            Builtin::IfZero => Arity::Dyadic,
            Builtin::Shift => Arity::Dyadic,
            Builtin::Begin => Arity::AtLeast(1),
            Builtin::BranchIfZero => Arity::Dyadic,
            Builtin::BranchIfZeroElse => Arity::Exactly(3),
            Builtin::BranchIfNotZero => Arity::Dyadic,
            Builtin::BranchIfNotZeroElse => Arity::Exactly(3),
            Builtin::Nth => Arity::Dyadic,
        }
    }
    fn validate_types(&self, args: &[Constraint]) -> Result<()> {
        match self {
            f @ (Builtin::Add | Builtin::Sub | Builtin::Mul | Builtin::IfZero) => {
                if args.iter().all(|a| !matches!(a, Constraint::List(_))) {
                    Ok(())
                } else {
                    Err(eyre!(
                        "`{:?}` expects scalar arguments but received a list",
                        f,
                    ))
                }
            }
            Builtin::Neg | Builtin::Inv => {
                if args.iter().all(|a| !matches!(a, Constraint::List(_))) {
                    Ok(())
                } else {
                    Err(eyre!(
                        "`{:?}` expects a scalar argument but received a list",
                        self
                    ))
                }
            }
            Builtin::Shift => {
                if matches!(args[0], Constraint::Column(_))
                    && matches!(args[1], Constraint::Const(x) if x != 0)
                {
                    Ok(())
                } else {
                    Err(eyre!(
                        "`{:?}` expects a COLUMN and a non-null INTEGER but received {:?}",
                        self,
                        args
                    ))
                }
            }
            Builtin::Nth => {
                if matches!(args[0], Constraint::ArrayColumn(..))
                    && matches!(args[1], Constraint::Const(x) if x >= 0)
                {
                    Ok(())
                } else {
                    Err(eyre!(
                        "`{:?}` expects [SYMBOL CONST] but received {:?}",
                        self,
                        args
                    ))
                }
            }
            Builtin::BranchIfZero | Builtin::BranchIfNotZero => {
                if !matches!(args[0], Constraint::List(_)) && matches!(args[1], Constraint::List(_))
                {
                    Ok(())
                } else {
                    Err(eyre!(
                        "`{:?}` expects scalar arguments but received a list",
                        self
                    ))
                }
            }
            Builtin::BranchIfZeroElse | Builtin::BranchIfNotZeroElse => {
                if !matches!(args[0], Constraint::List(_))
                    && matches!(args[1], Constraint::List(_))
                    && matches!(args[2], Constraint::List(_))
                {
                    Ok(())
                } else {
                    Err(eyre!(
                        "`{:?}` expects (SCALAR, LIST, LIST) but received {:?}",
                        self,
                        args
                    ))
                }
            }
            Builtin::Begin => Ok(()),
        }
    }
}
