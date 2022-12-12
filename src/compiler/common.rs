#![allow(dead_code)]
use anyhow::*;
use serde::{Deserialize, Serialize};
use std::cmp::Ordering;
use std::collections::HashMap;

use super::generator::{Builtin, Function, FunctionClass};
use super::parser::{AstNode, Token};

const MODULE_SEPARATOR: &str = "__";
const ARRAY_SEPARATOR: &str = "_";

lazy_static::lazy_static! {
    pub static ref BUILTINS: HashMap<&'static str, Function> = maplit::hashmap!{
        "nth" => Function {
            handle: Handle::new(super::MAIN_MODULE, "nth"),
            class: FunctionClass::Builtin(Builtin::Nth),
        },

        "for" => Function {
            handle: Handle::new(super::MAIN_MODULE, "for"),
            class: FunctionClass::SpecialForm(Form::For),
        },

        "debug" => Function {
            handle: Handle::new(super::MAIN_MODULE, "debug"),
            class: FunctionClass::SpecialForm(Form::Debug),
        },


        // monadic
        "inv" => Function {
            handle: Handle::new(super::MAIN_MODULE, "inv"),
            class: FunctionClass::Builtin(Builtin::Inv)
        },
        "neg" => Function {
            handle: Handle::new(super::MAIN_MODULE, "neg"),
            class: FunctionClass::Builtin(Builtin::Neg)
        },
        "not" => Function {
            handle: Handle::new(super::MAIN_MODULE, "not"),
            class: FunctionClass::Builtin(Builtin::Not),
        },

        // Dyadic
        "eq" => Function{
            handle: Handle::new(super::MAIN_MODULE, "eq"),
            class: FunctionClass::Builtin(Builtin::Eq),
        },
        "shift" => Function{
            handle: Handle::new(super::MAIN_MODULE, "shift"),
            class: FunctionClass::Builtin(Builtin::Shift),
        },


        // polyadic
        "+" => Function {
            handle: Handle::new(super::MAIN_MODULE, "+"),
            class: FunctionClass::Builtin(Builtin::Add)
        },
        "*" => Function {
            handle: Handle::new(super::MAIN_MODULE, "*"),
            class: FunctionClass::Builtin(Builtin::Mul)
        },
        "^" => Function {
            handle: Handle::new(super::MAIN_MODULE, "^"),
            class: FunctionClass::Builtin(Builtin::Exp)
        },
        "-" => Function {
            handle: Handle::new(super::MAIN_MODULE, "-"),
            class: FunctionClass::Builtin(Builtin::Sub)
        },

        "begin" => Function{
            handle: Handle::new(super::MAIN_MODULE, "begin"),
            class: FunctionClass::Builtin(Builtin::Begin)
        },

        "if-zero" => Function {
            handle: Handle::new(super::MAIN_MODULE, "if-zero"),
            class: FunctionClass::Builtin(Builtin::IfZero)
        },
        "if-not-zero" => Function {
            handle: Handle::new(super::MAIN_MODULE, "if-not-zero"),
            class: FunctionClass::Builtin(Builtin::IfNotZero)
        },

        "make-decomposition" => Function {
            handle: Handle::new(super::MAIN_MODULE, "make-decomposition"),
            class: FunctionClass::Builtin(Builtin::ByteDecomposition)
        }
    };
}

// Form have a special treatment and do not evaluate all their arguments
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Form {
    For,
    Debug,
}

pub enum Arity {
    AtLeast(usize),
    // AtMost(usize),
    // Even,
    // Odd,
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
            // Arity::AtMost(x) => format!("expected at most {}, but received {}", arg_count(*x), l),
            // Arity::Even => format!("expected an even numer of arguments, but received {}", l),
            // Arity::Odd => format!("expected an odd numer of arguments, but received {}", l),
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
        match self {
            Arity::AtLeast(x) => l >= *x,
            // Arity::AtMost(x) => l <= *x,
            // Arity::Even => l % 2 == 0,
            // Arity::Odd => l % 2 == 1,
            Arity::Monadic => l == 1,
            Arity::Dyadic => l == 2,
            Arity::Exactly(x) => l == *x,
            Arity::Between(x, y) => l >= *x && l <= *y,
        }
        .then(|| ())
        .ok_or_else(|| anyhow!(self.make_error(l)))
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

impl FuncVerifier<AstNode> for Form {
    fn arity(&self) -> Arity {
        match self {
            Form::For => Arity::Exactly(3),
            Form::Debug => Arity::AtLeast(1),
        }
    }
    fn validate_types(&self, args: &[AstNode]) -> Result<()> {
        match self {
            Form::For => {
                if matches!(args[0].class, Token::Symbol(_))
                    && matches!(args[1].class, Token::Range(_))
                    && matches!(args[2].class, Token::List { .. })
                {
                    Ok(())
                } else {
                    Err(anyhow!(
                        "`{:?}` expects [SYMBOL VALUE] but received {:?}",
                        self,
                        args
                    ))
                }
            }
            Form::Debug => Ok(()),
        }
    }
}

/// The type of a column in the IR
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum Type {
    Void,
    Scalar(Magma),
    Column(Magma),
    List(Magma),
}
impl Type {
    pub const SUPREMUM: Self = Type::Column(Magma::SUPREMUM);
    pub const INFIMUM: Self = Type::Void;

    pub fn magma(self) -> Magma {
        match self {
            Type::Void => todo!(),
            Type::Scalar(m) => m,
            Type::Column(m) => m,
            Type::List(m) => m,
        }
    }

    pub fn same_scale(&self, new: Magma) -> Self {
        match self {
            Type::Void => todo!(),
            Type::Scalar(_) => Type::Scalar(new),
            Type::Column(_) => Type::Column(new),
            Type::List(_) => Type::List(new),
        }
    }
    pub fn is_bool(&self) -> bool {
        match self {
            Type::Void | Type::List(_) => false,
            Type::Column(x) => matches!(x, Magma::Boolean),
            Type::Scalar(x) => matches!(x, Magma::Boolean),
        }
    }
    pub fn is_scalar(&self) -> bool {
        matches!(self, Type::Scalar(_))
    }
    pub fn is_column(&self) -> bool {
        matches!(self, Type::Column(_))
    }
    pub fn is_value(&self) -> bool {
        self.is_scalar() || self.is_column()
    }
    pub fn min(&self, other: &Self) -> Self {
        other.max(self)
    }
    pub fn max(&self, other: &Self) -> Self {
        match (self, other) {
            (Type::Void, Type::Void) => Type::Void,
            (Type::Void, _) => *other,
            (_, Type::Void) => *self,
            (Type::Column(x), Type::Column(y)) => Type::Column(x.max(y).to_owned()),
            (Type::Column(x), Type::Scalar(y)) => Type::Column(x.max(y).to_owned()),
            (Type::Scalar(x), Type::Column(y)) => Type::Column(x.max(y).to_owned()),
            (Type::Scalar(x), Type::Scalar(y)) => Type::Scalar(x.max(y).to_owned()),
            (Type::Scalar(x), Type::List(y)) => Type::List(x.max(y).to_owned()),
            (Type::Column(x), Type::List(y)) => Type::List(x.max(y).to_owned()),
            (Type::List(_), Type::Scalar(_)) => todo!(),
            (Type::List(x), Type::Column(y)) => Type::List(x.max(y).to_owned()),
            (Type::List(x), Type::List(y)) => Type::List(x.max(y).to_owned()),
        }
    }
}
impl std::cmp::PartialOrd for Type {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (Type::Void, Type::Void) => Some(Ordering::Equal),
            (Type::Void, _) => Some(Ordering::Less),
            (Type::Column(_), Type::Void) => Some(Ordering::Greater),
            (Type::Column(x), Type::Column(y)) => Some(x.cmp(y)),
            (Type::Column(_), Type::Scalar(_)) => None,
            (Type::Scalar(_), Type::Void) => Some(Ordering::Greater),
            (Type::Scalar(_), Type::Column(_)) => None,
            (Type::Scalar(x), Type::Scalar(y)) => Some(x.cmp(y)),
            _ => unimplemented!(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum Magma {
    Boolean,
    Nibble,
    Integer,
}
impl std::cmp::Ord for Magma {
    fn cmp(&self, other: &Self) -> Ordering {
        self.partial_cmp(other).unwrap()
    }
}
impl std::cmp::PartialOrd for Magma {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (Magma::Boolean, Magma::Boolean) => Some(Ordering::Equal),
            (Magma::Boolean, Magma::Nibble) => Some(Ordering::Less),
            (Magma::Boolean, Magma::Integer) => Some(Ordering::Less),
            (Magma::Nibble, Magma::Boolean) => Some(Ordering::Greater),
            (Magma::Nibble, Magma::Nibble) => Some(Ordering::Equal),
            (Magma::Nibble, Magma::Integer) => Some(Ordering::Less),
            (Magma::Integer, Magma::Boolean) => Some(Ordering::Greater),
            (Magma::Integer, Magma::Nibble) => Some(Ordering::Greater),
            (Magma::Integer, Magma::Integer) => Some(Ordering::Equal),
        }
    }
}
impl Magma {
    const SUPREMUM: Self = Magma::Integer;
}

#[derive(Clone, Eq, Serialize, Deserialize)]
pub struct Handle {
    pub module: String,
    pub name: String,
    pub id: Option<usize>,
}
impl std::cmp::PartialEq for Handle {
    fn eq(&self, other: &Self) -> bool {
        (self.module == other.module) && (self.name == other.name)
    }
}
impl std::hash::Hash for Handle {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.module.hash(state);
        self.id.hash(state);
    }
}
impl Handle {
    pub fn new<S1: AsRef<str>, S2: AsRef<str>>(module: S1, name: S2) -> Self {
        Handle {
            module: module.as_ref().to_owned(),
            name: name.as_ref().to_owned(),
            id: None,
        }
    }

    pub fn with_id<S1: AsRef<str>, S2: AsRef<str>>(module: S1, name: S2, id: usize) -> Self {
        Handle {
            module: module.as_ref().to_owned(),
            name: name.as_ref().to_owned(),
            id: Some(id),
        }
    }

    pub fn set_id(&mut self, i: usize) {
        self.id = Some(i)
    }

    pub fn ith(&self, i: usize) -> Handle {
        Handle {
            module: self.module.clone(),
            name: format!("{}{}{}", self.name, ARRAY_SEPARATOR, i),
            id: self.id,
        }
    }

    fn purify(s: &str) -> String {
        s.replace('(', "_")
            .replace(')', "_")
            .replace('{', "_")
            .replace('}', "_")
            .replace('[', "_")
            .replace(']', "_")
            .replace('<', "")
            .replace('>', "")
            .replace(':', "_")
            .replace('%', "_")
            .replace('.', "_")
            .replace('-', "_")
            .replace('*', "mul_")
            .replace('+', "add_")
            .replace('/', "div_")
            .replace(|c: char| !c.is_ascii(), "_")
    }

    pub fn mangle(&self) -> String {
        let r = format!(
            "{}{}{}",
            Self::purify(&self.module),
            if self.module.is_empty() {
                ""
            } else {
                MODULE_SEPARATOR
            },
            Self::purify(&self.name)
        );
        if r.len() < 100 {
            r
        } else {
            format!("H{:?}", md5::compute(r))
        }
    }

    pub fn mangled_name(&self) -> String {
        Self::purify(&self.name)
    }

    pub fn mangled_module(&self) -> String {
        Self::purify(&self.module)
    }
}
impl std::fmt::Debug for Handle {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}.{}", self.module, self.name)
    }
}
impl std::fmt::Display for Handle {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}.{}", self.module, self.name)
    }
}
