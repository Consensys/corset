#![allow(dead_code)]
use anyhow::*;
use serde::{Deserialize, Serialize};
use std::cmp::Ordering;

use crate::{column::Value, errors::RuntimeError};

pub fn max_type<'a, TS: IntoIterator<Item = &'a Type>>(ts: TS) -> Type {
    ts.into_iter().fold(Type::INFIMUM, |a, b| a.max(*b))
}

/// The type of a column in the IR. This struct contains both the dimensionality
/// of the type and its underlying magma.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum Type {
    Void,
    Scalar(Magma),
    Column(Magma),
    Any(Magma),
    ArrayColumn(Magma),
    List(Magma),
}
impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Type::Void => write!(f, "∅"),
            Type::Scalar(m) => write!(f, "{}", m),
            Type::Column(m) => write!(f, "[{}]", m),
            Type::Any(m) => write!(f, "∋{}", m),
            Type::ArrayColumn(m) => write!(f, "[[{}]]", m),
            Type::List(m) => write!(f, "{{{}}}", m),
        }
    }
}
impl Type {
    pub const INFIMUM: Self = Type::Void;

    pub fn c(self) -> Conditioning {
        match self {
            Type::Void => Conditioning::None,
            Type::Scalar(m)
            | Type::Column(m)
            | Type::Any(m)
            | Type::ArrayColumn(m)
            | Type::List(m) => m.c(),
        }
    }

    pub fn m(self) -> Magma {
        match self {
            Type::Void => RawMagma::None.into(),
            Type::Scalar(m)
            | Type::Column(m)
            | Type::Any(m)
            | Type::ArrayColumn(m)
            | Type::List(m) => m,
        }
    }

    pub fn rm(self) -> RawMagma {
        match self {
            Type::Void => RawMagma::None.into(),
            Type::Scalar(m)
            | Type::Column(m)
            | Type::Any(m)
            | Type::ArrayColumn(m)
            | Type::List(m) => m.rm(),
        }
    }

    pub fn invert(&self) -> Type {
        self.with_raw_magma(self.m().invert().rm())
    }

    pub fn with_conditioning(&self, c: Conditioning) -> Self {
        self.with_magma(self.m().with_conditioning(c))
    }

    pub fn with_magma(&self, m: Magma) -> Self {
        match self {
            Type::Void => todo!(),
            Type::Scalar(_) => Type::Scalar(m),
            Type::Column(_) => Type::Column(m),
            Type::Any(_) => Type::Any(m),
            Type::ArrayColumn(_) => Type::ArrayColumn(m),
            Type::List(_) => Type::List(m),
        }
    }

    pub fn with_raw_magma(&self, new: RawMagma) -> Self {
        let m = self.m().with_raw_magma(new);
        match self {
            Type::Void => todo!(),
            Type::Scalar(_) => Type::Scalar(m),
            Type::Column(_) => Type::Column(m),
            Type::Any(_) => Type::Any(m),
            Type::ArrayColumn(_) => Type::ArrayColumn(m),
            Type::List(_) => Type::List(m),
        }
    }

    pub fn with_scale(&self, new: Type) -> Self {
        let magma = self.m();
        match new {
            Type::Void => Type::Void,
            Type::Scalar(_) => Type::Scalar(magma),
            Type::Column(_) => Type::Column(magma),
            Type::Any(_) => Type::Any(magma),
            Type::ArrayColumn(_) => Type::ArrayColumn(magma),
            Type::List(_) => Type::List(magma),
        }
    }

    pub fn is_binary(&self) -> bool {
        match self {
            Type::Void | Type::List(_) | Type::ArrayColumn(_) => false,
            Type::Column(x) | Type::Scalar(x) | Type::Any(x) => x.is_binary(),
        }
    }

    pub fn is_conditioned(&self) -> bool {
        self.m().is_conditioned()
    }

    pub fn can_cast_to(&self, other: Type) -> bool {
        match (self, other) {
            (Type::Void, Type::Void) => true,
            (Type::Void, _) => false,

            (Type::Scalar(_), Type::Void) => false,
            (Type::Scalar(x), Type::Scalar(y))
            | (Type::Scalar(x), Type::Column(y))
            | (Type::Scalar(x), Type::Any(y)) => x.can_cast_to(y),
            (Type::Scalar(_), Type::ArrayColumn(_)) | (Type::Scalar(_), Type::List(_)) => false,

            (Type::Column(x), Type::Any(y)) | (Type::Column(x), Type::Column(y)) => {
                x.can_cast_to(y)
            }
            (Type::Column(_), Type::Void)
            | (Type::Column(_), Type::Scalar(_))
            | (Type::Column(_), Type::ArrayColumn(_))
            | (Type::Column(_), Type::List(_)) => false,

            (Type::Any(x), Type::Any(y)) => x.can_cast_to(y),
            (Type::Any(_), Type::Void)
            | (Type::Any(_), Type::Scalar(_))
            | (Type::Any(_), Type::Column(_))
            | (Type::Any(_), Type::ArrayColumn(_))
            | (Type::Any(_), Type::List(_)) => false,

            (Type::ArrayColumn(x), Type::ArrayColumn(y)) => x.can_cast_to(y),
            (Type::ArrayColumn(_), _) => false,

            (Type::List(x), Type::List(y)) => x.can_cast_to(y),
            (Type::List(_), Type::Void)
            | (Type::List(_), Type::Scalar(_))
            | (Type::List(_), Type::Column(_))
            | (Type::List(_), Type::Any(_))
            | (Type::List(_), Type::ArrayColumn(_)) => false,
        }
    }
}
impl std::cmp::PartialOrd for Type {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}
impl std::cmp::Ord for Type {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            (Type::Void, Type::Void) => Ordering::Equal,
            (Type::Void, _) => Ordering::Less,
            (s, o) => {
                if s.m() == o.m() {
                    match (s, o) {
                        (Type::Scalar(_), Type::Void) => Ordering::Greater,
                        (Type::Scalar(_), Type::Scalar(_)) => Ordering::Equal,
                        (Type::Scalar(_), Type::Column(_)) => Ordering::Less,
                        (Type::Scalar(_), Type::Any(_)) => Ordering::Equal,
                        (Type::Scalar(_), Type::ArrayColumn(_)) => todo!(),
                        (Type::Scalar(_), Type::List(_)) => todo!(),
                        (Type::Column(_), Type::Void) => Ordering::Greater,
                        (Type::Column(_), Type::Scalar(_)) => Ordering::Greater,
                        (Type::Column(_), Type::Column(_)) => Ordering::Equal,
                        (Type::Column(_), Type::Any(_)) => Ordering::Equal,
                        (Type::Column(_), Type::ArrayColumn(_)) => todo!(),
                        (Type::Column(_), Type::List(_)) => todo!(),
                        (Type::Any(_), Type::Void) => Ordering::Greater,
                        (Type::Any(_), Type::Scalar(_)) => Ordering::Equal,
                        (Type::Any(_), Type::Column(_)) => Ordering::Equal,
                        (Type::Any(_), Type::Any(_)) => Ordering::Equal,
                        (Type::Any(_), Type::ArrayColumn(_)) => todo!(),
                        (Type::Any(_), Type::List(_)) => todo!(),
                        (Type::ArrayColumn(_), Type::Void) => todo!(),
                        (Type::ArrayColumn(_), Type::Scalar(_)) => todo!(),
                        (Type::ArrayColumn(_), Type::Column(_)) => todo!(),
                        (Type::ArrayColumn(_), Type::Any(_)) => todo!(),
                        (Type::ArrayColumn(_), Type::ArrayColumn(_)) => todo!(),
                        (Type::ArrayColumn(_), Type::List(_)) => todo!(),
                        (Type::List(_), Type::Void) => todo!(),
                        (Type::List(_), Type::Scalar(_)) => todo!(),
                        (Type::List(_), Type::Column(_)) => todo!(),
                        (Type::List(_), Type::Any(_)) => todo!(),
                        (Type::List(_), Type::ArrayColumn(_)) => todo!(),
                        (Type::List(_), Type::List(_)) => todo!(),
                        _ => unreachable!(),
                    }
                } else {
                    s.m().cmp(&o.m())
                }
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, Hash, PartialOrd)]
pub enum Conditioning {
    None,
    Boolean,
    Loobean,
}
impl std::cmp::Ord for Conditioning {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            (Conditioning::None, Conditioning::None) => Ordering::Equal,
            (Conditioning::None, Conditioning::Boolean) => Ordering::Less,
            (Conditioning::None, Conditioning::Loobean) => Ordering::Less,
            (Conditioning::Boolean, Conditioning::None) => Ordering::Greater,
            (Conditioning::Boolean, Conditioning::Boolean) => Ordering::Equal,
            (Conditioning::Boolean, Conditioning::Loobean) => unreachable!(),
            (Conditioning::Loobean, Conditioning::None) => Ordering::Greater,
            (Conditioning::Loobean, Conditioning::Boolean) => unreachable!(),
            (Conditioning::Loobean, Conditioning::Loobean) => Ordering::Equal,
        }
    }
}
impl std::fmt::Display for Conditioning {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Conditioning::None => "∅",
                Conditioning::Boolean => "𝔹",
                Conditioning::Loobean => "𝕃",
            }
        )
    }
}

lazy_static::lazy_static! {
    static ref F_15: Value = Value::from(15);
    static ref F_255: Value = Value::from(255);
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub enum RawMagma {
    None,
    Binary,
    /// 4-bits
    Nibble,
    /// 8-bits
    Byte,
    /// a field element
    Native,
    /// an arbitrary long integer
    Integer(usize),
    /// Anything
    Any,
}
impl RawMagma {
    fn bit_size(&self) -> usize {
        match self {
            RawMagma::None => 0,
            RawMagma::Binary => 1,
            RawMagma::Nibble => 4,
            RawMagma::Byte => 8,
            RawMagma::Native => crate::constants::FIELD_BITSIZE,
            RawMagma::Integer(x) => *x,
            RawMagma::Any => crate::constants::FIELD_BITSIZE,
        }
    }

    fn byte_size(&self) -> usize {
        let bit_size = self.bit_size();
        (bit_size + 8 - 1) / 8
    }

    pub fn validate(&self, x: Value) -> Result<Value> {
        match self {
            RawMagma::None => unreachable!(),
            RawMagma::Binary => {
                if x.is_zero() || x.is_one() {
                    Ok(x)
                } else {
                    bail!(RuntimeError::InvalidValue("bool", x))
                }
            }
            RawMagma::Nibble => {
                if x.le(&F_15) {
                    Ok(x)
                } else {
                    bail!(RuntimeError::InvalidValue("nibble", x))
                }
            }
            RawMagma::Byte => {
                if x.le(&F_255) {
                    Ok(x)
                } else {
                    bail!(RuntimeError::InvalidValue("byte", x))
                }
            }
            RawMagma::Native => Ok(x),
            RawMagma::Integer(b) => {
                if x.bit_size() > *b {
                    bail!(RuntimeError::InvalidValue("integer", x))
                } else {
                    Ok(x)
                }
            }
            RawMagma::Any => unreachable!(),
        }
    }
}

/// [ill-named] A magma is a set where some operations stay within itself.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub struct Magma {
    m: RawMagma,
    c: Conditioning,
}
impl From<RawMagma> for Magma {
    fn from(m: RawMagma) -> Magma {
        Magma {
            m,
            c: Conditioning::None,
        }
    }
}
impl Magma {
    // the dominant instantiable magma
    const SUPREMUM: Self = Magma {
        m: RawMagma::Native,
        c: Conditioning::None,
    };

    pub const BINARY: Self = Magma {
        m: RawMagma::Binary,
        c: Conditioning::None,
    };

    pub const NIBBLE: Self = Magma {
        m: RawMagma::Nibble,
        c: Conditioning::None,
    };

    pub const BYTE: Self = Magma {
        m: RawMagma::Byte,
        c: Conditioning::None,
    };

    pub const NATIVE: Self = Magma {
        m: RawMagma::Native,
        c: Conditioning::None,
    };

    pub const ANY: Self = Magma {
        m: RawMagma::Any,
        c: Conditioning::None,
    };

    pub fn is_binary(&self) -> bool {
        matches!(self.m, RawMagma::Binary)
    }

    pub fn bit_size(&self) -> usize {
        self.m.bit_size()
    }

    pub fn invert(&self) -> Magma {
        Magma {
            m: match self.m {
                RawMagma::None => unreachable!(),
                RawMagma::Integer(_) => unreachable!(),
                RawMagma::Any => unreachable!(),
                RawMagma::Binary => RawMagma::Binary,
                _ => RawMagma::Native,
            },
            c: self.c,
        }
    }

    fn can_cast_to(&self, other: Magma) -> bool {
        match (self.m, other.m) {
            (RawMagma::None, RawMagma::None) => true,
            (RawMagma::None, _) => false,

            (RawMagma::Binary, RawMagma::None) => false,
            (RawMagma::Binary, _) => true,

            (RawMagma::Nibble, RawMagma::None) | (RawMagma::Nibble, RawMagma::Binary) => false,
            (RawMagma::Nibble, RawMagma::Nibble)
            | (RawMagma::Nibble, RawMagma::Byte)
            | (RawMagma::Nibble, RawMagma::Native) => true,

            (RawMagma::Byte, RawMagma::None)
            | (RawMagma::Byte, RawMagma::Binary)
            | (RawMagma::Byte, RawMagma::Nibble) => false,
            (RawMagma::Byte, RawMagma::Byte) | (RawMagma::Byte, RawMagma::Native) => true,

            (RawMagma::Native, RawMagma::None)
            | (RawMagma::Native, RawMagma::Binary)
            | (RawMagma::Native, RawMagma::Nibble)
            | (RawMagma::Native, RawMagma::Byte) => false,
            (RawMagma::Native, RawMagma::Native) => true,
            (RawMagma::Native, RawMagma::Any) => true,

            (RawMagma::Integer(_), RawMagma::None) => false,
            (RawMagma::Integer(_), RawMagma::Binary) => true,
            (RawMagma::Integer(_), RawMagma::Nibble)
            | (RawMagma::Integer(_), RawMagma::Byte)
            | (RawMagma::Integer(_), RawMagma::Native)
            | (RawMagma::Integer(_), RawMagma::Integer(_)) => {
                self.m.bit_size() <= other.m.bit_size()
            }

            (_, RawMagma::Integer(_)) => self.m.bit_size() >= other.m.bit_size(),

            (RawMagma::Any, _) => false,
            (_, RawMagma::Any) => true,
        }
    }

    pub fn rm(&self) -> RawMagma {
        self.m
    }
    pub fn c(&self) -> Conditioning {
        self.c
    }
    pub fn is_conditioned(&self) -> bool {
        !matches!(self.c, Conditioning::None)
    }
    pub fn is_boolean(&self) -> bool {
        matches!(self.c, Conditioning::Boolean)
    }
    pub fn is_loobean(&self) -> bool {
        matches!(self.c, Conditioning::Loobean)
    }
    pub fn with_raw_magma(&self, m: RawMagma) -> Magma {
        Magma { m, ..self.clone() }
    }
    pub fn with_conditioning(&self, c: Conditioning) -> Magma {
        Magma { c, ..self.clone() }
    }
    pub fn binary() -> Magma {
        RawMagma::Binary.into()
    }
    pub fn nibble() -> Magma {
        RawMagma::Nibble.into()
    }
    pub fn byte() -> Magma {
        RawMagma::Byte.into()
    }
    pub fn native() -> Magma {
        RawMagma::Native.into()
    }
    pub fn integer(b: usize) -> Magma {
        RawMagma::Integer(b).into()
    }
    pub fn any() -> Magma {
        RawMagma::Any.into()
    }
}
impl std::convert::TryFrom<&str> for Magma {
    type Error = anyhow::Error;

    fn try_from(s: &str) -> Result<Self, Self::Error> {
        match s.to_lowercase().as_str() {
            ":loobean" | ":loob" => Ok(Magma {
                m: RawMagma::Native,
                c: Conditioning::Loobean,
            }),
            ":boolean" | ":bool" => Ok(Magma {
                m: RawMagma::Native,
                c: Conditioning::Boolean,
            }),
            ":nibble" => Ok(RawMagma::Nibble.into()),
            ":byte" => Ok(RawMagma::Byte.into()),
            ":native" | ":natural" => Ok(RawMagma::Native.into()),
            s => {
                let re = regex_lite::Regex::new(r":i(\d+)").unwrap();
                match re.captures(s).and_then(|cs| cs.get(1)) {
                    Some(x) => Ok(RawMagma::Integer(x.as_str().parse::<usize>().unwrap()).into()),
                    None => bail!("unknown type: `{}`", s),
                }
            }
        }
    }
}
impl std::cmp::Ord for Magma {
    fn cmp(&self, other: &Self) -> Ordering {
        self.partial_cmp(other).unwrap()
    }
}
/// The ordering relation defines the casting rules for inter-magmas operations
/// e.g. boolean × boolean = boolean, but boolean × integer = Integer
impl std::cmp::PartialOrd for Magma {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self.m, other.m) {
            (RawMagma::None, RawMagma::None) => Some(Ordering::Equal),
            (RawMagma::None, _) => Some(Ordering::Less),
            (_, RawMagma::None) => Some(Ordering::Greater),

            (RawMagma::Binary, RawMagma::Binary) => Some(Ordering::Equal),
            (RawMagma::Binary, RawMagma::Nibble) => Some(Ordering::Less),
            (RawMagma::Binary, RawMagma::Byte) => Some(Ordering::Less),
            (RawMagma::Binary, RawMagma::Native) => Some(Ordering::Less),
            (RawMagma::Binary, RawMagma::Integer(_)) => Some(Ordering::Less),

            (RawMagma::Nibble, RawMagma::Binary) => Some(Ordering::Greater),
            (RawMagma::Nibble, RawMagma::Nibble) => Some(Ordering::Equal),
            (RawMagma::Nibble, RawMagma::Byte) => Some(Ordering::Less),
            (RawMagma::Nibble, RawMagma::Native) => Some(Ordering::Less),
            (RawMagma::Nibble, RawMagma::Integer(_)) => Some(Ordering::Less),

            (RawMagma::Byte, RawMagma::Binary) => Some(Ordering::Greater),
            (RawMagma::Byte, RawMagma::Nibble) => Some(Ordering::Greater),
            (RawMagma::Byte, RawMagma::Byte) => Some(Ordering::Equal),
            (RawMagma::Byte, RawMagma::Native) => Some(Ordering::Less),
            (RawMagma::Byte, RawMagma::Integer(_)) => Some(Ordering::Less),

            (RawMagma::Native, RawMagma::Binary) => Some(Ordering::Greater),
            (RawMagma::Native, RawMagma::Nibble) => Some(Ordering::Greater),
            (RawMagma::Native, RawMagma::Byte) => Some(Ordering::Greater),
            (RawMagma::Native, RawMagma::Native) => Some(Ordering::Equal),
            (RawMagma::Native, RawMagma::Integer(_)) => {
                Some(self.m.bit_size().cmp(&other.m.bit_size()))
            }

            (RawMagma::Any, RawMagma::Any) => Some(Ordering::Equal),
            (RawMagma::Any, _) => Some(Ordering::Greater),
            (_, RawMagma::Any) => Some(Ordering::Less),

            // The following are assuming that field elements will always be larger than 8 bits
            (RawMagma::Integer(_), RawMagma::Binary) => Some(Ordering::Greater),
            (RawMagma::Integer(_), RawMagma::Nibble) => Some(Ordering::Greater),
            (RawMagma::Integer(_), RawMagma::Byte) => Some(Ordering::Greater),
            (RawMagma::Integer(_), RawMagma::Native) => {
                Some(self.m.bit_size().cmp(&other.m.bit_size()))
            }
            (RawMagma::Integer(_), RawMagma::Integer(_)) => {
                Some(self.m.bit_size().cmp(&other.m.bit_size()))
            }
        }
    }
}
impl std::fmt::Display for Magma {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self.m {
            RawMagma::None => write!(f, "NONE"),
            RawMagma::Binary => write!(f, "B"),
            RawMagma::Nibble => write!(f, "𝟜"),
            RawMagma::Byte => write!(f, "𝟠"),
            RawMagma::Native => write!(f, "𝔽"),
            RawMagma::Integer(x) => write!(f, "i{}", x),
            RawMagma::Any => write!(f, "∀"),
        }?;

        match self.c {
            Conditioning::None => {}
            Conditioning::Boolean => write!(f, "->𝔹")?,
            Conditioning::Loobean => write!(f, "->𝕃")?,
        };

        std::fmt::Result::Ok(())
    }
}

/// Checks that a given list of types `found` is compatible with a list of list
/// of types `expected`. For each position in the lists, the type in `found`
/// must match with one of the types in the corresponding nested list in
/// `expected` for the operation to be a success.
///
/// In the case where `found` is longer than `expected`, the last element of
/// `expected` is repeated as many times as required. This allows for typing
/// variadic functions, as long as their tail of arguments is type-homogeneous.
pub fn compatible_with_repeating(expected: &[&[Type]], found: &[Type]) -> bool {
    for (es, f) in expected
        .iter()
        .chain(std::iter::repeat(expected.last().unwrap()))
        .zip(found.iter())
    {
        if !es.iter().any(|e| f.can_cast_to(*e)) {
            return false;
        }
    }
    true
}

/// Checks that a given list of types `found` is compatible with a list of list
/// of types `expected`. For each position in the lists, the type in `found`
/// must match with the corresponding type `expected` for the operation to be a
/// success.
///
/// If `found` and `expected` differ in length, the operation is a failure.
pub fn compatible_with(expected: &[Type], found: &[Type]) -> bool {
    if expected.len() != found.len() {
        return false;
    }

    for (e, f) in expected.iter().zip(found.iter()) {
        if !f.can_cast_to(*e) {
            return false;
        }
    }
    true
}
