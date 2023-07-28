#![allow(dead_code)]
use std::cmp::Ordering;

use anyhow::bail;
use num_bigint::BigInt;
use num_traits::{One, Zero};
use serde::{Deserialize, Serialize};

use crate::structs::Field;

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

    pub fn magma(self) -> Magma {
        match self {
            Type::Void => Magma::None,
            Type::Scalar(m) => m,
            Type::Column(m) => m,
            Type::Any(m) => m,
            Type::ArrayColumn(m) => m,
            Type::List(m) => m,
        }
    }

    pub fn _with_magma(&self, new: Magma) -> Self {
        match self {
            Type::Void => todo!(),
            Type::Scalar(_) => Type::Scalar(new),
            Type::Column(_) => Type::Column(new),
            Type::Any(_) => Type::Any(new),
            Type::ArrayColumn(_) => Type::ArrayColumn(new),
            Type::List(_) => Type::List(new),
        }
    }

    pub fn _with_scale(&self, new: Type) -> Self {
        let magma = self.magma();
        match new {
            Type::Void => Type::Void,
            Type::Scalar(_) => Type::Scalar(magma),
            Type::Column(_) => Type::Column(magma),
            Type::Any(_) => Type::Any(magma),
            Type::ArrayColumn(_) => Type::ArrayColumn(magma),
            Type::List(_) => Type::List(magma),
        }
    }

    pub fn is_bool(&self) -> bool {
        match self {
            Type::Void | Type::List(_) | Type::ArrayColumn(_) => false,
            Type::Column(x) | Type::Scalar(x) | Type::Any(x) => matches!(x, Magma::Boolean),
        }
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
                if s.magma() == o.magma() {
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
                    s.magma().cmp(&o.magma())
                }
            }
        }
    }
}

/// [ill-named] A magma is a set where some operations stay within itself.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub enum Magma {
    None,
    Boolean,
    Loobean,
    /// 4-bits
    Nibble,
    /// 8-bits
    Byte,
    Integer(usize),      // number of bytes
    FieldElement(usize), // number of bytes stored in one field element
    /// Anything
    Any,
}

impl From<usize> for Magma {
    fn from(i: usize) -> Self {
        if i == 0 || i == 1 {
            Magma::Boolean
        } else {
            Magma::Integer(((usize::BITS - i.leading_zeros()) as usize + 7) / 8)
        }
    }
}

impl From<isize> for Magma {
    fn from(i: isize) -> Self {
        Magma::from(i as usize) // TODO ?
    }
}

impl From<&BigInt> for Magma {
    fn from(b: &BigInt) -> Self {
        if b.is_zero() || b.is_one() {
            Magma::Boolean
        } else {
            Magma::Integer(b.bits() as usize)
        }
    }
}

// TODO remove
impl Default for Magma {
    fn default() -> Self {
        Magma::Integer(32)
    }
}

impl Magma {
    pub fn from_bits(bits: usize) -> Self {
        if bits == 0 {
            panic!()
        } else if bits == 1 {
            Magma::Boolean
        } else {
            Magma::Integer((bits + 7) / 8)
        }
    }

    pub fn repr_count<F: Field>(&self) -> usize {
        // the number of field elements needed to represent this magma
        let n_bytes = (self.bits().unwrap() + 7) / 8;
        (n_bytes + F::BYTE_CAPACITY - 1) / F::BYTE_CAPACITY
    }

    pub fn bits(&self) -> Option<usize> {
        match self {
            Magma::None => Some(0),
            Magma::Boolean => Some(1),
            Magma::Loobean => Some(1),
            Magma::Nibble => Some(4),
            Magma::Byte => Some(8),
            Magma::Integer(bytes) => Some(*bytes * 8),
            Magma::FieldElement(bytes) => Some(*bytes * 8),
            Magma::Any => None,
        }
    }

    pub fn primitive<F: Field>() -> Self {
        Magma::FieldElement(F::BYTE_CAPACITY)
    }

    pub fn is_primitive(&self) -> bool {
        matches!(self, Magma::FieldElement(_))
    }

    fn can_cast_to(&self, other: Magma) -> bool {
        match (self, other) {
            (Magma::Any, _) => false,
            (_, Magma::Any) => true,
            _ => self.bits().unwrap() <= other.bits().unwrap(),
        }
    }
}
impl std::convert::TryFrom<&str> for Magma {
    type Error = anyhow::Error;

    fn try_from(s: &str) -> Result<Self, Self::Error> {
        if s.starts_with(":sized-") {
            // TODO something more readable
            match s[7..].parse::<usize>() {
                Ok(bytes) => return Ok(Magma::Integer(bytes)),
                Err(_) => bail!("invalid sized type: `{}`", s),
            }
        }
        match s.to_lowercase().as_str() {
            ":loobean" | ":loob" => Ok(Magma::Loobean),
            ":boolean" | ":bool" => Ok(Magma::Boolean),
            ":nibble" => Ok(Magma::Nibble),
            ":byte" => Ok(Magma::Byte),
            ":integer" | ":natural" => Ok(Magma::default()),
            _ => bail!("unknown type: `{}`", s),
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
        match (self, other) {
            (Magma::Any, Magma::Any) => Some(Ordering::Equal),
            (Magma::Any, _) => Some(Ordering::Greater),
            (_, Magma::Any) => Some(Ordering::Less),
            _ => self.bits().unwrap().partial_cmp(&other.bits().unwrap()),
        }
    }
}
impl std::fmt::Display for Magma {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Magma::None => write!(f, "NONE"),
            Magma::Loobean => write!(f, "𝕃"),
            Magma::Boolean => write!(f, "𝔹"),
            Magma::Nibble => write!(f, "4×.."),
            Magma::Byte => write!(f, "8×"),
            Magma::Integer(bytes) => write!(f, "Integer({} bytes)", bytes),
            Magma::FieldElement(bytes) => write!(f, "FieldElement({} bytes)", bytes),
            Magma::Any => write!(f, "∀"),
        }
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
