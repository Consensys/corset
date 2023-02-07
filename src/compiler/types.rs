#![allow(dead_code)]
use std::cmp::Ordering;

use anyhow::bail;
use serde::{Deserialize, Serialize};

/// The type of a column in the IR. This struct contains both the dimensionality
/// of the type and its underlying magma.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Ord, Serialize, Deserialize)]
pub enum Type {
    Void,
    Scalar(Magma),
    Column(Magma),
    ArrayColumn(Magma),
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
            Type::ArrayColumn(m) => m,
            Type::List(m) => m,
        }
    }

    pub fn same_scale(&self, new: Magma) -> Self {
        match self {
            Type::Void => todo!(),
            Type::Scalar(_) => Type::Scalar(new),
            Type::Column(_) => Type::Column(new),
            Type::ArrayColumn(_) => Type::ArrayColumn(new),
            Type::List(_) => Type::List(new),
        }
    }

    pub fn as_scalar(&self) -> Self {
        match self {
            Type::Column(x) => Type::Scalar(*x),
            Type::ArrayColumn(x) => Type::Scalar(*x),
            Type::List(x) => Type::Scalar(*x),
            _ => *self,
        }
    }
    pub fn is_bool(&self) -> bool {
        match self {
            Type::Void | Type::List(_) | Type::ArrayColumn(_) => false,
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
}
impl std::cmp::PartialOrd for Type {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (Type::Void, Type::Void) => Some(Ordering::Equal),
            (Type::Void, _) => Some(Ordering::Less),
            (Type::Scalar(_), Type::Void) => Some(Ordering::Greater),
            (Type::Scalar(_), Type::Column(_)) => None,
            (Type::Scalar(x), Type::Scalar(y)) => Some(x.cmp(y)),
            (Type::Column(_), Type::Void) => Some(Ordering::Greater),
            (Type::Column(x), Type::Column(y)) => Some(x.cmp(y)),
            (Type::Column(_), Type::Scalar(_)) => None,

            (Type::ArrayColumn(x), Type::ArrayColumn(y)) => Some(x.cmp(y)),
            (Type::ArrayColumn(_), _) => None,
            (_, Type::ArrayColumn(_)) => None,

            (Type::List(_), Type::Column(_)) => None,
            (Type::Column(_), Type::List(_)) => None,

            (x, y) => unimplemented!("{:?} <?> {:?}", x, y),
        }
    }
}

/// [ill-named] A magma is a set where operations stay within the itself.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum Magma {
    Boolean,
    /// 4-bits
    Nibble,
    /// 8-bits
    Byte,
    /// a field element
    Integer,
    /// Anything
    Any,
}
impl std::convert::TryFrom<&str> for Magma {
    type Error = anyhow::Error;

    fn try_from(s: &str) -> Result<Self, Self::Error> {
        match s.to_lowercase().as_str() {
            ":boolean" | ":bool" => Ok(Magma::Boolean),
            ":nibble" => Ok(Magma::Nibble),
            ":byte" => Ok(Magma::Byte),
            ":integer" | ":natural" => Ok(Magma::Integer),
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
            (Magma::Boolean, Magma::Boolean) => Some(Ordering::Equal),
            (Magma::Boolean, Magma::Nibble) => Some(Ordering::Less),
            (Magma::Boolean, Magma::Byte) => Some(Ordering::Less),
            (Magma::Boolean, Magma::Integer) => Some(Ordering::Less),

            (Magma::Nibble, Magma::Boolean) => Some(Ordering::Greater),
            (Magma::Nibble, Magma::Nibble) => Some(Ordering::Equal),
            (Magma::Nibble, Magma::Byte) => Some(Ordering::Less),
            (Magma::Nibble, Magma::Integer) => Some(Ordering::Less),

            (Magma::Byte, Magma::Boolean) => Some(Ordering::Greater),
            (Magma::Byte, Magma::Nibble) => Some(Ordering::Greater),
            (Magma::Byte, Magma::Byte) => Some(Ordering::Equal),
            (Magma::Byte, Magma::Integer) => Some(Ordering::Less),

            (Magma::Integer, Magma::Boolean) => Some(Ordering::Greater),
            (Magma::Integer, Magma::Nibble) => Some(Ordering::Greater),
            (Magma::Integer, Magma::Byte) => Some(Ordering::Greater),
            (Magma::Integer, Magma::Integer) => Some(Ordering::Equal),

            (Magma::Any, Magma::Any) => Some(Ordering::Equal),
            (Magma::Any, _) => Some(Ordering::Greater),
            (_, Magma::Any) => Some(Ordering::Less),
        }
    }
}
impl Magma {
    // the dominant instantiable magma
    const SUPREMUM: Self = Magma::Integer;
}

pub fn compatible_with(expected: &[&[Type]], found: &[Type]) -> bool {
    for (es, f) in expected.iter().cycle().zip(found.iter()) {
        if !es.iter().any(|e| e >= f) {
            return false;
        }
    }
    true
}
