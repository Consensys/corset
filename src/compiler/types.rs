use std::cmp::Ordering;

use anyhow::bail;
use serde::{Deserialize, Serialize};

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
            Type::Any(m) => write!(f, "∀{}", m),
            Type::ArrayColumn(m) => write!(f, "[[{}]]", m),
            Type::List(m) => write!(f, "{{{}}}", m),
        }
    }
}
impl Type {
    pub const SUPREMUM: Self = Type::Column(Magma::SUPREMUM);
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

    pub fn with_magma(&self, new: Magma) -> Self {
        match self {
            Type::Void => todo!(),
            Type::Scalar(_) => Type::Scalar(new),
            Type::Column(_) => Type::Column(new),
            Type::Any(_) => Type::Any(new),
            Type::ArrayColumn(_) => Type::ArrayColumn(new),
            Type::List(_) => Type::List(new),
        }
    }

    pub fn with_scale(&self, new: Type) -> Self {
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
    Loobean,
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
            ":loobean" | ":loob" => Ok(Magma::Loobean),
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
            (Magma::None, Magma::None) => Some(Ordering::Equal),
            (Magma::None, _) => Some(Ordering::Less),
            (_, Magma::None) => Some(Ordering::Greater),

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

            (Magma::Loobean, Magma::Loobean) => Some(Ordering::Equal),
            (Magma::Loobean, _) => Some(Ordering::Less),
            (_, Magma::Loobean) => Some(Ordering::Greater),
        }
    }
}
impl std::fmt::Display for Magma {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Magma::None => write!(f, "NONE"),
            Magma::Loobean => write!(f, "𝕃"),
            Magma::Boolean => write!(f, "𝔹"),
            Magma::Nibble => write!(f, "Nib."),
            Magma::Byte => write!(f, "Byte"),
            Magma::Integer => write!(f, "Fr"),
            Magma::Any => write!(f, "∀"),
        }
    }
}
impl Magma {
    // the dominant instantiable magma
    const SUPREMUM: Self = Magma::Integer;
}

pub fn cyclic_compatible_with(expected: &[&[Type]], found: &[Type]) -> bool {
    for (es, f) in expected.iter().cycle().zip(found.iter()) {
        if !es.iter().any(|e| e >= f) {
            return false;
        }
    }
    true
}

pub fn compatible_with(expected: &[Type], found: &[Type]) -> bool {
    if expected.len() != found.len() {
        return false;
    }

    for (e, f) in expected.iter().zip(found.iter()) {
        if f > e {
            return false;
        }
    }
    true
}
