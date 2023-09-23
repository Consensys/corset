#![allow(dead_code)]
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
    /// a field element
    Native,
    /// an arbitrary long integer
    Integer(usize),
    /// Anything
    Any,
}
impl Magma {
    // the dominant instantiable magma
    const SUPREMUM: Self = Magma::Native;

    pub fn bit_size(&self) -> usize {
        match self {
            Magma::None => 0,
            Magma::Boolean => 1,
            Magma::Loobean => 1,
            Magma::Nibble => 4,
            Magma::Byte => 8,
            Magma::Native => 254,
            Magma::Integer(x) => *x,
            Magma::Any => 254,
        }
    }

    pub fn byte_size(&self) -> usize {
        let bit_size = self.bit_size();
        if bit_size % 8 == 0 || bit_size < 8 {
            bit_size / 8
        } else {
            bit_size / 8 + 1
        }
    }

    fn can_cast_to(&self, other: Magma) -> bool {
        match (self, other) {
            (Magma::None, Magma::None) => true,
            (Magma::None, _) => false,

            (Magma::Boolean, Magma::None) => false,
            (Magma::Boolean, _) => true,

            (Magma::Loobean, Magma::None) => false,
            (Magma::Loobean, Magma::Boolean) => false,
            (Magma::Loobean, Magma::Loobean) => true,
            (Magma::Loobean, Magma::Nibble) => false,
            (Magma::Loobean, Magma::Byte) => false,
            (Magma::Loobean, Magma::Native) => false,
            (Magma::Loobean, Magma::Integer(_)) => false,
            (Magma::Loobean, Magma::Any) => true,

            (Magma::Nibble, Magma::None)
            | (Magma::Nibble, Magma::Boolean)
            | (Magma::Nibble, Magma::Loobean) => false,
            (Magma::Nibble, Magma::Nibble)
            | (Magma::Nibble, Magma::Byte)
            | (Magma::Nibble, Magma::Native) => true,

            (Magma::Byte, Magma::None)
            | (Magma::Byte, Magma::Boolean)
            | (Magma::Byte, Magma::Loobean)
            | (Magma::Byte, Magma::Nibble) => false,
            (Magma::Byte, Magma::Byte) | (Magma::Byte, Magma::Native) => true,

            (Magma::Native, Magma::None)
            | (Magma::Native, Magma::Boolean)
            | (Magma::Native, Magma::Loobean)
            | (Magma::Native, Magma::Nibble)
            | (Magma::Native, Magma::Byte) => false,
            (Magma::Native, Magma::Native) => true,
            (Magma::Native, Magma::Any) => true,

            (Magma::Integer(_), Magma::None) => false,
            (Magma::Integer(_), Magma::Boolean) => true,
            (Magma::Integer(_), Magma::Loobean) => false,
            (Magma::Integer(_), Magma::Nibble)
            | (Magma::Integer(_), Magma::Byte)
            | (Magma::Integer(_), Magma::Native)
            | (Magma::Integer(_), Magma::Integer(_)) => self.bit_size() <= other.bit_size(),

            (_, Magma::Integer(_)) => self.bit_size() >= other.bit_size(),

            (Magma::Any, _) => false,
            (_, Magma::Any) => true,
        }
    }
}
impl std::convert::TryFrom<&str> for Magma {
    type Error = anyhow::Error;

    fn try_from(s: &str) -> Result<Self, Self::Error> {
        let re = regex_lite::Regex::new(r":i(\d+)").unwrap();

        match s.to_lowercase().as_str() {
            ":loobean" | ":loob" => Ok(Magma::Loobean),
            ":boolean" | ":bool" => Ok(Magma::Boolean),
            ":nibble" => Ok(Magma::Nibble),
            ":byte" => Ok(Magma::Byte),
            ":native" | ":natural" => Ok(Magma::Native),
            s => match re.captures(s).and_then(|cs| cs.get(1)) {
                Some(x) => Ok(Magma::Integer(dbg!(x.as_str()).parse::<usize>().unwrap())),
                None => bail!("unknown type: `{}`", s),
            },
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
            (Magma::Boolean, Magma::Native) => Some(Ordering::Less),
            (Magma::Boolean, Magma::Integer(_)) => Some(Ordering::Less),

            (Magma::Nibble, Magma::Boolean) => Some(Ordering::Greater),
            (Magma::Nibble, Magma::Nibble) => Some(Ordering::Equal),
            (Magma::Nibble, Magma::Byte) => Some(Ordering::Less),
            (Magma::Nibble, Magma::Native) => Some(Ordering::Less),
            (Magma::Nibble, Magma::Integer(_)) => Some(Ordering::Less),

            (Magma::Byte, Magma::Boolean) => Some(Ordering::Greater),
            (Magma::Byte, Magma::Nibble) => Some(Ordering::Greater),
            (Magma::Byte, Magma::Byte) => Some(Ordering::Equal),
            (Magma::Byte, Magma::Native) => Some(Ordering::Less),
            (Magma::Byte, Magma::Integer(_)) => Some(Ordering::Less),

            (Magma::Native, Magma::Boolean) => Some(Ordering::Greater),
            (Magma::Native, Magma::Nibble) => Some(Ordering::Greater),
            (Magma::Native, Magma::Byte) => Some(Ordering::Greater),
            (Magma::Native, Magma::Native) => Some(Ordering::Equal),
            (Magma::Native, Magma::Integer(_)) => Some(self.bit_size().cmp(&other.bit_size())),

            (Magma::Any, Magma::Any) => Some(Ordering::Equal),
            (Magma::Any, _) => Some(Ordering::Greater),
            (_, Magma::Any) => Some(Ordering::Less),

            (Magma::Loobean, Magma::Loobean) => Some(Ordering::Equal),
            (Magma::Loobean, _) => Some(Ordering::Less),
            (_, Magma::Loobean) => Some(Ordering::Greater),

            // The following are assuming that field elements will always be larger than 8 bits
            (Magma::Integer(_), Magma::Boolean) => Some(Ordering::Greater),
            (Magma::Integer(_), Magma::Nibble) => Some(Ordering::Greater),
            (Magma::Integer(_), Magma::Byte) => Some(Ordering::Greater),
            (Magma::Integer(_), Magma::Native) => Some(self.bit_size().cmp(&other.bit_size())),
            (Magma::Integer(_), Magma::Integer(_)) => Some(self.bit_size().cmp(&other.bit_size())),
        }
    }
}
impl std::fmt::Display for Magma {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Magma::None => write!(f, "NONE"),
            Magma::Loobean => write!(f, "𝕃"),
            Magma::Boolean => write!(f, "𝔹"),
            Magma::Nibble => write!(f, "𝟜"),
            Magma::Byte => write!(f, "𝟠"),
            Magma::Native => write!(f, "𝔽"),
            Magma::Integer(x) => write!(f, "i{}", x),
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
