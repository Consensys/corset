use crate::{
    compiler::{ColumnRef, EvalSettings, Intrinsic, Kind, Magma, Node},
    constants, errors,
    pretty::{opcodes, Base, Pretty},
    structs::Handle,
};
use anyhow::*;
use ark_bls12_377::fr::Fr;
use ark_ff::{fields::Field, BigInteger, PrimeField};
use itertools::Itertools;
use num_bigint::{BigInt, Sign};
use num_traits::{Euclid, FromPrimitive, Num, One, ToPrimitive, Zero};
use owo_colors::OwoColorize;
use serde::{Deserialize, Serialize};
use std::ops::{AddAssign, MulAssign, SubAssign};
use std::{
    collections::{HashMap, HashSet},
    str::FromStr,
    sync::OnceLock,
};

pub type RegisterID = usize;
pub type ColumnID = usize;

static POW_2_256: OnceLock<BigInt> = OnceLock::new();
fn clamp_bi(bi: &mut BigInt) {
    // TODO: adapt to field size
    *bi = bi.rem_euclid(POW_2_256.get_or_init(|| {
        BigInt::from_str_radix(
            "10000000000000000000000000000000000000000000000000000000000000000",
            16,
        )
        .unwrap()
    }));
    assert!(bi.sign() != Sign::Minus);
}

#[derive(Debug, Clone, Eq, Ord, Hash, Serialize, Deserialize)]
pub enum Value {
    BigInt(BigInt),
    #[serde(with = "FrDef")]
    Native(Fr),
    #[serde(skip)]
    ExoNative(Vec<Fr>),
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(remote = "Fr")]
struct FrDef {
    #[serde(getter = "get_limbs")]
    limbs: [u64; 4],
}

fn get_limbs(x: &Fr) -> &[u64; 4] {
    &x.0 .0
}

impl From<FrDef> for Fr {
    fn from(def: FrDef) -> Fr {
        Fr::from(ark_ff::BigInt(def.limbs))
    }
}

impl Value {
    pub fn is_zero(&self) -> bool {
        match self {
            Value::Native(f) => f.is_zero(),
            Value::ExoNative(fs) => fs.iter().all(|f| f.is_zero()),
            Value::BigInt(i) => i.is_zero(),
        }
    }

    pub fn is_one(&self) -> bool {
        match &self {
            Value::Native(f) => *f == Fr::one(),
            Value::ExoNative(fs) => fs[0] == Fr::one() && fs.iter().skip(1).all(|f| f.is_zero()),
            Value::BigInt(bi) => bi.is_one(),
        }
    }

    pub fn big_int(bi: BigInt) -> Self {
        Value::BigInt(bi)
    }

    pub(crate) fn zero() -> Self {
        if *crate::IS_NATIVE.read().unwrap() {
            Value::Native(Fr::zero())
        } else {
            Value::BigInt(BigInt::zero())
        }
    }

    pub(crate) fn one() -> Self {
        if *crate::IS_NATIVE.read().unwrap() {
            Value::Native(Fr::one())
        } else {
            Value::BigInt(BigInt::one())
        }
    }

    pub(crate) fn add_assign(&mut self, other: &Value) {
        match (self, other) {
            (Value::BigInt(ref mut i1), Value::BigInt(ref i2)) => *i1 += i2,
            (Value::BigInt(_), Value::Native(_)) => todo!(),
            (Value::BigInt(_), Value::ExoNative(_)) => todo!(),
            (Value::Native(_), Value::BigInt(_)) => todo!(),
            (Value::Native(ref mut f1), Value::Native(ref f2)) => f1.add_assign(f2),
            (Value::Native(_), Value::ExoNative(_)) => todo!(),
            (Value::ExoNative(_), Value::BigInt(_)) => todo!(),
            (Value::ExoNative(_), Value::Native(_)) => todo!(),
            (Value::ExoNative(_), Value::ExoNative(_)) => todo!(),
        }
    }

    pub(crate) fn sub_assign(&mut self, other: &Value) {
        match (self, other) {
            (Value::BigInt(ref mut i1), Value::BigInt(ref i2)) => *i1 -= i2,
            (Value::BigInt(i), Value::Native(n)) => todo!("{} -= {}", i, n),
            (Value::BigInt(_), Value::ExoNative(_)) => todo!(),
            (Value::Native(_), Value::BigInt(_)) => todo!(),
            (Value::Native(ref mut f1), Value::Native(ref f2)) => f1.sub_assign(f2),
            (Value::Native(_), Value::ExoNative(_)) => todo!(),
            (Value::ExoNative(_), Value::BigInt(_)) => todo!(),
            (Value::ExoNative(_), Value::Native(_)) => todo!(),
            (Value::ExoNative(_), Value::ExoNative(_)) => todo!(),
        }
    }

    pub(crate) fn mul_assign(&mut self, other: &Value) {
        match (self, other) {
            (Value::BigInt(ref mut i1), Value::BigInt(ref i2)) => *i1 *= i2,
            (Value::BigInt(x), Value::Native(y)) => todo!("{} *= {}", x, y),
            (Value::BigInt(_), Value::ExoNative(_)) => todo!(),
            (Value::Native(_), Value::BigInt(_)) => todo!(),
            (Value::Native(ref mut f1), Value::Native(ref f2)) => f1.mul_assign(f2),
            (Value::Native(_), Value::ExoNative(_)) => todo!(),
            (Value::ExoNative(_), Value::BigInt(_)) => todo!(),
            (Value::ExoNative(_), Value::Native(_)) => todo!(),
            (Value::ExoNative(_), Value::ExoNative(_)) => todo!(),
        }
    }

    pub(crate) fn negate(&mut self) {
        let mut ax = Value::zero().same_as(self);
        ax.sub_assign(self);
        *self = ax;
    }

    pub(crate) fn vector_add_assign(&mut self, other: &Value) {
        match (self, other) {
            (Value::BigInt(ref mut i1), Value::BigInt(ref i2)) => *i1 += i2,
            (Value::BigInt(_), Value::Native(_)) => todo!(),
            (Value::BigInt(_), Value::ExoNative(_)) => todo!(),
            (Value::Native(_), Value::BigInt(_)) => todo!(),
            (Value::Native(ref mut f1), Value::Native(ref f2)) => f1.add_assign(f2),
            (Value::Native(_), Value::ExoNative(_)) => todo!(),
            (Value::ExoNative(_), Value::BigInt(_)) => todo!(),
            (Value::ExoNative(_), Value::Native(_)) => todo!(),
            (Value::ExoNative(f1s), Value::ExoNative(f2s)) => f1s
                .iter_mut()
                .zip(f2s.iter())
                .for_each(|(f1, f2)| f1.add_assign(f2)),
        }
    }

    pub(crate) fn vector_sub_assign(&mut self, other: &Value) {
        match (self, other) {
            (Value::BigInt(ref mut i1), Value::BigInt(ref i2)) => *i1 -= i2,
            (Value::BigInt(_), Value::Native(_)) => todo!(),
            (Value::BigInt(_), Value::ExoNative(_)) => todo!(),
            (Value::Native(_), Value::BigInt(_)) => todo!(),
            (Value::Native(ref mut f1), Value::Native(ref f2)) => f1.sub_assign(f2),
            (Value::Native(_), Value::ExoNative(_)) => todo!(),
            (Value::ExoNative(_), Value::BigInt(_)) => todo!(),
            (Value::ExoNative(_), Value::Native(_)) => todo!(),
            (Value::ExoNative(f1s), Value::ExoNative(f2s)) => f1s
                .iter_mut()
                .zip(f2s.iter())
                .for_each(|(f1, f2)| f1.sub_assign(f2)),
        }
    }

    pub(crate) fn vector_mul_assign(&mut self, other: &Value) {
        match (self, other) {
            (Value::BigInt(ref mut i1), Value::BigInt(ref i2)) => *i1 *= i2,
            (Value::BigInt(_), Value::Native(_)) => todo!(),
            (Value::BigInt(_), Value::ExoNative(_)) => todo!(),
            (Value::Native(_), Value::BigInt(_)) => todo!(),
            (Value::Native(ref mut f1), Value::Native(ref f2)) => f1.mul_assign(f2),
            (Value::Native(_), Value::ExoNative(_)) => todo!(),
            (Value::ExoNative(_), Value::BigInt(_)) => todo!(),
            (Value::ExoNative(_), Value::Native(_)) => todo!(),
            (Value::ExoNative(f1s), Value::ExoNative(f2s)) => f1s
                .iter_mut()
                .zip(f2s.iter())
                .for_each(|(f1, f2)| f1.mul_assign(f2)),
        }
    }

    pub(crate) fn inverse(&self) -> Value {
        match &self {
            Value::Native(f) => Value::Native(f.inverse().unwrap_or_else(Fr::zero)),
            Value::ExoNative(fs) => Value::ExoNative(
                fs.iter()
                    .map(|f| f.inverse().unwrap_or_else(Fr::zero))
                    .collect(),
            ),
            Value::BigInt(_) => panic!("can not inverse BigInt"),
        }
    }

    // pub(crate) fn from_str(s: &str) -> Result<Value> {
    //     Ok(Value::BigInt(
    //         s.parse::<BigInt>()
    //             .with_context(|| anyhow!("while parsing `{}`", s))?,
    //     ))
    // }

    pub fn exoize(&self) -> Value {
        match self {
            Value::Native(f) => Value::BigInt(BigInt::from_str(&f.to_string()).unwrap()),
            Value::ExoNative(_) => todo!(),
            Value::BigInt(_) => self.clone(),
        }
    }

    pub(crate) fn to_bytes(&self) -> Vec<u8> {
        match &self {
            Value::BigInt(bi) => bi.to_bytes_be().1,
            Value::Native(f) => f.into_bigint().to_bytes_be(),
            Value::ExoNative(_) => todo!(),
        }
    }

    pub(crate) fn to_native(&mut self) {
        if let Value::BigInt(i) = self {
            clamp_bi(i);
            *self = if i.bits() as usize > crate::constants::FIELD_BITSIZE {
                let bs = i.to_bytes_le();
                let mut r = Vec::new();
                for bytes in &bs.1.iter().chunks(crate::constants::FIELD_BITSIZE / 8) {
                    let bb = bytes.cloned().collect_vec();
                    let small_big_int = BigInt::from_bytes_le(Sign::Plus, &bb);
                    r.push(Fr::from_str(&small_big_int.to_string()).unwrap());
                }
                r.reverse();
                Value::ExoNative(r)
            } else {
                Value::Native(Fr::from_str(&i.to_string()).unwrap())
            };
        }
    }

    pub(crate) fn to_bi(&self) -> BigInt {
        match self {
            Value::BigInt(x) => x.clone(),
            Value::Native(fr) => BigInt::from_bytes_be(Sign::Plus, &fr.into_bigint().to_bytes_be()),
            Value::ExoNative(_) => unimplemented!(),
        }
    }

    // pub(crate) fn make_bi(&mut self) {
    //     match self {
    //         Value::BigInt(_) => {}
    //         _ => *self = Value::BigInt(BigInt::from_bytes_le(Sign::Plus, &self.to_bytes())),
    //     }
    // }

    pub(crate) fn to_bi_variant(&self) -> Value {
        match self {
            Value::BigInt(_) => self.clone(),
            Value::Native(_) => Value::BigInt(self.to_bi()),
            _ => unimplemented!(),
        }
    }

    pub(crate) fn into_native(self) -> Value {
        match self {
            Value::BigInt(mut i) => {
                clamp_bi(&mut i);
                if i.bits() as usize > crate::constants::FIELD_BITSIZE {
                    let bs = i.to_bytes_le();
                    let mut r = Vec::new();
                    for bytes in &bs.1.iter().chunks(crate::constants::FIELD_BITSIZE / 8) {
                        let bb = bytes.cloned().collect_vec();
                        let small_big_int = BigInt::from_bytes_le(Sign::Plus, &bb);
                        r.push(Fr::from_str(&small_big_int.to_string()).unwrap());
                    }
                    r.reverse();
                    Value::ExoNative(r)
                } else {
                    Value::Native(Fr::from_str(&i.to_string()).unwrap())
                }
            }
            _ => self,
        }
    }

    pub(crate) fn same_as(self, other: &Value) -> Value {
        match other {
            Value::BigInt(_) => self.to_bi_variant(),
            Value::Native(_) => self.into_native(),
            Value::ExoNative(_) => unreachable!(),
        }
    }

    /// Return the normalized version of a value, i.e.:
    ///  * 0 or 1 for integers and field elements;
    ///  * each element recursively normalized for exo-values
    pub(crate) fn normalize(&self) -> Value {
        match &self {
            Value::Native(f) => Value::Native(if f.is_zero() { Fr::zero() } else { Fr::one() }),
            Value::ExoNative(fs) => Value::ExoNative(
                fs.iter()
                    .map(|f| if f.is_zero() { Fr::zero() } else { Fr::one() })
                    .collect(),
            ),
            Value::BigInt(i) => Value::BigInt(if i.is_zero() {
                BigInt::zero()
            } else {
                BigInt::one()
            }),
        }
    }

    pub(crate) fn fr_zero() -> Value {
        Value::Native(Fr::zero())
    }

    pub(crate) fn bi_zero() -> Value {
        Value::BigInt(BigInt::zero())
    }

    pub(crate) fn bit_size(&self) -> usize {
        match self {
            Value::BigInt(i) => i.bits() as usize,
            Value::Native(_) => crate::constants::FIELD_BITSIZE,
            Value::ExoNative(fs) => fs.len() * crate::constants::FIELD_BITSIZE,
        }
    }
}
impl std::default::Default for Value {
    fn default() -> Value {
        Value::zero()
    }
}

impl TryFrom<BigInt> for Value {
    type Error = errors::RuntimeError;

    fn try_from(int: BigInt) -> Result<Self, Self::Error> {
        if int.bits() as usize > constants::FIELD_BITSIZE {
            return Err(errors::RuntimeError::InvalidValue(
                "field element",
                Value::BigInt(int),
            ));
        }
        let mut v = Value::BigInt(int);
        if *crate::IS_NATIVE.read().unwrap() {
            v.to_native();
        }
        Result::Ok(v)
    }
}
impl TryFrom<&BigInt> for Value {
    type Error = errors::RuntimeError;

    fn try_from(int: &BigInt) -> Result<Self, Self::Error> {
        if int.bits() as usize > constants::FIELD_BITSIZE {
            return Err(errors::RuntimeError::InvalidValue(
                "field element",
                Value::BigInt(int.to_owned()),
            ));
        }
        let mut v = Value::BigInt(int.to_owned());
        if *crate::IS_NATIVE.read().unwrap() {
            v.to_native();
        }
        Result::Ok(v)
    }
}
impl From<Fr> for Value {
    fn from(f: Fr) -> Self {
        Value::Native(f)
    }
}
impl From<usize> for Value {
    fn from(x: usize) -> Self {
        if *crate::IS_NATIVE.read().unwrap() {
            Value::Native(Fr::from(x as u64))
        } else {
            Value::BigInt(BigInt::from_usize(x).unwrap())
        }
    }
}
impl From<isize> for Value {
    fn from(x: isize) -> Self {
        if *crate::IS_NATIVE.read().unwrap() {
            Value::Native(Fr::from(x as i64))
        } else {
            Value::BigInt(BigInt::from_isize(x).unwrap())
        }
    }
}
impl From<u64> for Value {
    fn from(x: u64) -> Self {
        if *crate::IS_NATIVE.read().unwrap() {
            Value::Native(Fr::from(x))
        } else {
            Value::BigInt(BigInt::from_u64(x).unwrap())
        }
    }
}
impl From<i32> for Value {
    fn from(x: i32) -> Self {
        if *crate::IS_NATIVE.read().unwrap() {
            Value::Native(Fr::from(x))
        } else {
            Value::BigInt(BigInt::from_i32(x).unwrap())
        }
    }
}
impl From<&str> for Value {
    fn from(x: &str) -> Self {
        if *crate::IS_NATIVE.read().unwrap() {
            Value::Native(Fr::from_str(x).unwrap())
        } else {
            Value::BigInt(BigInt::from_str(x).unwrap())
        }
    }
}
impl From<&Value> for BigInt {
    fn from(v: &Value) -> Self {
        v.to_bi()
    }
}
impl From<Value> for BigInt {
    fn from(v: Value) -> Self {
        v.to_bi()
    }
}
impl Pretty for Value {
    fn pretty(&self) -> String {
        match self {
            Value::BigInt(i) => format!("{}", i),
            Value::Native(f) => f.pretty(),
            Value::ExoNative(fs) => fs.iter().map(|f| f.pretty()).join("/"),
        }
    }

    fn pretty_with_base(&self, base: Base) -> String {
        match self {
            Value::BigInt(i) => match base {
                Base::Dec => i.to_str_radix(10),
                Base::Hex => format!("0x{}", i.to_str_radix(16)),
                Base::Bin | Base::Bool | Base::Loob => i.to_str_radix(2),
                Base::Bytes => i
                    .to_bytes_be()
                    .1
                    .iter()
                    .map(|b| format!("{b:0>2x}"))
                    .join(" "),
                Base::OpCode => {
                    opcodes::to_str(i.to_usize().unwrap_or(0xfe).try_into().unwrap_or(0xfe))
                }
            }
            .to_string(),
            Value::Native(f) => f.pretty_with_base(base),
            Value::ExoNative(fs) => fs.iter().map(|f| f.pretty_with_base(base)).join("."),
        }
    }
}
impl std::cmp::PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Value::BigInt(i1), Value::BigInt(i2)) => Some(i1.cmp(i2)),
            (Value::BigInt(_), Value::Native(_)) => todo!(),
            (Value::BigInt(_), Value::ExoNative(_)) => todo!(),
            (Value::Native(_), Value::BigInt(_)) => todo!(),
            (Value::Native(f1), Value::Native(f2)) => Some(f1.cmp(f2)),
            (Value::Native(_), Value::ExoNative(_)) => todo!(),
            (Value::ExoNative(_), Value::BigInt(_)) => todo!(),
            (Value::ExoNative(_), Value::Native(_)) => todo!(),
            (Value::ExoNative(_), Value::ExoNative(_)) => todo!(),
        }
    }
}
impl std::cmp::PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::BigInt(i1), Value::BigInt(i2)) => i1.cmp(i2).is_eq(),
            (Value::BigInt(_), Value::Native(_)) => false,
            (Value::BigInt(_), Value::ExoNative(_)) => todo!(),
            (Value::Native(_), Value::BigInt(_)) => todo!(),
            (Value::Native(f1), Value::Native(f2)) => f1.eq(f2),
            (Value::ExoNative(_), Value::BigInt(_)) => todo!(),
            (Value::Native(f2), Value::ExoNative(f1s))
            | (Value::ExoNative(f1s), Value::Native(f2)) => {
                f1s[0].eq(f2) && f1s.iter().skip(1).all(|f2| f2.is_zero())
            }
            (Value::ExoNative(fs1), Value::ExoNative(fs2)) => {
                fs1.iter().zip(fs2.iter()).all(|(f1, f2)| f1.eq(f2))
            }
        }
    }
}
impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            Value::BigInt(i) => write!(f, "{}", i),
            Value::Native(fr) => write!(f, "{}", fr.pretty()),
            Value::ExoNative(fs) => {
                write!(f, "{:?}", fs.iter().map(|f| f.pretty()).collect::<Vec<_>>())
            }
        }
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct FieldRegister {
    pub handle: Handle,
}

// #[derive(Debug)]
pub enum ValueBacking {
    Vector {
        v: Vec<Value>,
        spilling: isize,
    },
    Expression {
        e: Node,
        len: usize,
        spilling: isize,
    },
    Function {
        /// if i >= 0, shall return the expected actual value; if i < 0, shall
        /// return the adequate padding value
        f: Box<dyn Fn(isize, &ColumnSet) -> Option<Value> + Sync + Send>,
        len: usize,
        spilling: isize,
    },
}
impl std::fmt::Debug for ValueBacking {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ValueBacking::Vector { v, spilling } => {
                write!(
                    f,
                    "Vector-backed: len ({}) = {} + {}",
                    v.len(),
                    v.len() - *spilling as usize,
                    spilling
                )
            }
            ValueBacking::Function { len, spilling, .. } => {
                write!(f, "Function-backed: len = {} + {}", len, spilling)
            }
            ValueBacking::Expression { e, len, spilling } => {
                write!(f, "{}: len = {} + {}", e.pretty(), len, spilling)
            }
        }
    }
}
impl std::default::Default for ValueBacking {
    fn default() -> Self {
        ValueBacking::Vector {
            v: Vec::new(),
            spilling: 0,
        }
    }
}
impl ValueBacking {
    pub fn from_vec(v: Vec<Value>, spilling: isize) -> Self {
        ValueBacking::Vector { v, spilling }
    }

    pub fn from_expression(e: Node, len: usize, spilling: isize) -> Self {
        ValueBacking::Expression { e, len, spilling }
    }

    pub fn from_fn<F: Fn(isize, &'_ ColumnSet) -> Option<Value> + Sync + 'static + Send>(
        f: Box<F>,
        len: usize,
        spilling: isize,
    ) -> Self {
        ValueBacking::Function { f, len, spilling }
    }

    pub fn len(&self) -> usize {
        match self {
            ValueBacking::Vector { v, spilling } => v.len() - *spilling as usize,
            ValueBacking::Expression { len, .. } => *len,
            ValueBacking::Function { len, .. } => *len,
        }
    }

    fn padded_len(&self) -> usize {
        match self {
            ValueBacking::Vector { v, .. } => v.len(),
            ValueBacking::Expression { len, spilling, .. }
            | ValueBacking::Function { len, spilling, .. } => len + *spilling as usize,
        }
    }

    fn spilling(&self) -> isize {
        match self {
            ValueBacking::Vector { spilling, .. }
            | ValueBacking::Expression { spilling, .. }
            | ValueBacking::Function { spilling, .. } => *spilling,
        }
    }

    fn update_value(&mut self, _v: Vec<Value>, _spilling: isize) -> Result<()> {
        match self {
            ValueBacking::Vector { v, spilling } => {
                assert!(*spilling == _spilling);
                if v.len() != _v.len() {
                    bail!(
                        "unable to merge a {}-long backing into a {}-long one",
                        _v.len(),
                        v.len()
                    );
                }
                for (x, y) in v.iter_mut().zip(_v.iter()) {
                    if !x.is_zero() {
                        bail!("overwriting non-zero value in shared register")
                    } else {
                        x.add_assign(y)
                    }
                }
            }
            ValueBacking::Expression { .. } => {
                bail!("can not update value of expression-based register backing")
            }
            ValueBacking::Function { .. } => {
                bail!("can not update value of functional register backing")
            }
        }
        Ok(())
    }

    pub fn get(&self, i: isize, wrap: bool, cs: &ColumnSet) -> Option<Value> {
        match self {
            ValueBacking::Vector { v, spilling } => {
                if i < 0 {
                    if wrap {
                        let new_i = v.len() as isize + i;
                        if new_i < 0 || new_i >= v.len() as isize {
                            panic!("abnormal wrapping value {}", new_i)
                        }
                        v.get((v.len() as isize + i) as usize)
                    } else if i < -spilling {
                        Some(v.get(0).unwrap())
                    } else {
                        v.get((i + spilling) as usize)
                    }
                } else {
                    v.get((i + spilling) as usize)
                }
            }
            .cloned(),
            ValueBacking::Expression { e, .. } => e.eval(
                i,
                |handle, j, _| {
                    cs.get(handle, j, false)
                        .or_else(|| cs.column(handle).unwrap().padding_value.as_ref().cloned())
                },
                &mut None,
                &EvalSettings { wrap: false },
            ),
            ValueBacking::Function { f, .. } => f(i, cs),
        }
    }

    fn get_raw(&self, i: isize, wrap: bool, cs: &ColumnSet) -> Option<Value> {
        match self {
            ValueBacking::Vector { v, spilling } => {
                if i < 0 {
                    if wrap {
                        v.get((v.len() as isize + i) as usize)
                    } else {
                        None
                    }
                } else {
                    v.get((i + spilling) as usize)
                }
            }
            .cloned(),
            ValueBacking::Expression { e, .. } => e.eval(
                i,
                |handle, j, _| {
                    cs.get(handle, j, false)
                        .or_else(|| cs.column(handle).unwrap().padding_value.as_ref().cloned())
                },
                &mut None,
                &EvalSettings { wrap: false },
            ),
            ValueBacking::Function { f, .. } => f(i, cs),
        }
    }

    fn concretize(mut self) -> Self {
        match self {
            ValueBacking::Vector { mut v, spilling } => {
                v.iter_mut().for_each(|x| x.to_native());
                ValueBacking::Vector { v, spilling }
            }
            ValueBacking::Expression { ref mut e, .. } => {
                e.concretize();
                self
            }
            ValueBacking::Function { f, len, spilling } => ValueBacking::Function {
                f: Box::new(move |i, columns: &ColumnSet| {
                    let mut v = f(i, columns);
                    if let Some(x) = v.as_mut() {
                        x.to_native()
                    }
                    v
                }),
                len,
                spilling,
            },
        }
    }

    pub fn iter<'a>(&'a self, columns: &'a ColumnSet) -> ValueBackingIter<'a> {
        ValueBackingIter {
            value: self,
            i: -self.spilling(),
            len: self.len() as isize,
            spilling: self.spilling(),
            columns,
        }
    }

    pub fn iter_without_spilling<'a>(&'a self, columns: &'a ColumnSet) -> ValueBackingIter<'a> {
        ValueBackingIter {
            value: self,
            i: 0,
            len: self.len() as isize,
            spilling: self.spilling(),
            columns,
        }
    }
}

pub struct ValueBackingIter<'a> {
    value: &'a ValueBacking,
    columns: &'a ColumnSet,
    len: isize,
    spilling: isize,
    i: isize,
}

impl<'a> Iterator for ValueBackingIter<'a> {
    type Item = Value;

    fn next(&mut self) -> Option<Self::Item> {
        match self.value {
            ValueBacking::Vector { v, .. } => {
                if self.i >= (v.len() as isize) {
                    None
                } else {
                    self.i += 1;
                    v.get((self.i + self.spilling - 1) as usize).cloned()
                }
            }
            ValueBacking::Expression { .. } => {
                if self.i >= self.len {
                    None
                } else {
                    self.i += 1;
                    Some(
                        self.value
                            .get(self.i - 1, false, self.columns)
                            .unwrap_or_default(),
                    )
                }
            }
            ValueBacking::Function { f, .. } => {
                if self.i >= self.len {
                    None
                } else {
                    self.i += 1;
                    f(self.i - 1, self.columns)
                }
            }
        }
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Register {
    pub handle: Option<Handle>,
    pub magma: Magma,
    pub length_multiplier: usize,
    #[serde(skip_serializing, skip_deserializing, default)]
    backing: Option<ValueBacking>,
    width: usize,
}

impl Register {
    pub fn make_with_spilling(
        f: &mut dyn FnMut(isize) -> Value,
        len: usize,
        spilling: isize,
    ) -> Vec<Value> {
        (-spilling..len as isize).map(f).collect()
    }

    pub fn is_empty(&self) -> bool {
        self.backing.is_none()
    }

    pub fn width(&self) -> usize {
        self.width
    }

    pub fn backing(&self) -> Option<&ValueBacking> {
        self.backing.as_ref()
    }

    fn set_value(&mut self, v: Vec<Value>, spilling: isize) -> Result<()> {
        if let Some(ref mut provider) = self.backing.as_mut() {
            provider.update_value(v, spilling)
        } else {
            let _ = self.backing.insert(ValueBacking::from_vec(
                Self::make_with_spilling(
                    &mut |i| v.get(i as usize).cloned().unwrap_or_else(Value::zero),
                    v.len(),
                    spilling,
                ),
                spilling,
            ));
            Ok(())
        }
    }

    fn set_raw_value(&mut self, v: Vec<Value>, spilling: isize) -> Result<()> {
        if let Some(ref mut provider) = self.backing.as_mut() {
            provider.update_value(v, spilling)
        } else {
            let _ = self.backing.insert(ValueBacking::from_vec(v, spilling));
            Ok(())
        }
    }

    pub fn set_backing(&mut self, v: ValueBacking) -> Result<()> {
        if self.backing.is_some() {
            bail!("backing already set");
        }
        self.backing = Some(v);
        Ok(())
    }

    pub fn padded_len(&self) -> Option<usize> {
        self.backing.as_ref().map(|v| v.padded_len())
    }

    pub fn len(&self) -> Option<usize> {
        self.backing.as_ref().map(|v| v.len())
    }

    pub fn get(&self, i: isize, wrap: bool, columns: &ColumnSet) -> Option<Value> {
        self.backing.as_ref().and_then(|v| v.get(i, wrap, columns))
    }

    pub fn get_raw(&self, i: isize, wrap: bool, columns: &ColumnSet) -> Option<Value> {
        self.backing
            .as_ref()
            .and_then(|v| v.get_raw(i, wrap, columns))
    }

    pub fn concretize(&mut self) {
        if let Some(v) = self.backing.take() {
            let _ = self.backing.insert(v.concretize());
        }
    }
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct Column {
    pub register: Option<RegisterID>,
    pub padding_value: Option<Value>,
    pub used: bool,
    pub must_prove: bool,
    pub kind: Kind<()>,
    pub t: Magma,
    pub intrinsic_size_factor: Option<usize>,
    pub base: Base,
    pub handle: Handle,
    computed: bool,
}
#[buildstructor::buildstructor]
impl Column {
    #[builder]
    pub fn new(
        register: Option<RegisterID>,
        padding_value: Option<i64>, // TODO: Value
        used: Option<bool>,
        must_prove: Option<bool>,
        kind: Option<Kind<()>>,
        t: Option<Magma>,
        intrinsic_size_factor: Option<usize>,
        base: Option<Base>,
        handle: Handle,
    ) -> Self {
        Column {
            register,
            padding_value: padding_value.map(|v| Value::from(v as usize)),
            used: used.unwrap_or(true),
            must_prove: must_prove.unwrap_or(false),
            kind: kind.unwrap_or(Kind::Computed),
            t: t.unwrap_or(Magma::native()),
            intrinsic_size_factor,
            base: base.unwrap_or(Base::Dec),
            computed: false,
            handle,
        }
    }
}

#[derive(Debug, Default, Serialize, Deserialize)]
pub struct ColumnSet {
    pub _cols: Vec<Column>,
    pub cols: HashMap<Handle, usize>,
    pub effective_len: HashMap<String, isize>,
    /// a module may have a lower bound on its columns length if it is involved
    /// in range proofs
    pub min_len: HashMap<String, usize>,
    pub field_registers: Vec<FieldRegister>,
    pub registers: Vec<Register>,
    pub spilling: HashMap<String, isize>, // module -> (past-spilling, future-spilling)
}

impl ColumnSet {
    pub(crate) fn module_of(&self, c: &ColumnRef) -> String {
        self.column(c).unwrap().handle.module.clone()
    }

    /// Determine the appropriate module for a given column
    /// expression.  This is done by looking at all the column
    /// references within that expression.  If they all refer to the
    /// same module, then that module is returned.  Otherwise, `None`
    /// is returned.
    pub(crate) fn module_for<I: std::borrow::Borrow<ColumnRef>, C: IntoIterator<Item = I>>(
        &self,
        cols: C,
    ) -> Option<String> {
        let modules = cols
            .into_iter()
            .filter_map(|c| self.column(c.borrow()).ok())
            .map(|c| c.handle.module.clone())
            .collect::<HashSet<_>>();
        if modules.len() != 1 {
            None
        } else {
            modules.into_iter().next()
        }
    }

    /// Determine the appropriate module for a set of column
    /// expressions.  This is done by looking at all the column
    /// references within those expressions.  If they all refer to the
    /// same module, then that module is returned.  Otherwise, `None`
    /// is returned.
    pub(crate) fn module_forall<I: std::borrow::Borrow<Node>, C: IntoIterator<Item = I>>(
        &self,
        nodes: C,
    ) -> Option<String> {
        let mut module: Option<String> = None;
        // Search though the dependencies of each node.  That is, the
        // set of column references in that node.  For the first one
        // encountered, we assign its module to to module.  Then, the
        // module of any subsequent column references are checked
        // against module to ensure they are the same.  If they're not
        // the same, then the result is ambiguous and `None` is
        // returned accordingly.
        for n in nodes.into_iter() {
            for c in n.borrow().dependencies() {
                // Safe assuming initial expression well-formed.
                let c_mod = &self.column(&c).unwrap().handle.module;
                //
                match module {
                    None => module = Some(c_mod.clone()),
                    Some(m) if &m != c_mod => {
                        // Abort early because we have identified
                        // there are two or more modules referenced in
                        // this set of nodes.
                        return None;
                    }
                    Some(_) => {
                        // Continue
                    }
                }
            }
        }
        // Done
        module
    }

    pub fn set_min_len(&mut self, module: &str, len: usize) {
        self.min_len
            .entry(module.to_string())
            .and_modify(|l| *l = (*l).max(len))
            .or_insert(len);
    }

    pub fn set_perspective(&mut self, h: &ColumnRef, p: &str) -> Result<()> {
        let current = &mut self
            .get_col_mut(h)
            .ok_or_else(|| anyhow!("TODO: create message"))?
            .handle
            .perspective
            .clone();

        if let Some(current_p) = current.as_ref() {
            if current_p != p {
                bail!(
                    "perspective already set for {}: {} ≠ {}",
                    self.column(h).unwrap().handle.pretty(),
                    current_p.blue(),
                    p.yellow()
                )
            } else {
                Ok(())
            }
        } else {
            *current = Some(p.to_string());
            Ok(())
        }
    }

    pub fn force_perspective(&mut self, h: &ColumnRef, p: &str) {
        self.get_col_mut(h).unwrap().handle.perspective = Some(p.to_string());
    }

    pub fn column(&self, h: &ColumnRef) -> Result<&Column> {
        if h.is_id() {
            self._cols.get(h.as_id()).ok_or_else(|| unreachable!())
        } else if h.is_handle() {
            self.by_handle(h.as_handle())
        } else {
            unreachable!()
        }
    }

    pub fn register(&self, h: &RegisterRef) -> Option<&Register> {
        if h.is_id() {
            self.registers.get(h.as_id())
        } else if h.is_handle() {
            self.registers.iter().find(|r| {
                r.handle
                    .as_ref()
                    .map(|handle| handle == h.as_handle())
                    .unwrap_or(false)
            })
        } else {
            unreachable!()
        }
    }

    pub(crate) fn mark_used(&mut self, h: &ColumnRef) -> Result<()> {
        if let Some(ref mut column) = self.get_col_mut(h) {
            column.used = true;
            Ok(())
        } else {
            bail!("{} can not be found", h.pretty())
        }
    }

    pub fn get_col_mut(&mut self, h: &ColumnRef) -> Option<&mut Column> {
        if h.is_id() {
            self._cols.get_mut(h.as_id())
        } else if h.is_handle() {
            self.by_handle_mut(h.as_handle())
        } else {
            unreachable!()
        }
    }

    pub fn get_register_mut(&mut self, h: &RegisterRef) -> Option<&mut Register> {
        if h.is_id() {
            self.registers.get_mut(h.as_id())
        } else if h.is_handle() {
            self.registers.iter_mut().find(|r| {
                r.handle
                    .as_ref()
                    .map(|handle| handle == h.as_handle())
                    .unwrap_or(false)
            })
        } else {
            unreachable!()
        }
    }

    pub fn all(&self) -> Vec<ColumnRef> {
        (0..self._cols.len()).map(ColumnRef::from).collect()
    }

    pub fn regs(&self) -> Vec<RegisterID> {
        (0..self.registers.len()).collect()
    }

    pub fn by_handle(&self, handle: &Handle) -> Result<&Column> {
        self.cols
            .get(handle)
            .and_then(|i| self._cols.get(*i))
            .ok_or_else(|| anyhow!(errors::CompileError::NotFound(handle.to_owned())))
    }

    pub fn by_handle_mut(&mut self, handle: &Handle) -> Option<&mut Column> {
        self.cols.get(handle).and_then(|i| self._cols.get_mut(*i))
    }

    pub fn modules(&self) -> HashSet<String> {
        self.cols.keys().map(|h| h.module.clone()).collect()
    }

    pub fn perspective(&self, r: &ColumnRef) -> Result<Option<&String>> {
        Ok(self.column(r)?.handle.perspective.as_ref())
    }

    pub(crate) fn perspective_of<'a, H: IntoIterator<Item = &'a ColumnRef>>(
        &self,
        hs: H,
    ) -> Result<Option<String>> {
        let ps = hs
            .into_iter()
            .filter_map(|h| self.column(h).unwrap().handle.perspective.clone())
            .collect::<HashSet<_>>();
        if ps.len() > 1 {
            bail!("no unique perspective")
        }

        Ok(ps.into_iter().next())
    }

    pub fn id_of(&self, h: &ColumnRef) -> usize {
        if h.is_id() {
            h.as_id()
        } else if h.is_handle() {
            *self
                .cols
                .get(h.as_handle())
                .unwrap_or_else(|| panic!("unknown column: {:?} in {:?}", h, self.cols))
        } else {
            unreachable!()
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = (ColumnRef, &Column)> {
        (0..self._cols.len()).map(|i| (ColumnRef::from(i), &self._cols[i]))
    }

    pub fn iter_module<'a>(
        &'a self,
        module: &'a str,
    ) -> impl Iterator<Item = (ColumnRef, &Column)> + 'a {
        self.iter().filter(move |c| c.1.handle.module == module)
    }

    pub fn iter_cols(&self) -> impl Iterator<Item = &Column> {
        self._cols.iter()
    }

    pub(crate) fn new_register(
        &mut self,
        handle: Handle,
        magma: Magma,
        length_multiplier: usize,
    ) -> RegisterID {
        self.registers.push(Register {
            handle: Some(handle),
            magma,
            length_multiplier,
            backing: None,
            width: crate::constants::col_count_magma(magma),
        });
        self.registers.len() - 1
    }

    pub fn assign_register(&mut self, handle: &ColumnRef, reg: RegisterID) -> Result<()> {
        self.assign_register_for_id(self.id_of(handle), reg)
    }

    fn assign_register_for_id(&mut self, id: usize, reg: RegisterID) -> Result<()> {
        assert!(reg < self.registers.len());
        let col = &mut self._cols[id];
        assert!(
            col.register.is_none(),
            "column {} is already assigned to {:?}",
            col.handle,
            self.registers[col.register.unwrap()].handle
        );
        col.register = Some(reg);
        Ok(())
    }

    pub fn insert_column(&mut self, column: Column) -> Result<ColumnRef> {
        if self.cols.contains_key(&column.handle) {
            bail!(
                "column {} already exists",
                column.handle.to_string().red().bold()
            )
        } else {
            let i = self._cols.len();

            self.cols.insert(column.handle.to_owned(), i);
            let r = ColumnRef::from_handle(column.handle.clone()).id(i);
            self._cols.push(column);
            Ok(r)
        }
    }

    pub fn maybe_insert_column(&mut self, column: Column) -> Option<ColumnRef> {
        if let Some(_) = self.cols.get(&column.handle) {
            None
        } else {
            let id = self._cols.len();
            self.cols.insert(column.handle.to_owned(), id);
            self._cols.push(column);

            Some(ColumnRef::from_id(id))
        }
    }

    pub fn insert_column_and_register(&mut self, mut column: Column) -> Result<ColumnRef> {
        let length_multiplier = column.intrinsic_size_factor.unwrap_or(1);
        column.register =
            Some(self.new_register(column.handle.clone(), column.t, length_multiplier));
        self.insert_column(column)
    }

    pub fn maybe_insert_column_and_register(&mut self, mut column: Column) -> Option<ColumnRef> {
        if self.cols.contains_key(&column.handle) {
            None
        } else {
            let length_multiplier = column.intrinsic_size_factor.unwrap_or(1);
            column.register =
                Some(self.new_register(column.handle.clone(), column.t, length_multiplier));
            self.maybe_insert_column(column)
        }
    }

    pub fn is_empty(&self) -> bool {
        const IGNORED: &[&str] = &["shift-table", "binary-table", "instruction-decoder"];
        self.iter_cols()
            .filter(|c| !IGNORED.contains(&c.handle.module.as_str()))
            .all(|c| self.registers[c.register.unwrap()].is_empty())
    }

    /// Determine the set of columns which are allocated to a given
    /// register.
    pub fn columns_of(&self, reg_id: RegisterID) -> Vec<ColumnRef> {
        let mut crefs = Vec::new();
        for i in 0..self._cols.len() {
            let Some(r) = self._cols[i].register else {
                continue;
            };
            if r == reg_id {
                crefs.push(ColumnRef::from(i));
            }
        }
        return crefs;
    }

    pub fn register_of_mut(&mut self, h: &ColumnRef) -> &mut Register {
        let reg = self.column(h).unwrap().register.unwrap();
        &mut self.registers[reg]
    }

    pub fn register_of(&self, h: &ColumnRef) -> &Register {
        let reg = self.column(h).unwrap().register.unwrap();
        &self.registers[reg]
    }

    pub fn get(&self, h: &ColumnRef, i: isize, wrap: bool) -> Option<Value> {
        self.register_of(h).get(i, wrap, self)
    }

    pub fn get_raw(&self, h: &ColumnRef, i: isize, wrap: bool) -> Option<Value> {
        self.register_of(h).get_raw(i, wrap, self)
    }

    pub fn len(&self, h: &ColumnRef) -> Option<usize> {
        self.register_of(h).len()
    }

    pub fn padded_len(&self, h: &ColumnRef) -> Option<usize> {
        self.register_of(h).padded_len()
    }

    pub fn backing(&self, h: &ColumnRef) -> Option<&ValueBacking> {
        self.register_of(h).backing.as_ref()
    }

    pub fn is_computed(&self, h: &ColumnRef) -> bool {
        self.column(h).unwrap().computed
    }

    pub fn set_column_value(
        &mut self,
        h: &ColumnRef,
        v: Vec<Value>,
        spilling: isize,
    ) -> Result<()> {
        self.get_col_mut(h).unwrap().computed = true;
        self.register_of_mut(h)
            .set_value(v, spilling)
            .with_context(|| anyhow!("while filling {}", h.pretty()))
    }

    pub fn set_register_value(
        &mut self,
        h: &RegisterRef,
        v: Vec<Value>,
        spilling: isize,
    ) -> Result<()> {
        let reg_id = if h.is_id() {
            h.as_id()
        } else {
            self.registers
                .iter()
                .enumerate()
                .find(|(_, r)| {
                    r.handle
                        .as_ref()
                        .map(|handle| handle == h.as_handle())
                        .unwrap_or(false)
                })
                .map(|(id, _)| id)
                .unwrap()
        };
        for column in self
            ._cols
            .iter_mut()
            .filter(|c| c.register.map(|r| r == reg_id).unwrap_or(false))
        {
            column.computed = true;
        }

        self.get_register_mut(h)
            .unwrap()
            .set_value(v, spilling)
            .with_context(|| anyhow!("while filling {}", h.pretty()))
    }

    pub fn set_raw_value(&mut self, h: &ColumnRef, v: Vec<Value>, spilling: isize) -> Result<()> {
        self.get_col_mut(h).unwrap().computed = true;
        self.register_of_mut(h).set_raw_value(v, spilling)
    }

    pub fn set_backing(&mut self, h: &ColumnRef, v: ValueBacking) -> Result<()> {
        self.get_col_mut(h).unwrap().computed = true;
        self.register_of_mut(h).set_backing(v)
    }
}

type RegisterRef = ColumnRef;

#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum ExoOperation {
    Add,
    Sub,
    Mul,
}
impl From<Intrinsic> for ExoOperation {
    fn from(op: Intrinsic) -> Self {
        match op {
            Intrinsic::Add => ExoOperation::Add,
            Intrinsic::Sub => ExoOperation::Sub,
            Intrinsic::Mul => ExoOperation::Mul,
            _ => unreachable!(),
        }
    }
}
impl std::fmt::Display for ExoOperation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ExoOperation::Add => write!(f, "⊕"),
            ExoOperation::Sub => write!(f, "⊖"),
            ExoOperation::Mul => write!(f, "⊗"),
        }
    }
}

// TODO: add a targets() function to automatize computation insertion
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Computation {
    Composite {
        target: ColumnRef,
        exp: Node,
    },
    ExoOperation {
        op: ExoOperation,
        sources: [Node; 2],
        target: ColumnRef,
    },
    ExoConstant {
        value: Value,
        target: ColumnRef,
    },
    Interleaved {
        target: ColumnRef,
        froms: Vec<ColumnRef>,
    },
    Sorted {
        froms: Vec<ColumnRef>,
        tos: Vec<ColumnRef>,
        signs: Vec<bool>,
    },
    CyclicFrom {
        target: ColumnRef,
        froms: Vec<ColumnRef>,
        modulo: usize,
    },
    SortingConstraints {
        ats: Vec<ColumnRef>,
        eq: ColumnRef,
        delta: ColumnRef,
        delta_bytes: Vec<ColumnRef>,
        signs: Vec<bool>,
        froms: Vec<ColumnRef>,
        sorted: Vec<ColumnRef>,
    },
}
impl std::fmt::Display for Computation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Computation::Composite { target, exp } => {
                write!(f, "{} = {}", target.pretty(), exp.pretty())
            }
            Computation::Interleaved { target, froms } => {
                write!(
                    f,
                    "{} ⪡ {}",
                    target.pretty(),
                    froms.iter().map(|c| c.pretty()).join(", ")
                )
            }
            Computation::ExoOperation {
                op,
                sources,
                target,
            } => {
                write!(f, "{} {:?} -> {}", op, sources, target)
            }
            Computation::ExoConstant { value, target } => {
                write!(f, "{} := {}", target, value)
            }
            Computation::Sorted { froms, tos, signs } => write!(
                f,
                "[{}] ⇳ [{}]",
                tos.iter().map(|c| c.pretty()).join(" "),
                froms
                    .iter()
                    .zip(signs.iter())
                    .map(|(c, s)| format!("{} {}", if *s { '↓' } else { '↑' }, c.pretty()))
                    .join(" "),
            ),
            Computation::CyclicFrom { target, froms, .. } => write!(
                f,
                "{} ↻ {}",
                froms.iter().map(|c| c.pretty()).join(", "),
                target
            ),
            Computation::SortingConstraints { sorted, .. } => write!(
                f,
                "Sorting constraints for {}",
                sorted.iter().map(|c| c.pretty()).join(", ")
            ),
        }
    }
}
impl Computation {
    pub fn pretty_target(&self) -> String {
        match self {
            Computation::Composite { target, .. }
            | Computation::Interleaved { target, .. }
            | Computation::ExoOperation { target, .. }
            | Computation::ExoConstant { target, .. } => target.to_string(),
            Computation::Sorted { tos, .. } => tos
                .iter()
                .map(|t| t.to_string())
                .collect::<Vec<_>>()
                .join(", "),
            Computation::CyclicFrom { target, .. } => target.to_string(),
            Computation::SortingConstraints { ats: target, .. } => target
                .iter()
                .map(|t| t.to_string())
                .collect::<Vec<_>>()
                .join(", "),
        }
    }

    pub fn is_interleaved(&self) -> bool {
        matches!(self, Computation::Interleaved { .. })
    }
}
