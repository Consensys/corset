use crate::{
    compiler::{ColumnRef, Intrinsic, Kind, Magma, Node},
    errors,
    pretty::{opcodes, Base, Pretty},
    structs::Handle,
};
use anyhow::*;
use itertools::Itertools;
use num_bigint::{BigInt, Sign};
use num_traits::{Euclid, FromPrimitive, Num, One, ToPrimitive, Zero};
use owo_colors::OwoColorize;
use pairing_ce::{
    bn256::Fr,
    ff::{Field, PrimeField},
};
use serde::{Deserialize, Serialize};
use std::{
    cell::OnceCell,
    collections::{HashMap, HashSet},
    str::FromStr,
};

pub type RegisterID = usize;
pub type ColumnID = usize;

const POW_2_256: OnceCell<BigInt> = OnceCell::new();
fn clamp_bi(bi: &mut BigInt) {
    *bi = bi.rem_euclid(POW_2_256.get_or_init(|| {
        BigInt::from_str_radix(
            "10000000000000000000000000000000000000000000000000000000000000000",
            16,
        )
        .unwrap()
    }));
    assert!(bi.sign() != Sign::Minus);
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, Ord, Hash)]
pub enum Value {
    BigInt(BigInt),
    Native(Fr),
    ExoNative(Vec<Fr>),
}
impl Value {
    pub fn to_string(&self) -> String {
        match self {
            Value::Native(f) => f.pretty(),
            Value::ExoNative(fs) => fs.iter().map(|f| f.to_string()).join(" "),
            Value::BigInt(x) => x.to_str_radix(10),
        }
    }

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

    pub(crate) fn zero() -> Self {
        Value::BigInt(BigInt::zero())
    }

    pub(crate) fn one() -> Self {
        Value::BigInt(BigInt::one())
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
            (Value::BigInt(_), Value::Native(_)) => todo!(),
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
        todo!()
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
            (Value::BigInt(ref mut i1), Value::BigInt(ref i2)) => *i1 += i2,
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
            (Value::BigInt(ref mut i1), Value::BigInt(ref i2)) => *i1 += i2,
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

    pub(crate) fn inverse(&self) -> Option<Value> {
        match &self {
            Value::Native(f) => f.inverse().map(Value::Native),
            Value::ExoNative(_) => unreachable!(),
            Value::BigInt(_) => panic!("can not inverse ExoValue"),
        }
    }

    pub(crate) fn from_str(s: &str) -> Result<Value> {
        Ok(Value::BigInt(
            s.parse::<BigInt>()
                .with_context(|| anyhow!("while parsing `{}`", s))?,
        ))
    }

    pub fn exoize(&self) -> Value {
        match self {
            Value::Native(f) => Value::BigInt(BigInt::from_str(&f.to_string()).unwrap()),
            Value::ExoNative(_) => todo!(),
            Value::BigInt(_) => self.clone(),
        }
    }

    pub(crate) fn into_repr(&self) -> impl Iterator<Item = u64> {
        let us = match &self {
            Value::Native(f) => f.into_repr().0.to_vec(),
            Value::ExoNative(fs) => fs
                .iter()
                .flat_map(|f| f.into_repr().0.into_iter())
                .collect(),
            Value::BigInt(_) => todo!(),
        };
        us.into_iter()
    }

    pub(crate) fn into_bytes(&self) -> Vec<u8> {
        match &self {
            Value::Native(f) => f
                .into_repr()
                .0
                .iter()
                .flat_map(|x| x.to_be_bytes())
                .collect(),
            Value::ExoNative(fs) => fs
                .iter()
                .flat_map(|f| f.into_repr().0.into_iter().flat_map(|x| x.to_be_bytes()))
                .collect(),
            Value::BigInt(bi) => bi.to_bytes_be().1,
        }
    }

    pub(crate) fn to_native(&mut self) {
        match self {
            Value::BigInt(i) => {
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
            _ => {}
        }
    }

    pub(crate) fn to_bi(&mut self) {
        match self {
            Value::BigInt(_) => {}
            _ => *self = Value::BigInt(BigInt::from_bytes_le(Sign::Plus, &self.into_bytes())),
        }
    }

    /// Return the normalized version of a value, i.e.:
    ///  * 0 or 1 for integers and field elements;
    ///  * each element recursively normalized for exo-values
    pub(crate) fn normalize(&self) -> Value {
        match &self {
            Value::Native(f) => {
                let mut r = f.inverse().unwrap_or_else(Fr::zero);
                r.mul_assign(&f);
                Value::Native(r)
            }
            Value::ExoNative(fs) => {
                Value::ExoNative(fs.iter().map(|f| f.inverse().unwrap()).collect())
            }
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
        Value::BigInt(BigInt::zero())
    }
}
impl From<BigInt> for Value {
    fn from(int: BigInt) -> Self {
        Value::BigInt(int)
    }
}
impl From<&BigInt> for Value {
    fn from(int: &BigInt) -> Self {
        Value::BigInt(int.clone())
    }
}
impl From<Fr> for Value {
    fn from(f: Fr) -> Self {
        Value::Native(f)
    }
}
impl From<usize> for Value {
    fn from(x: usize) -> Self {
        Value::BigInt(BigInt::from_usize(x).unwrap())
    }
}
impl From<isize> for Value {
    fn from(x: isize) -> Self {
        let bi = BigInt::from_isize(x).unwrap();
        Value::BigInt(bi)
    }
}
impl From<i32> for Value {
    fn from(x: i32) -> Self {
        let bi = BigInt::from_i32(x).unwrap();
        Value::BigInt(bi)
    }
}
impl From<&str> for Value {
    fn from(x: &str) -> Self {
        Value::BigInt(BigInt::from_str(x).unwrap())
    }
}
impl From<&Value> for BigInt {
    fn from(v: &Value) -> Self {
        match v {
            Value::BigInt(bi) => bi.clone(),
            Value::Native(_) => todo!(),
            Value::ExoNative(_) => todo!(),
        }
    }
}
impl From<Value> for BigInt {
    fn from(v: Value) -> Self {
        match v {
            Value::BigInt(bi) => bi.clone(),
            Value::Native(_) => todo!(),
            Value::ExoNative(_) => todo!(),
        }
    }
}
impl Pretty for Value {
    fn pretty(&self) -> String {
        match self {
            Value::BigInt(i) => format!("ε{}", i),
            Value::Native(f) => f.pretty(),
            Value::ExoNative(fs) => fs.iter().map(|f| f.pretty()).join("/"),
        }
    }

    fn pretty_with_base(&self, base: Base) -> String {
        match self {
            Value::BigInt(i) => {
                format!(
                    "ε{}",
                    match base {
                        Base::Dec => i.to_str_radix(10),
                        Base::Hex => i.to_str_radix(16),
                        Base::Bin | Base::Bool | Base::Loob => i.to_str_radix(2),
                        Base::Bytes => i
                            .to_bytes_le()
                            .1
                            .iter()
                            .map(|b| format!("{b:0>2x}"))
                            .join(" "),
                        Base::OpCode => opcodes::to_str(i.to_usize().unwrap().try_into().unwrap()),
                    }
                )
            }
            Value::Native(f) => f.pretty_with_base(base),
            Value::ExoNative(fs) => fs.iter().map(|f| f.pretty_with_base(base)).join("."),
        }
    }
}
impl std::cmp::PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Value::BigInt(i1), Value::BigInt(i2)) => Some(i1.cmp(&i2)),
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
            Value::BigInt(i) => write!(f, "ε{}", i),
            Value::Native(fr) => write!(f, "{}", fr),
            Value::ExoNative(fs) => {
                write!(
                    f,
                    "{:?}",
                    fs.iter().map(|f| f.to_string()).collect::<Vec<_>>()
                )
            }
        }
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct FieldRegister {
    pub handle: Handle,
    value: Option<Vec<Fr>>,
}

// #[derive(Debug)]
pub enum ValueBacking {
    Vector {
        v: Vec<Value>,
        spilling: isize,
    },
    Function {
        /// if i >= 0, shall return the expected actual value; if i < 0, shall
        /// return the adequate padding value
        f: Box<dyn Fn(isize, &ColumnSet) -> Option<Value> + Sync>,
        spilling: isize,
        end: usize,
    },
}
impl std::fmt::Debug for ValueBacking {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "XXXbackingXXX")
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

    pub fn from_fn<F: Fn(isize, &'_ ColumnSet) -> Option<Value> + Sync + 'static>(
        f: Box<F>,
    ) -> Self {
        ValueBacking::Function {
            f,
            spilling: 0,
            end: 32,
        }
    }

    pub fn len(&self) -> usize {
        match self {
            ValueBacking::Vector { v, spilling } => v.len() - *spilling as usize,
            ValueBacking::Function { end, .. } => *end,
        }
    }

    fn padded_len(&self) -> usize {
        match self {
            ValueBacking::Vector { v, .. } => v.len(),
            ValueBacking::Function { spilling, end, .. } => end + *spilling as usize,
        }
    }

    fn spilling(&self) -> isize {
        match self {
            ValueBacking::Vector { spilling, .. } | ValueBacking::Function { spilling, .. } => {
                *spilling
            }
        }
    }

    fn update_value(&mut self, _v: Vec<Value>, _spilling: isize) -> Result<()> {
        match self {
            ValueBacking::Vector { v, spilling } => {
                assert!(*spilling == _spilling);
                assert!(v.len() == _v.len());
                for (x, y) in v.iter_mut().zip(_v.iter()) {
                    if !x.is_zero() {
                        bail!("overwriting non-zero value in shared register")
                    } else {
                        x.add_assign(y)
                    }
                }
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
                        None
                    } else {
                        v.get((i + spilling) as usize)
                    }
                } else {
                    v.get((i + spilling) as usize)
                }
            }
            .cloned(),
            ValueBacking::Function { f, spilling, .. } => f(i + spilling, cs),
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
            ValueBacking::Function { f, .. } => f(i, cs),
        }
    }

    pub fn iter<'a>(&'a self, columns: &'a ColumnSet) -> ValueBackingIter<'a> {
        ValueBackingIter {
            value: self,
            i: 0,
            columns,
        }
    }

    fn concretize(self) -> Self {
        match self {
            ValueBacking::Vector { mut v, spilling } => {
                v.iter_mut().for_each(|x| x.to_native());
                ValueBacking::Vector { v, spilling }
            }
            ValueBacking::Function { f, spilling, end } => ValueBacking::Function {
                f: Box::new(move |i, columns: &ColumnSet| {
                    let mut v = f(i, columns);
                    v.as_mut().map(|x| x.to_native());
                    v
                }),
                spilling,
                end,
            },
        }
    }
}

pub struct ValueBackingIter<'a> {
    value: &'a ValueBacking,
    columns: &'a ColumnSet,
    i: isize,
}

impl<'a> Iterator for ValueBackingIter<'a> {
    type Item = Value;

    fn next(&mut self) -> Option<Self::Item> {
        match self.value {
            ValueBacking::Vector { v, .. } => {
                let i = self.i as usize;
                if i >= v.len() {
                    None
                } else {
                    self.i += 1;
                    v.get(self.i as usize).cloned()
                }
            }
            ValueBacking::Function { f, .. } => {
                self.i += 1;
                f(self.i - 1, self.columns)
            }
        }
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Register {
    pub handle: Option<Handle>,
    pub magma: Magma,
    #[serde(skip_serializing, skip_deserializing, default)]
    value: Option<ValueBacking>,
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
        self.value.is_none()
    }

    pub fn width(&self) -> usize {
        self.width
    }

    pub fn spilling(&self) -> Option<isize> {
        self.value.as_ref().map(|v| v.spilling())
    }

    pub fn set_value(&mut self, v: Vec<Value>, spilling: isize) -> Result<()> {
        if let Some(ref mut provider) = self.value.as_mut() {
            provider.update_value(v, spilling)
        } else {
            let _ = self.value.insert(ValueBacking::from_vec(
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

    pub fn set_raw_value(&mut self, v: Vec<Value>, spilling: isize) -> Result<()> {
        if let Some(ref mut provider) = self.value.as_mut() {
            provider.update_value(v, spilling)
        } else {
            let _ = self.value.insert(ValueBacking::from_vec(v, spilling));
            Ok(())
        }
    }

    pub fn set_backing(&mut self, v: ValueBacking) -> Result<()> {
        if self.value.is_some() {
            bail!("backing already set");
        }
        self.value = Some(v);
        Ok(())
    }

    pub fn padded_len(&self) -> Option<usize> {
        self.value.as_ref().map(|v| v.padded_len())
    }

    pub fn len(&self) -> Option<usize> {
        self.value.as_ref().map(|v| v.len())
    }

    pub fn get(&self, i: isize, wrap: bool, columns: &ColumnSet) -> Option<Value> {
        self.value.as_ref().and_then(|v| v.get(i, wrap, columns))
    }

    pub fn get_raw(&self, i: isize, wrap: bool, columns: &ColumnSet) -> Option<Value> {
        self.value
            .as_ref()
            .and_then(|v| v.get_raw(i, wrap, columns))
    }

    pub fn concretize(&mut self) {
        if let Some(v) = self.value.take() {
            let _ = self.value.insert(v.concretize());
        }
    }
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct Column {
    pub register: Option<RegisterID>,
    pub shift: i16,
    pub padding_value: Option<Value>,
    pub used: bool,
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
        shift: Option<i16>,
        padding_value: Option<i64>, // TODO: Value
        used: Option<bool>,
        kind: Option<Kind<()>>,
        t: Option<Magma>,
        intrinsic_size_factor: Option<usize>,
        base: Option<Base>,
        handle: Handle,
    ) -> Self {
        Column {
            register,
            shift: shift.unwrap_or(0),
            padding_value: padding_value.map(|v| Value::from(v as usize)),
            used: used.unwrap_or(true),
            kind: kind.unwrap_or(Kind::Phantom),
            t: t.unwrap_or(Magma::Native),
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
    pub(crate) fn module_of<'a, I: std::borrow::Borrow<ColumnRef>, C: IntoIterator<Item = I>>(
        &self,
        cols: C,
    ) -> Option<String> {
        let modules = cols
            .into_iter()
            .map(|c| self.column(c.borrow()).unwrap().handle.module.clone())
            .collect::<HashSet<_>>();
        if modules.len() != 1 {
            None
        } else {
            modules.into_iter().next()
        }
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
            .perspective;

        if current.is_some() {
            bail!("perspective already set")
        } else {
            *current = Some(p.to_string());
            Ok(())
        }
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
            *self.cols.get(h.as_handle()).expect(&h.to_string())
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

    pub(crate) fn new_register(&mut self, handle: Handle, magma: Magma) -> RegisterID {
        self.registers.push(Register {
            handle: Some(handle),
            magma,
            value: None,
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

    pub fn insert_column_and_register(&mut self, mut column: Column) -> Result<ColumnRef> {
        column.register = Some(self.new_register(column.handle.clone(), column.t));
        self.insert_column(column)
    }

    pub fn is_empty(&self) -> bool {
        const IGNORED: &[&str] = &["shift-table", "binary-table", "instruction-decoder"];
        self.iter_cols()
            .filter(|c| !IGNORED.contains(&c.handle.module.as_str()))
            .all(|c| self.registers[c.register.unwrap()].is_empty())
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
        self.register_of(h).value.as_ref()
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
