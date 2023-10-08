use crate::{
    compiler::{ColumnRef, Kind, Magma, Node},
    errors,
    pretty::{Base, Pretty},
    structs::Handle,
};
use anyhow::*;
use either::Either;
use itertools::Itertools;
use num_bigint::BigInt;
use num_traits::{FromPrimitive, One, Zero};
use owo_colors::OwoColorize;
use pairing_ce::{
    bn256::{Fr, FrRepr},
    ff::{Field, PrimeField},
};
use serde::{Deserialize, Serialize};
use std::{
    collections::{HashMap, HashSet},
    str::FromStr,
};

pub type RegisterID = usize;
pub type ColumnID = usize;

#[derive(Debug, Clone, Serialize, Deserialize, Eq, Ord, Hash)]
pub struct Value(Either<Fr, BigInt>);
impl Value {
    pub fn to_string(&self) -> String {
        match self.0 {
            Either::Left(f) => f.to_string(),
            Either::Right(_) => String::from("TODO:to_string"),
        }
    }

    pub fn is_zero(&self) -> bool {
        match &self.0 {
            Either::Left(f) => f.is_zero(),
            Either::Right(i) => i.is_zero(),
        }
    }

    pub fn is_one(&self) -> bool {
        match &self.0 {
            Either::Left(f) => *f == Fr::one(),
            Either::Right(fs) => fs.is_one(),
        }
    }

    pub(crate) fn zero() -> Self {
        Value(Either::Left(Fr::zero()))
    }

    pub(crate) fn one() -> Self {
        Value(Either::Left(Fr::one()))
    }

    pub(crate) fn add_assign(&mut self, other: &Value) {
        match self.0 {
            Either::Left(ref mut f1) => match other.0 {
                Either::Left(f2) => f1.add_assign(&f2),
                Either::Right(_) => unreachable!(),
            },
            Either::Right(_) => unreachable!(),
        }
    }

    pub(crate) fn sub_assign(&mut self, other: &Value) {
        match self.0 {
            Either::Left(ref mut f1) => match other.0 {
                Either::Left(f2) => f1.sub_assign(&f2),
                Either::Right(_) => unreachable!(),
            },
            Either::Right(_) => unreachable!(),
        }
    }

    pub(crate) fn mul_assign(&mut self, other: &Value) {
        match self.0 {
            Either::Left(ref mut f1) => match other.0 {
                Either::Left(f2) => f1.mul_assign(&f2),
                Either::Right(_) => unreachable!(),
            },
            Either::Right(_) => unreachable!(),
        }
    }

    pub(crate) fn negate(&mut self) {
        todo!()
    }

    pub(crate) fn inverse(&self) -> Option<Value> {
        match &self.0 {
            Either::Left(f) => f.inverse().map(|i| Value(Either::Left(i))),
            Either::Right(_) => panic!("can not inverse ExoValue"),
        }
    }

    pub(crate) fn from_str(s: &str) -> Value {
        let int = s.parse::<BigInt>().unwrap();
        Value::from(&int)
    }

    pub fn exoize(&self) -> Value {
        match self.0 {
            Either::Left(f) => Value(Either::Right(BigInt::from_str(&f.to_string()).unwrap())),
            Either::Right(_) => self.clone(),
        }
    }

    pub(crate) fn into_repr(&self) -> impl Iterator<Item = u64> {
        let us = match &self.0 {
            Either::Left(f) => f.into_repr().0.to_vec(),
            Either::Right(_) => todo!(),
        };
        us.into_iter()
    }

    pub(crate) fn to_fr(&self) -> Vec<Fr> {
        match &self.0 {
            Either::Left(f) => vec![f.clone()],
            Either::Right(int) => {
                if int.bits() <= crate::constants::FIELD_BITSIZE as u64 {
                    vec![Fr::from_str(&int.to_string()).unwrap()]
                } else {
                    assert!(int.sign() != num_bigint::Sign::Minus);
                    let bs = int.to_bytes_le();
                    let mut r = Vec::new();
                    for bytes in &bs.1.iter().chunks(crate::constants::FIELD_BITSIZE / 8) {
                        let bb = bytes.cloned().collect_vec();
                        dbg!(&bb);
                        let small_big_int = BigInt::from_bytes_le(num_bigint::Sign::Plus, &bb);
                        r.push(Fr::from_str(&small_big_int.to_string()).unwrap());
                    }
                    r.reverse();
                    r
                }
            }
        }
    }

    pub(crate) fn normalize(&self) -> Value {
        match &self.0 {
            Either::Left(f) => {
                let mut r = f.inverse().unwrap();
                r.mul_assign(&f);
                Value(Either::Left(r))
            }
            Either::Right(i) => Value(Either::Right(if i.is_zero() {
                BigInt::zero()
            } else {
                BigInt::one()
            })),
        }
    }
}
impl std::default::Default for Value {
    fn default() -> Value {
        Value(Either::Left(Fr::zero()))
    }
}
impl From<&BigInt> for Value {
    fn from(int: &BigInt) -> Self {
        Value(Either::Right(int.clone()))
    }
}
impl From<Fr> for Value {
    fn from(f: Fr) -> Self {
        Value(Either::Left(f))
    }
}
impl From<usize> for Value {
    fn from(x: usize) -> Self {
        Value(Either::Left(Fr::from_str(&x.to_string()).unwrap()))
    }
}
impl From<&str> for Value {
    fn from(x: &str) -> Self {
        Value(Either::Left(Fr::from_str(x).unwrap()))
    }
}
impl Pretty for Value {
    fn pretty(&self) -> String {
        self.0
            .as_ref()
            .either(|f| f.pretty(), |i| format!("E{}", i))
    }

    fn pretty_with_base(&self, base: Base) -> String {
        self.0.as_ref().either(
            |f| f.pretty_with_base(base),
            |i| {
                format!(
                    "E{}",
                    i.to_str_radix(match base {
                        Base::Dec => 10,
                        Base::Hex => 16,
                        Base::Bin | Base::Bool | Base::Loob => 2,
                        Base::Bytes | Base::OpCode => 16,
                    })
                )
            },
        )
    }
}
impl std::cmp::PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        todo!()
    }
}
impl std::cmp::PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match &self.0 {
            Either::Left(f) => match &other.0 {
                Either::Left(o) => f.eq(&o),
                Either::Right(_) => todo!(),
            },
            Either::Right(fs) => match &other.0 {
                Either::Left(_) => todo!("{} -- {}", self, other),
                Either::Right(os) => fs.cmp(os).is_eq(),
            },
        }
    }
}
impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.0 {
            Either::Left(fr) => write!(f, "{}", fr),
            Either::Right(i) => write!(f, "E{}", i),
        }
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct FieldRegister {
    pub handle: Handle,
    value: Option<Vec<Fr>>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Register {
    pub handle: Option<Handle>,
    pub magma: Magma,
    value: Option<Vec<Value>>,
    spilling: Option<isize>,
    width: usize,
}

impl Register {
    pub fn width(&self) -> usize {
        self.width
    }

    pub fn make_with_spilling(
        f: &mut dyn FnMut(isize) -> Value,
        len: usize,
        spilling: isize,
    ) -> Vec<Value> {
        (-spilling..len as isize).map(f).collect()
    }

    pub fn set_value(&mut self, v: Vec<Value>, spilling: isize) -> Result<()> {
        assert!(self.spilling.map(|s| s == spilling).unwrap_or(true));
        self.spilling = Some(spilling);
        if self.value.is_some() {
            assert!(self.value.as_ref().unwrap().len() == v.len());
            for (x, y) in self.value.as_mut().unwrap().iter_mut().zip(v.iter()) {
                if !x.is_zero() {
                    bail!("overwriting non-zero value in shared register")
                } else {
                    x.add_assign(y)
                }
            }
        } else {
            self.value = Some(Self::make_with_spilling(
                &mut |i| v.get(i as usize).cloned().unwrap_or_else(Value::zero),
                v.len(),
                spilling,
            ));
        }
        Ok(())
    }

    pub fn set_raw_value(&mut self, v: Vec<Value>, spilling: isize) -> Result<()> {
        assert!(self.spilling.map(|s| s == spilling).unwrap_or(true));
        if self.value.is_some() {
            assert!(self.value.as_ref().unwrap().len() == v.len());
            for (x, y) in self.value.as_mut().unwrap().iter_mut().zip(v.iter()) {
                if !x.is_zero() {
                    bail!("overwriting non-zero value in shared register")
                } else {
                    x.add_assign(y)
                }
            }
        } else {
            self.value = Some(v);
        }
        self.spilling = Some(spilling);
        Ok(())
    }

    pub fn value(&self) -> Option<&Vec<Value>> {
        self.value.as_ref()
    }

    pub fn padded_len(&self) -> Option<usize> {
        self.value.as_ref().map(|v| v.len())
    }

    pub fn len(&self) -> Option<usize> {
        self.value
            .as_ref()
            .map(|v| v.len() - self.spilling.unwrap() as usize)
    }

    pub fn get(&self, i: isize, wrap: bool) -> Option<&Value> {
        if i < 0 {
            if wrap {
                let new_i = self.value.as_ref().map(Vec::len).unwrap() as isize + i;
                if new_i < 0 || new_i >= self.padded_len().unwrap() as isize {
                    panic!("abnormal wrapping value {} for {:?}", new_i, self.handle)
                }
                self.value
                    .as_ref()
                    .and_then(|v| v.get((v.len() as isize + i) as usize))
            } else if i < -self.spilling.unwrap() {
                None
            } else {
                self.value
                    .as_ref()
                    .and_then(|v| v.get((i + self.spilling.unwrap()) as usize))
            }
        } else {
            self.value
                .as_ref()
                .and_then(|v| v.get((i + self.spilling.unwrap()) as usize))
        }
    }

    pub fn get_raw(&self, i: isize, wrap: bool) -> Option<&Value> {
        if i < 0 {
            if wrap {
                self.value
                    .as_ref()
                    .and_then(|v| v.get((v.len() as isize + i) as usize))
            } else {
                None
            }
        } else {
            self.value
                .as_ref()
                .and_then(|v| v.get((i + self.spilling.unwrap()) as usize))
        }
    }
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct Column {
    pub register: Option<RegisterID>,
    pub padding_value: Option<(i64, Fr)>, // TODO: convert to Value
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
        padding_value: Option<i64>,
        used: Option<bool>,
        kind: Kind<()>,
        t: Option<Magma>,
        intrinsic_size_factor: Option<usize>,
        base: Option<Base>,
        handle: Handle,
    ) -> Self {
        Column {
            register,
            padding_value: padding_value.map(|x| (x, Fr::from_str(&x.to_string()).unwrap())),
            used: used.unwrap_or(true),
            kind,
            t: t.unwrap_or(Magma::Native),
            intrinsic_size_factor,
            base: base.unwrap_or(Base::Dec),
            computed: false,
            handle,
        }
    }
}

#[derive(Debug, Default, Serialize, Deserialize, Clone)]
pub struct ColumnSet {
    pub _cols: Vec<Column>,
    pub cols: HashMap<Handle, usize>,
    pub raw_len: HashMap<String, isize>,
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

    pub(crate) fn module(&self, h: &ColumnRef) -> &str {
        &self.column(h).unwrap().handle.module
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

    pub fn register_by_id(&self, id: RegisterID) -> Option<&Register> {
        self.registers.get(id)
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
            *self.cols.get(h.as_handle()).unwrap()
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
            spilling: None,
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
            panic!(
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
            .all(|c| self.registers[c.register.unwrap()].value().is_none())
    }

    pub fn register_of_mut(&mut self, h: &ColumnRef) -> &mut Register {
        let reg = self.column(h).unwrap().register.unwrap();
        &mut self.registers[reg]
    }

    pub fn register_of(&self, h: &ColumnRef) -> &Register {
        let reg = self.column(h).unwrap().register.unwrap();
        &self.registers[reg]
    }

    pub fn get(&self, h: &ColumnRef, i: isize, wrap: bool) -> Option<&Value> {
        self.register_of(h).get(i, wrap)
    }

    pub fn get_raw(&self, h: &ColumnRef, i: isize, wrap: bool) -> Option<&Value> {
        self.register_of(h).get_raw(i, wrap)
    }

    pub fn len(&self, h: &ColumnRef) -> Option<usize> {
        self.register_of(h).len()
    }

    pub fn padded_len(&self, h: &ColumnRef) -> Option<usize> {
        self.register_of(h).padded_len()
    }

    pub fn value(&self, h: &ColumnRef) -> Option<&Vec<Value>> {
        self.register_of(h).value()
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
}

type RegisterRef = ColumnRef;

// TODO: add a targets() function to automatize computation insertion
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Computation {
    Composite {
        target: ColumnRef,
        exp: Node,
    },
    ExoAddition {
        sources: [Node; 2],
        target: ColumnRef,
    },
    ExoMultiplication {
        sources: [Node; 2],
        target: ColumnRef,
    },
    ExoConstant {
        value: BigInt,
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
            Computation::ExoAddition { sources, target } => {
                write!(f, "+ {:?} -> {}", sources, target)
            }
            Computation::ExoMultiplication { sources, target } => {
                write!(f, "× {:?} -> {}", sources, target)
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
            | Computation::ExoAddition { target, .. }
            | Computation::ExoMultiplication { target, .. }
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
