use crate::{
    compiler::{ColumnRef, Kind, Magma, Node, Type},
    errors,
    pretty::{Base, Pretty},
    structs::Handle,
};
use anyhow::*;
use colored::Colorize;
use itertools::Itertools;
use pairing_ce::{bn256::Fr, ff::Field};
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};

pub type RegisterID = usize;
pub type ColumnID = usize;

#[derive(Default, Clone, Debug, Serialize, Deserialize)]
pub struct Register {
    value: Option<Vec<Fr>>,
    spilling: Option<isize>,
    pub handle: Option<Handle>,
}

impl Register {
    pub fn make_with_spilling(
        f: &mut dyn FnMut(isize) -> Fr,
        len: usize,
        spilling: isize,
    ) -> Vec<Fr> {
        (-spilling..len as isize).map(f).collect()
    }

    pub fn set_value(&mut self, v: Vec<Fr>, spilling: isize) -> Result<()> {
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
                &mut |i| v.get(i as usize).cloned().unwrap_or_else(Fr::zero),
                v.len().try_into().expect("demented size"),
                spilling,
            ));
        }
        Ok(())
    }

    pub fn set_raw_value(&mut self, v: Vec<Fr>, spilling: isize) -> Result<()> {
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

    pub fn value(&self) -> Option<&Vec<Fr>> {
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

    pub fn get(&self, i: isize, wrap: bool) -> Option<&Fr> {
        if i < 0 {
            if wrap {
                let new_i = self.value.as_ref().map(Vec::len).unwrap() as isize + i;
                if new_i < 0 || new_i >= self.padded_len().unwrap() as isize {
                    panic!("abnormal wrapping value")
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

    pub fn get_raw(&self, i: isize, wrap: bool) -> Option<&Fr> {
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
    pub padding_value: Option<i64>,
    pub used: bool,
    pub kind: Kind<()>,
    pub t: Type,
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
            padding_value,
            used: used.unwrap_or(true),
            kind,
            t: Type::Column(t.unwrap_or(Magma::Integer)),
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
    pub registers: Vec<Register>,
    pub spilling: HashMap<String, isize>, // module -> (past-spilling, future-spilling)
}

impl ColumnSet {
    pub(crate) fn module_of<'a, C: IntoIterator<Item = &'a ColumnRef>>(
        &self,
        cols: C,
    ) -> Option<String> {
        let modules = cols
            .into_iter()
            .map(|c| self.get_col(c).unwrap().handle.module.clone())
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

    pub fn get_col(&self, h: &ColumnRef) -> Result<&Column> {
        match &h.0 {
            either::Either::Left(ref handle) => self.by_handle(handle),
            either::Either::Right(id) => self._cols.get(*id).ok_or_else(|| unreachable!()),
        }
    }

    pub fn get_col_mut(&mut self, h: &ColumnRef) -> Option<&mut Column> {
        match &h.0 {
            either::Either::Left(ref handle) => self.by_handle_mut(handle),
            either::Either::Right(id) => self._cols.get_mut(*id),
        }
    }

    pub fn all(&self) -> Vec<ColumnRef> {
        (0..self._cols.len())
            .into_iter()
            .map(ColumnRef::from)
            .collect()
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
        Ok(self.get_col(r)?.handle.perspective.as_ref())
    }

    pub(crate) fn perspective_of<'a, H: IntoIterator<Item = &'a ColumnRef>>(
        &self,
        hs: H,
    ) -> Result<Option<String>> {
        let ps = hs
            .into_iter()
            .filter_map(|h| self.get_col(h).unwrap().handle.perspective.clone())
            .collect::<HashSet<_>>();
        if ps.len() > 1 {
            bail!("no unique perspective")
        }

        Ok(ps.into_iter().next())
    }
    pub fn id_of(&self, handle: &ColumnRef) -> usize {
        match &handle.0 {
            either::Either::Left(handle) => *self.cols.get(handle).unwrap(),
            either::Either::Right(i) => *i,
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = (ColumnRef, &Column)> {
        (0..self._cols.len())
            .into_iter()
            .map(|i| (ColumnRef::from(i), &self._cols[i]))
    }

    pub fn iter_cols(&self) -> impl Iterator<Item = &Column> {
        self._cols.iter()
    }

    pub fn new_register(&mut self, handle: Handle) -> RegisterID {
        self.registers.push(Register {
            handle: Some(handle),
            ..Default::default()
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

    pub fn insert_column(&mut self, column: Column, allow_dup: bool) -> Result<ColumnRef> {
        if self.cols.contains_key(&column.handle) && !allow_dup {
            bail!("{} already exists", column.handle.to_string().red().bold())
        } else {
            let i = self._cols.len();

            self.cols.insert(column.handle.to_owned(), i);
            self._cols.push(column);
            Ok(ColumnRef::from(i))
        }
    }

    pub fn insert_column_and_register(
        &mut self,
        mut column: Column,
        allow_dup: bool,
    ) -> Result<ColumnRef> {
        column.register = Some(self.new_register(column.handle.clone()));
        self.insert_column(column, allow_dup)
    }

    pub fn is_empty(&self) -> bool {
        const IGNORED: &[&str] = &["shift-table", "binary-table", "instruction-decoder"];
        self.iter_cols()
            .filter(|c| !IGNORED.contains(&c.handle.module.as_str()))
            .all(|c| self.registers[c.register.unwrap()].value().is_none())
    }

    fn register_of_mut(&mut self, h: &ColumnRef) -> &mut Register {
        let reg = self.get_col(h).unwrap().register.unwrap();
        &mut self.registers[reg]
    }

    fn register_of(&self, h: &ColumnRef) -> &Register {
        let reg = self.get_col(h).unwrap().register.unwrap();
        &self.registers[reg]
    }

    pub fn get(&self, h: &ColumnRef, i: isize, wrap: bool) -> Option<&Fr> {
        self.register_of(h).get(i, wrap)
    }

    pub fn get_raw(&self, h: &ColumnRef, i: isize, wrap: bool) -> Option<&Fr> {
        self.register_of(h).get_raw(i, wrap)
    }

    pub fn len(&self, h: &ColumnRef) -> Option<usize> {
        self.register_of(h).len()
    }

    pub fn padded_len(&self, h: &ColumnRef) -> Option<usize> {
        self.register_of(h).padded_len()
    }

    pub fn value(&self, h: &ColumnRef) -> Option<&Vec<Fr>> {
        self.register_of(h).value()
    }

    pub fn is_computed(&self, h: &ColumnRef) -> bool {
        self.get_col(h).unwrap().computed
    }

    pub fn set_value(&mut self, h: &ColumnRef, v: Vec<Fr>, spilling: isize) -> Result<()> {
        self.get_col_mut(h).unwrap().computed = true;
        self.register_of_mut(h)
            .set_value(v, spilling)
            .with_context(|| anyhow!("while filling {}", h.pretty()))
    }

    pub fn set_raw_value(&mut self, h: &ColumnRef, v: Vec<Fr>, spilling: isize) -> Result<()> {
        self.get_col_mut(h).unwrap().computed = true;
        self.register_of_mut(h).set_raw_value(v, spilling)
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Computation {
    Composite {
        target: ColumnRef,
        exp: Node,
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
            Computation::Composite { target, .. } => target.to_string(),
            Computation::Interleaved { target, .. } => target.to_string(),
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
}
