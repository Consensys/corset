use crate::{
    compiler::{ColumnRef, Expression, Kind, Magma, Node},
    errors,
    pretty::{Base, Pretty},
    structs::{Field, Handle},
};
use anyhow::*;
use itertools::Itertools;
use owo_colors::OwoColorize;
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};

pub type RegisterID = usize;
pub type ColumnID = usize;

#[derive(Clone, Debug, Serialize, Deserialize, Default)]
pub struct FieldRegister<F: Field> {
    value: Option<Vec<F>>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Register<F: Field> {
    pub handle: Option<Handle>,
    pub magma: Magma,
    // ex: in case of BN256, a register storing 32 bytes elements will have 2 FieldRegister
    values: Vec<FieldRegister<F>>,
    spilling: Option<isize>,
}

impl<F: Field> Register<F> {
    pub fn make_with_spilling(
        f: &mut dyn FnMut(isize) -> F,
        len: usize,
        spilling: isize,
    ) -> Vec<F> {
        (-spilling..len as isize).map(f).collect()
    }

    // pub fn get_row(&self, i: usize) -> Vec<&F> {
    //     self.values
    //         .as_ref()
    //         .unwrap()
    //         .iter()
    //         .map(|v| &v.value[i])
    //         .collect()
    // }

    // pub fn get_row_mut(&mut self, i: usize) -> Vec<&mut F> {
    //     self.values
    //         .as_mut()
    //         .unwrap()
    //         .iter_mut()
    //         .map(|v| &mut v.value[i])
    //         .collect()
    // }

    pub fn set_value(&mut self, v: Vec<F>, spilling: isize) -> Result<()> {
        todo!()
        // assert!(self.spilling.map(|s| s == spilling).unwrap_or(true));
        // self.spilling = Some(spilling);
        // if self.values.is_some() {
        //     assert!(self.values.as_ref().unwrap().len() == v.len());
        //     for (x, y) in self.values.as_mut().unwrap().iter_mut().zip(v.iter()) {
        //         if !x.is_zero() {
        //             bail!("overwriting non-zero value in shared register")
        //         } else {
        //             x.add_assign(y)
        //         }
        //     }
        // } else {
        //     self.values = Some(Self::make_with_spilling(
        //         &mut |i| v.get(i as usize).cloned().unwrap_or_else(F::zero),
        //         v.len(),
        //         spilling,
        //     ));
        // }
        // Ok(())
    }

    pub fn set_raw_value(&mut self, v: Vec<F>, spilling: isize) -> Result<()> {
        todo!()
        // assert!(self.spilling.map(|s| s == spilling).unwrap_or(true));
        // if self.values.is_some() {
        //     assert!(self.values.as_ref().unwrap().len() == v.len());
        //     for (x, y) in self.values.as_mut().unwrap().iter_mut().zip(v.iter()) {
        //         if !x.is_zero() {
        //             bail!("overwriting non-zero value in shared register")
        //         } else {
        //             x.add_assign(y)
        //         }
        //     }
        // } else {
        //     self.values = Some(v);
        // }
        // self.spilling = Some(spilling);
        // Ok(())
    }

    pub fn value(&self) -> Option<&Vec<F>> {
        todo!()
        //self.values.as_ref()
    }

    pub fn padded_len(&self) -> Option<usize> {
        self.values
            .first()
            .map(|v| v.value.as_ref().map(Vec::len))
            .flatten()
    }

    pub fn len(&self) -> Option<usize> {
        self.padded_len()
            .map(|l| l - self.spilling.unwrap() as usize)
    }

    pub fn get(&self, i: isize, wrap: bool) -> Option<&F> {
        todo!()
        // if i < 0 {
        //     if wrap {
        //         let new_i = self.values.as_ref().map(Vec::len).unwrap() as isize + i;
        //         if new_i < 0 || new_i >= self.padded_len().unwrap() as isize {
        //             panic!("abnormal wrapping value {} for {:?}", new_i, self.handle)
        //         }
        //         self.values
        //             .as_ref()
        //             .and_then(|v| v.get((v.len() as isize + i) as usize))
        //     } else if i < -self.spilling.unwrap() {
        //         None
        //     } else {
        //         self.values
        //             .as_ref()
        //             .and_then(|v| v.get((i + self.spilling.unwrap()) as usize))
        //     }
        // } else {
        //     self.values
        //         .as_ref()
        //         .and_then(|v| v.get((i + self.spilling.unwrap()) as usize))
        // }
    }

    pub fn get_raw(&self, i: isize, wrap: bool) -> Option<&F> {
        todo!()
        // if i < 0 {
        //     if wrap {
        //         self.values
        //             .as_ref()
        //             .and_then(|v| v.get((v.len() as isize + i) as usize))
        //     } else {
        //         None
        //     }
        // } else {
        //     self.values
        //         .as_ref()
        //         .and_then(|v| v.get((i + self.spilling.unwrap()) as usize))
        // }
    }
}

#[derive(Debug, Serialize, Clone, Deserialize)]
pub struct Column<F: Field> {
    pub register: Option<RegisterID>,
    pub padding_value: Option<(i64, F)>,
    pub used: bool,
    pub kind: Kind<()>,
    pub t: Magma,
    pub intrinsic_size_factor: Option<usize>,
    pub base: Base,
    pub handle: Handle,
    computed: bool,
}
#[buildstructor::buildstructor]
impl<F: Field> Column<F> {
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
            padding_value: padding_value.map(|x| (x, F::from_str(&x.to_string()).unwrap())),
            used: used.unwrap_or(true),
            kind,
            t: t.unwrap_or(Magma::default()),
            intrinsic_size_factor,
            base: base.unwrap_or(Base::Dec),
            computed: false,
            handle,
        }
    }
}

#[derive(Debug, Default, Serialize, Clone, Deserialize)]
pub struct ColumnSet<F: Field> {
    pub _cols: Vec<Column<F>>,
    pub cols: HashMap<Handle, usize>,
    pub raw_len: HashMap<String, isize>,
    /// a module may have a lower bound on its columns length if it is involved
    /// in range proofs
    pub min_len: HashMap<String, usize>,
    pub registers: Vec<Register<F>>,
    pub spilling: HashMap<String, isize>, // module -> (past-spilling, future-spilling)
}

impl<F: Field> ColumnSet<F> {
    // pub fn merge(&mut self, mut other: Self) {
    //     other.cols.values_mut().for_each(|i| *i += self._cols.len());

    //     self._cols.extend(other._cols);
    //     self.cols.extend(other.cols);
    //     self.raw_len.extend(other.raw_len);
    //     self.min_len.extend(other.min_len);
    //     self.registers.extend(other.registers);
    //     self.spilling.extend(other.spilling);
    // }

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

    pub fn get_col(&self, h: &ColumnRef) -> Result<&Column<F>> {
        if h.is_id() {
            self._cols.get(h.as_id()).ok_or_else(|| unreachable!())
        } else if h.is_handle() {
            self.by_handle(h.as_handle())
        } else {
            unreachable!()
        }
    }

    pub fn get_register(&self, h: &RegisterRef) -> Option<&Register<F>> {
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

    pub fn get_col_mut(&mut self, h: &ColumnRef) -> Option<&mut Column<F>> {
        if h.is_id() {
            self._cols.get_mut(h.as_id())
        } else if h.is_handle() {
            self.by_handle_mut(h.as_handle())
        } else {
            unreachable!()
        }
    }

    pub fn get_register_mut(&mut self, h: &RegisterRef) -> Option<&mut Register<F>> {
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

    pub fn by_handle(&self, handle: &Handle) -> Result<&Column<F>> {
        self.cols
            .get(handle)
            .and_then(|i| self._cols.get(*i))
            .ok_or_else(|| anyhow!(errors::CompileError::NotFound(handle.to_owned())))
    }

    pub fn by_handle_mut(&mut self, handle: &Handle) -> Option<&mut Column<F>> {
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
    pub fn id_of(&self, h: &ColumnRef) -> usize {
        if h.is_id() {
            h.as_id()
        } else if h.is_handle() {
            *self.cols.get(h.as_handle()).unwrap()
        } else {
            unreachable!()
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = (ColumnRef, &Column<F>)> {
        (0..self._cols.len()).map(|i| (ColumnRef::from(i), &self._cols[i]))
    }

    pub fn iter_module<'a>(
        &'a self,
        module: &'a str,
    ) -> impl Iterator<Item = (ColumnRef, &Column<F>)> + 'a {
        self.iter().filter(move |c| c.1.handle.module == module)
    }

    pub fn iter_cols(&self) -> impl Iterator<Item = &Column<F>> {
        self._cols.iter()
    }

    pub fn new_register(&mut self, handle: Handle, magma: Magma) -> RegisterID {
        self.registers.push(Register {
            handle: Some(handle),
            magma,
            values: vec![FieldRegister::default(); magma.repr_count::<F>()],
            spilling: None,
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

    pub fn insert_column(&mut self, column: Column<F>) -> Result<ColumnRef> {
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

    pub fn insert_column_and_register(&mut self, mut column: Column<F>) -> Result<ColumnRef> {
        column.register = Some(self.new_register(column.handle.clone(), column.t));
        self.insert_column(column)
    }

    pub fn is_empty(&self) -> bool {
        const IGNORED: &[&str] = &["shift-table", "binary-table", "instruction-decoder"];
        self.iter_cols()
            .filter(|c| !IGNORED.contains(&c.handle.module.as_str()))
            .all(|c| self.registers[c.register.unwrap()].value().is_none())
    }

    fn register_of_mut(&mut self, h: &ColumnRef) -> &mut Register<F> {
        let reg = self.get_col(h).unwrap().register.unwrap();
        &mut self.registers[reg]
    }

    fn register_of(&self, h: &ColumnRef) -> &Register<F> {
        let reg = self.get_col(h).unwrap().register.unwrap();
        &self.registers[reg]
    }

    pub fn get(&self, h: &ColumnRef, i: isize, wrap: bool) -> Option<&F> {
        self.register_of(h).get(i, wrap)
    }

    pub fn get_raw(&self, h: &ColumnRef, i: isize, wrap: bool) -> Option<&F> {
        self.register_of(h).get_raw(i, wrap)
    }

    pub fn len(&self, h: &ColumnRef) -> Option<usize> {
        self.register_of(h).len()
    }

    pub fn padded_len(&self, h: &ColumnRef) -> Option<usize> {
        self.register_of(h).padded_len()
    }

    pub fn value(&self, h: &ColumnRef) -> Option<&Vec<F>> {
        self.register_of(h).value()
    }

    pub fn is_computed(&self, h: &ColumnRef) -> bool {
        self.get_col(h).unwrap().computed
    }

    pub fn set_column_value(&mut self, h: &ColumnRef, v: Vec<F>, spilling: isize) -> Result<()> {
        self.get_col_mut(h).unwrap().computed = true;
        self.register_of_mut(h)
            .set_value(v, spilling)
            .with_context(|| anyhow!("while filling {}", h.pretty()))
    }

    pub fn set_register_value(
        &mut self,
        h: &RegisterRef,
        v: Vec<F>,
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

    pub fn set_raw_value(&mut self, h: &ColumnRef, v: Vec<F>, spilling: isize) -> Result<()> {
        self.get_col_mut(h).unwrap().computed = true;
        self.register_of_mut(h).set_raw_value(v, spilling)
    }
}

type RegisterRef = ColumnRef;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Computation<F: Field> {
    Composite {
        target: ColumnRef,
        exp: Node<Expression<F>, F>,
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

impl<F: Field> std::fmt::Display for Computation<F> {
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
impl<F: Field> Computation<F> {
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

    pub fn is_interleaved(&self) -> bool {
        matches!(self, Computation::Interleaved { .. })
    }
}
