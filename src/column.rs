use crate::compiler::{Handle, Kind, Node, Type};
use anyhow::{anyhow, Result};
use colored::Colorize;
use pairing_ce::{bn256::Fr, ff::Field};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct Column {
    value: Option<Vec<Fr>>,
    pub spilling: isize,
    pub used: bool,
    pub kind: Kind<()>,
    pub t: Type,
}
impl Column {
    pub fn make_with_spilling(
        f: &mut dyn FnMut(isize) -> Fr,
        len: usize,
        spilling: isize,
    ) -> Vec<Fr> {
        (-spilling..len as isize).map(f).collect()
    }

    pub fn padded_len(&self) -> Option<usize> {
        self.value.as_ref().map(|v| v.len())
    }

    pub fn len(&self) -> Option<usize> {
        self.value
            .as_ref()
            .map(|v| v.len() - self.spilling as usize)
    }

    pub fn is_computed(&self) -> bool {
        self.value.is_some()
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
            } else if i < -self.spilling {
                None
            } else {
                self.value
                    .as_ref()
                    .and_then(|v| v.get((i + self.spilling) as usize))
            }
        } else {
            self.value
                .as_ref()
                .and_then(|v| v.get((i + self.spilling) as usize))
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
                .and_then(|v| v.get((i + self.spilling) as usize))
        }
    }

    pub fn set_value(&mut self, v: Vec<Fr>, spilling: isize) {
        self.value = Some(Column::make_with_spilling(
            &mut |i| {
                if i < 0 {
                    Fr::zero()
                } else {
                    v.get(i as usize).cloned().unwrap_or_else(Fr::zero)
                }
            },
            v.len(),
            spilling,
        ));
        self.spilling = spilling;
    }

    pub fn set_raw_value(&mut self, v: Vec<Fr>, spilling: isize) {
        self.value = Some(v);
        self.spilling = spilling;
    }

    pub fn value(&self) -> Option<&Vec<Fr>> {
        self.value.as_ref()
    }
}

#[derive(Debug, Default, Serialize, Deserialize, Clone)]
pub struct ColumnSet {
    pub _cols: Vec<Column>,
    pub cols: HashMap<String, HashMap<String, usize>>, // Module -> (Name, ColumnID)
    pub spilling: HashMap<String, (isize, isize)>,     // Module -> (past_span, future_span)
    pub raw_len: HashMap<String, isize>,
}

impl ColumnSet {
    pub fn by_handle(&self, handle: &Handle) -> Option<&Column> {
        self.cols
            .get(&handle.module)
            .and_then(|m| m.get(&handle.name))
            .and_then(|i| self._cols.get(*i))
    }

    pub fn by_handle_mut(&mut self, handle: &Handle) -> Option<&mut Column> {
        self.cols
            .get(&handle.module)
            .and_then(|m| m.get(&handle.name))
            .and_then(|i| self._cols.get_mut(*i))
    }

    pub fn handles(&self) -> Vec<Handle> {
        self.cols
            .keys()
            .flat_map(|m| self.cols[m].keys().map(|c| Handle::new(m.clone(), c)))
            .collect()
    }

    pub fn id_of(&self, handle: &Handle) -> usize {
        *self
            .cols
            .get(&handle.module)
            .and_then(|m| m.get(&handle.name))
            .unwrap()
    }

    pub fn iter(&self) -> impl Iterator<Item = (Handle, &Column)> {
        self.cols.iter().flat_map(|(module, columns)| {
            columns
                .iter()
                .map(|(name, i)| (Handle::new(module.clone(), name), &self._cols[*i]))
        })
    }

    pub fn get(&self, handle: &Handle) -> Result<&Column> {
        self.cols
            .get(&handle.module)
            .ok_or_else(|| anyhow!("module `{}` unknwown", handle.module))?
            .get(&handle.name)
            .ok_or_else(|| {
                anyhow!(
                    "column {} not found in module {}",
                    handle.name.red().bold(),
                    handle.module.blue()
                )
            })
            .map(|i| &self._cols[*i])
    }

    pub fn get_mut(&mut self, handle: &Handle) -> Result<&mut Column> {
        self.cols
            .get_mut(&handle.module)
            .ok_or_else(|| anyhow!("module `{}` unknwown", handle.module))?
            .get_mut(&handle.name)
            .ok_or_else(|| {
                anyhow!(
                    "column {} not found in module {}",
                    handle.name.red().bold(),
                    handle.module.blue()
                )
            })
            .map(|i| &mut self._cols[*i])
    }

    pub fn insert_column(
        &mut self,
        handle: &Handle,
        t: Type,
        used: bool,
        kind: Kind<()>,
        allow_dup: bool,
    ) -> Result<usize> {
        if self
            .cols
            .get(&handle.module)
            .map(|module| module.contains_key(&handle.name))
            .unwrap_or(false)
            && !allow_dup
        {
            Err(anyhow!(
                "{} already exists",
                handle.to_string().red().bold()
            ))
        } else {
            let i = self._cols.len();
            self._cols.push(Column {
                value: None,
                spilling: 0,
                used,
                t,
                kind,
            });
            self.cols
                .entry(handle.module.to_owned())
                .or_default()
                .insert(handle.name.to_owned(), i);
            Ok(i)
        }
    }

    pub fn insert_array(
        &mut self,
        handle: &Handle,
        range: &[usize],
        t: Type,
        allow_dup: bool,
    ) -> Result<()> {
        for i in range.iter() {
            self.insert_column(&handle.ith(*i), t, true, Kind::Atomic, allow_dup)?;
        }
        Ok(())
    }

    pub fn is_empty(&self) -> bool {
        const IGNORED: &[&str] = &[
            "shift-table",
            "binary-table",
            "rom",
            "instruction-decoder",
            "fuse",
        ];
        self.iter()
            .filter(|(h, _)| !IGNORED.contains(&h.module.as_str()))
            .all(|(_, c)| c.value().is_none())
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Computation {
    Composite {
        target: Handle,
        exp: Node,
    },
    Interleaved {
        target: Handle,
        froms: Vec<Handle>,
    },
    Sorted {
        froms: Vec<Handle>,
        tos: Vec<Handle>,
    },
    CyclicFrom {
        target: Handle,
        froms: Vec<Handle>,
        modulo: usize,
    },
    SortingConstraints {
        ats: Vec<Handle>,
        eq: Handle,
        delta: Handle,
        delta_bytes: Vec<Handle>,
        signs: Vec<bool>,
        froms: Vec<Handle>,
        sorted: Vec<Handle>,
    },
}
impl Computation {
    pub fn target(&self) -> String {
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
    pub fn add_id_to_handles(&mut self, set_id: &dyn Fn(&mut Handle)) {
        if let Computation::Composite { exp, .. } = self {
            exp.add_id_to_handles(set_id)
        }
    }
}
