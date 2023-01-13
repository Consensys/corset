use crate::compiler::{Handle, Kind, Node, Type};
use anyhow::{anyhow, Result};
use colored::Colorize;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct Column<T: Clone> {
    value: Option<Vec<T>>,
    pub padding: Option<Vec<T>>,
    pub used: bool,
    pub kind: Kind<()>,
    pub t: Type,
}
impl<T: Clone> Column<T> {
    pub fn len(&self) -> Option<usize> {
        match (
            self.padding.as_ref().map(|p| p.len()),
            self.value.as_ref().map(|v| v.len()),
        ) {
            (Some(p), Some(v)) => Some(p + v),
            (Some(p), None) => Some(p),
            (None, None) => None,
            (None, Some(v)) => Some(v),
        }
    }
    pub fn is_computed(&self) -> bool {
        self.value.is_some() || self.padding.is_some()
    }

    pub fn get(&self, i: isize, wrap: bool) -> Option<&T> {
        if i < 0 {
            if wrap {
                let new_i = (i + self.len().unwrap() as isize) % self.len().unwrap() as isize;
                self.get(new_i, wrap)
            } else {
                None
            }
        } else {
            let pad_len = self.padding.as_ref().map(|v| v.len()).unwrap_or(0) as isize;
            if i < pad_len {
                Some(&self.padding.as_ref().unwrap()[i as usize])
            } else {
                self.value
                    .as_ref()
                    .and_then(|v| v.get((i - pad_len) as usize))
            }
        }
    }

    pub fn set_value(&mut self, v: Vec<T>) {
        self.value = Some(v);
    }

    pub fn value(&self) -> Option<&Vec<T>> {
        self.value.as_ref()
    }
}

#[derive(Debug, Default, Serialize, Deserialize, Clone)]
pub struct ColumnSet<T: Clone> {
    pub _cols: Vec<Column<T>>,
    pub cols: HashMap<String, HashMap<String, usize>>, // Module -> (Name, ColumnID)
    pub spilling: HashMap<String, (isize, isize)>,     // Module -> (past_span, future_span)
}

impl<T: Clone> ColumnSet<T> {
    pub fn by_handle(&self, handle: &Handle) -> Option<&Column<T>> {
        self.cols
            .get(&handle.module)
            .and_then(|m| m.get(&handle.name))
            .and_then(|i| self._cols.get(*i))
    }

    pub fn by_handle_mut(&mut self, handle: &Handle) -> Option<&mut Column<T>> {
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

    pub fn iter(&self) -> impl Iterator<Item = (Handle, &Column<T>)> {
        self.cols.iter().flat_map(|(module, columns)| {
            columns
                .iter()
                .map(|(name, i)| (Handle::new(module.clone(), name), &self._cols[*i]))
        })
    }
}

impl<T: Ord + Clone> ColumnSet<T> {
    pub fn get(&self, handle: &Handle) -> Result<&Column<T>> {
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

    pub fn get_mut(&mut self, handle: &Handle) -> Result<&mut Column<T>> {
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
                padding: None,
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

impl<T: Clone> std::convert::From<HashMap<String, HashMap<String, Column<T>>>> for ColumnSet<T> {
    fn from(x: HashMap<String, HashMap<String, Column<T>>>) -> Self {
        let mut r = ColumnSet {
            cols: Default::default(),
            _cols: Vec::with_capacity(x.values().flat_map(|y| y.values()).count()),
            spilling: Default::default(),
        };
        for (module, columns) in x.into_iter() {
            for (name, column) in columns.into_iter() {
                let i = r._cols.len();
                r._cols.push(column);
                r.cols.entry(module.to_owned()).or_default().insert(name, i);
            }
        }
        // r.build_spilling();
        r
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
        }
    }
    pub fn add_id_to_handles(&mut self, set_id: &dyn Fn(&mut Handle)) {
        if let Computation::Composite { exp, .. } = self {
            exp.add_id_to_handles(set_id)
        }
    }
}
