use crate::compiler::{Expression, Handle, Kind, Type};
use eyre::*;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct Column<T: Clone> {
    value: Option<Vec<T>>,
    pub kind: Kind<()>,
    pub t: Type,
}
impl<T: Clone> Column<T> {
    pub fn len(&self) -> Option<usize> {
        self.value.as_ref().map(|v| v.len())
    }
    pub fn is_computed(&self) -> bool {
        self.value.is_some()
    }

    pub fn get(&self, i: isize, wrap: bool) -> Option<&T> {
        fn get_rel<T>(v: &[T], i: isize, wrap: bool) -> Option<&T> {
            if wrap && i < 0 {
                v.get(((i + v.len() as isize) % v.len() as isize) as usize)
            } else {
                v.get(i as usize)
            }
        }

        self.value.as_ref().and_then(|v| get_rel(v, i, wrap))
    }

    pub fn set_value(&mut self, v: Vec<T>) {
        self.value = Some(v);
    }

    pub fn value(&self) -> Option<Vec<T>> {
        self.value.clone()
    }

    pub fn map(&mut self, f: &dyn Fn(&mut Vec<T>)) {
        self.value.as_mut().map(f);
    }
}

#[derive(Debug, Default, Serialize, Deserialize, Clone)]
pub struct ColumnSet<T: Clone> {
    pub _cols: Vec<Column<T>>,
    pub cols: HashMap<String, HashMap<String, usize>>, // Module -> Name -> Column
}

impl<T: Clone> ColumnSet<T> {
    pub fn id_of(&self, handle: &Handle) -> usize {
        *self
            .cols
            .get(&handle.module)
            .and_then(|m| m.get(&handle.name))
            .unwrap()
    }

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

    pub fn iter(&self) -> impl Iterator<Item = (Handle, &Column<T>)> {
        self.cols.iter().flat_map(|(module, columns)| {
            columns
                .iter()
                .map(|(name, i)| (Handle::new(module.to_owned(), name), &self._cols[*i]))
        })
    }

    pub fn columns_mut(&mut self) -> impl Iterator<Item = &mut Column<T>> {
        self._cols.iter_mut()
    }
}

impl<T: Ord + Clone> ColumnSet<T> {
    pub fn get(&self, handle: &Handle) -> Result<&Column<T>> {
        self.cols
            .get(&handle.module)
            .ok_or_else(|| eyre!("module `{}` unknwown", handle.module))?
            .get(&handle.name)
            .ok_or_else(|| {
                eyre!(
                    "column `{}` not found in module `{}`",
                    handle.name,
                    handle.module
                )
            })
            .and_then(|i| Ok(&self._cols[*i]))
    }

    pub fn get_mut(&mut self, handle: &Handle) -> Result<&mut Column<T>> {
        self.cols
            .get_mut(&handle.module)
            .ok_or_else(|| eyre!("module `{}` unknwown", handle.module))?
            .get_mut(&handle.name)
            .ok_or_else(|| {
                eyre!(
                    "column `{}` not found in module `{}`",
                    handle.name,
                    handle.module
                )
            })
            .and_then(|i| Ok(&mut self._cols[*i]))
    }

    pub fn insert_column(
        &mut self,
        handle: &Handle,
        t: Type,
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
            Err(eyre!("`{}` already exists", handle))
        } else {
            let i = self._cols.len();
            self._cols.push(Column {
                value: None,
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
            self.insert_column(&handle.ith(*i), t, Kind::Atomic, allow_dup)?;
        }
        Ok(())
    }

    pub fn len(&self) -> usize {
        let lens = self
            ._cols
            .iter()
            .filter_map(|c| c.len())
            .collect::<Vec<_>>();

        if lens.is_empty() {
            return 0;
        }

        *lens.iter().max().unwrap()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

impl<T: Clone> std::convert::From<HashMap<String, HashMap<String, Column<T>>>> for ColumnSet<T> {
    fn from(x: HashMap<String, HashMap<String, Column<T>>>) -> Self {
        let mut r = ColumnSet {
            cols: Default::default(),
            _cols: Vec::with_capacity(x.values().flat_map(|y| y.values()).count()),
        };
        for (module, columns) in x.into_iter() {
            for (name, column) in columns.into_iter() {
                let i = r._cols.len();
                r._cols.push(column);
                r.cols.entry(module.to_owned()).or_default().insert(name, i);
            }
        }
        r
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Computation {
    Composite {
        target: Handle,
        exp: Expression,
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
}
