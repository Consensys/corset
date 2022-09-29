use crate::compiler::{Expression, Handle, Type};
use eyre::*;
use std::collections::HashMap;

#[derive(Debug, Default)]
pub struct ColumnSet<T> {
    pub cols: HashMap<String, HashMap<String, Column<T>>>, // Module -> Name -> Column
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
    }

    pub fn len(&self) -> usize {
        let lens = self
            .cols
            .values()
            .flat_map(|m| m.values())
            .filter_map(|c| c.len())
            .collect::<Vec<_>>();

        if lens.is_empty() {
            return 0;
        }

        if !lens.windows(2).all(|w| w[0] == w[1]) {
            // panic!("different columns size found: {:?}", lens)
        }

        *lens.iter().max().unwrap()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

impl<T> std::convert::From<HashMap<String, HashMap<String, Column<T>>>> for ColumnSet<T> {
    fn from(x: HashMap<String, HashMap<String, Column<T>>>) -> Self {
        ColumnSet { cols: x }
    }
}

impl<T: std::cmp::Ord + Clone> ColumnSet<T> {
    fn insert_column(
        &mut self,
        module: &str,
        name: &str,
        c: Column<T>,
        allow_dup: bool,
    ) -> Result<()> {
        if self
            .cols
            .get(module)
            .map(|module| module.contains_key(name))
            .unwrap_or(false)
            && !allow_dup
        {
            Err(eyre!("`{}/{}` already exists", module, name))
        } else {
            self.cols
                .entry(module.into())
                .or_default()
                .insert(name.into(), c);
            Ok(())
        }
    }

    pub fn insert_atomic(&mut self, handle: &Handle, t: Type, allow_dup: bool) -> Result<()> {
        self.insert_column(
            handle.module.as_ref(),
            handle.name.as_ref(),
            Column::atomic(vec![], t),
            allow_dup,
        )
    }

    pub fn insert_array(
        &mut self,
        handle: &Handle,
        t: Type,
        range: &[usize],
        allow_dup: bool,
    ) -> Result<()> {
        self.insert_column(
            handle.module.as_ref(),
            handle.name.as_ref(),
            Column::Array {
                range: range.to_vec(),
                values: Default::default(),
                t,
            },
            allow_dup,
        )
    }

    pub fn insert_composite(
        &mut self,
        handle: &Handle,
        e: &Expression,
        allow_dup: bool,
    ) -> Result<()> {
        self.insert_column(
            handle.module.as_ref(),
            handle.name.as_ref(),
            Column::composite(e),
            allow_dup,
        )
    }

    pub fn insert_sorted<S1: AsRef<str>, S2: AsRef<str>>(
        &mut self,
        handle: &Handle,
        from: &[Handle],
        to: &[Handle],
        allow_dup: bool,
    ) -> Result<()> {
        self.insert_column(
            handle.module.as_ref(),
            handle.name.as_ref(),
            Column::Sorted {
                values: Default::default(),
                froms: from.iter().map(|n| n.to_owned()).collect::<Vec<_>>(),
                tos: to.iter().map(|n| n.to_owned()).collect::<Vec<_>>(),
            },
            allow_dup,
        )
    }

    pub fn insert_interleaved(
        &mut self,
        handle: &Handle,
        cols: &[Handle],
        allow_dup: bool,
    ) -> Result<()> {
        self.insert_column(
            handle.module.as_ref(),
            handle.name.as_ref(),
            Column::interleaved(cols),
            allow_dup,
        )
    }
}

#[derive(Debug, Clone)]
pub enum Column<T> {
    Atomic {
        value: Option<Vec<T>>,
        t: Type,
    },
    Composite {
        value: Option<Vec<T>>,
        exp: Expression,
    },
    Interleaved {
        value: Option<Vec<T>>,
        froms: Vec<Handle>,
    },
    Array {
        values: HashMap<usize, Vec<T>>,
        range: Vec<usize>,
        t: Type,
    },
    Sorted {
        values: Option<HashMap<String, Vec<T>>>,
        froms: Vec<Handle>,
        tos: Vec<Handle>,
    },
}

impl<T: std::cmp::Ord + Clone> Column<T> {
    pub fn len(&self) -> Option<usize> {
        match self {
            Column::Atomic { value, .. } => value.as_ref().map(|v| v.len()),
            Column::Composite { value, .. } => value.as_ref().map(|v| v.len()),
            Column::Interleaved { value, .. } => value.as_ref().map(|v| v.len()),
            _ => unreachable!(),
            // Column::Array { values, .. } => values.values().next().map(|x| x.len()),
            // Column::Sorted { values, .. } => values
            //     .as_ref()
            //     .and_then(|values| values.values().next())
            //     .map(|x| x.len()),
        }
    }

    pub fn get(&self, i: isize, wrap: bool) -> Option<&T> {
        fn get_rel<T>(v: &[T], i: isize, wrap: bool) -> Option<&T> {
            if wrap && i < 0 {
                v.get(((i + v.len() as isize) % v.len() as isize) as usize)
            } else {
                v.get(i as usize)
            }
        }

        match self {
            Column::Atomic { value, .. } => value.as_ref().and_then(|v| get_rel(v, i, wrap)),
            Column::Composite { value, .. } => value.as_ref().and_then(|v| get_rel(v, i, wrap)),
            Column::Interleaved { value, .. } => value.as_ref().and_then(|v| get_rel(v, i, wrap)),
            Column::Array { .. } | Column::Sorted { .. } => unreachable!(),
        }
    }

    pub fn map(&mut self, f: &dyn Fn(&mut Vec<T>)) {
        match self {
            Column::Atomic { value, .. }
            | Column::Composite { value, .. }
            | Column::Interleaved { value, .. } => {
                value.as_mut().map(f);
            }
            Column::Array { .. } | Column::Sorted { .. } => unreachable!(),
        }
    }

    pub fn is_computed(&self) -> bool {
        match self {
            Column::Atomic { .. } => true,
            Column::Composite { value, .. } => value.is_some(),
            Column::Interleaved { value, .. } => value.is_some(),
            Column::Array { .. } => true,
            Column::Sorted { values, .. } => values.is_some(),
        }
    }

    pub fn atomic(v: Vec<T>, t: Type) -> Self {
        Column::Atomic { value: Some(v), t }
    }

    pub fn composite(e: &Expression) -> Self {
        Column::Composite {
            exp: e.to_owned(),
            value: None,
        }
    }

    pub fn interleaved(c: &[Handle]) -> Self {
        Column::Interleaved {
            value: None,
            froms: c.iter().map(|x| x.to_owned()).collect(),
        }
    }

    pub fn set_values(&mut self, values: Vec<T>) {
        match self {
            Column::Atomic { .. } => panic!("DASF"),
            Column::Composite { ref mut value, .. } => *value = Some(values),
            Column::Interleaved { value, .. } => *value = Some(values),
            Column::Array { .. } | Column::Sorted { .. } => unreachable!(),
        }
    }
}
