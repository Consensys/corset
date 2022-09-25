use crate::compiler::{Expression, Type};
use eyre::*;
use std::collections::HashMap;

#[derive(Debug, Default)]
pub struct ColumnSet<T> {
    pub cols: HashMap<String, HashMap<String, Column<T>>>, // Module -> Name -> Column
}
impl<T> ColumnSet<T> {
    pub fn get(&self, module: &str, name: &str) -> Result<&Column<T>> {
        self.cols
            .get(module)
            .ok_or_else(|| anyhow!("module `{}` unknwown", module))?
            .get(name)
            .ok_or_else(|| anyhow!("column `{}` not found in module `{}`", name, module))
    }

    pub fn get_mut(&mut self, module: &str, name: &str) -> Result<&mut Column<T>> {
        self.cols
            .get_mut(module)
            .ok_or_else(|| anyhow!("module `{}` unknwown", module))?
            .get_mut(name)
            .ok_or_else(|| anyhow!("column `{}` not found in module `{}`", name, module))
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

    pub fn insert_atomic<S: AsRef<str>>(
        &mut self,
        module: S,
        name: S,
        t: Type,
        allow_dup: bool,
    ) -> Result<()> {
        self.insert_column(
            module.as_ref(),
            name.as_ref(),
            Column::Atomic { value: vec![], t },
            allow_dup,
        )
    }

    pub fn insert_array<S: AsRef<str>>(
        &mut self,
        module: S,
        name: S,
        t: Type,
        range: &[usize],
        allow_dup: bool,
    ) -> Result<()> {
        self.insert_column(
            module.as_ref(),
            name.as_ref(),
            Column::Array {
                range: range.to_vec(),
                values: Default::default(),
                t,
            },
            allow_dup,
        )
    }

    pub fn insert_composite<S: AsRef<str>>(
        &mut self,
        module: S,
        name: S,
        e: &Expression,
        allow_dup: bool,
    ) -> Result<()> {
        self.insert_column(
            module.as_ref(),
            name.as_ref(),
            Column::composite(e),
            allow_dup,
        )
    }

    pub fn insert_interleaved<S1: AsRef<str>, S2: AsRef<str>, S3: AsRef<str>>(
        &mut self,
        module: S1,
        name: S2,
        cols: &[S3],
        allow_dup: bool,
    ) -> Result<()> {
        self.insert_column(
            module.as_ref(),
            name.as_ref(),
            Column::Interleaved {
                value: None,
                from: cols
                    .iter()
                    .map(|n| n.as_ref().to_owned())
                    .collect::<Vec<_>>(),
            },
            allow_dup,
        )
    }
}

pub enum Direction {
    Ascending,
    Descending,
}
#[derive(Debug)]
pub enum Column<T> {
    Atomic {
        value: Vec<T>,
        t: Type,
    },
    Array {
        values: HashMap<usize, Vec<T>>,
        range: Vec<usize>,
        t: Type,
    },
    Composite {
        value: Option<Vec<T>>,
        exp: Expression,
    },
    Interleaved {
        value: Option<Vec<T>>,
        from: Vec<String>,
    },
}

impl<T: std::cmp::Ord + Clone> Column<T> {
    pub fn len(&self) -> Option<usize> {
        match self {
            Column::Atomic { value, .. } => Some(value.len()),
            Column::Array { values, .. } => values.values().next().map(|x| x.len()),
            Column::Composite { value, .. } => value.as_ref().map(|v| v.len()),
            Column::Interleaved { value, .. } => value.as_ref().map(|v| v.len()),
        }
    }

    pub fn get(&self, i: usize, idx: usize) -> Option<&T> {
        match self {
            Column::Atomic { value, .. } => value.get(i),
            Column::Array { values, .. } => values.get(&idx).and_then(|v| v.get(i)),
            Column::Composite { value, .. } => value.as_ref().and_then(|v| v.get(i)),
            Column::Interleaved { value, .. } => value.as_ref().and_then(|v| v.get(i)),
        }
    }

    pub fn map(&mut self, f: &dyn Fn(&mut Vec<T>)) {
        match self {
            Column::Atomic { value, .. } => f(value),
            Column::Array { values, .. } => {
                for values in values.values_mut() {
                    f(values);
                }
            }
            Column::Composite { value, .. } => match value {
                Some(v) => f(v),
                None => (),
            },
            Column::Interleaved { value, .. } => match value {
                Some(v) => f(v),
                None => (),
            },
        }
    }

    pub fn is_computed(&self) -> bool {
        match self {
            Column::Atomic { .. } => true,
            Column::Array { .. } => true,
            Column::Composite { value, .. } => value.is_some(),
            Column::Interleaved { value, .. } => value.is_some(),
        }
    }
    pub fn atomic(v: Vec<T>, t: Type) -> Self {
        Column::Atomic { value: v, t }
    }

    pub fn composite(e: &Expression) -> Self {
        Column::Composite {
            exp: e.to_owned(),
            value: None,
        }
    }

    pub fn interleaved<S: AsRef<str>>(c: &[S]) -> Self {
        Column::Interleaved {
            value: None,
            from: c.iter().map(|x| x.as_ref().to_string()).collect(),
        }
    }

    pub fn set_values(&mut self, values: Vec<T>) {
        match self {
            Column::Atomic { .. } => panic!("DASF"),
            Column::Array { .. } => panic!("ASDF"),
            Column::Composite { ref mut value, .. } => *value = Some(values),
            Column::Interleaved { value, .. } => *value = Some(values),
        }
    }
}
