use crate::compiler::{Expression, Type};
use eyre::*;
use std::collections::HashMap;

#[derive(Debug, Default)]
pub struct ColumnSet<T> {
    pub cols: HashMap<String, Column<T>>,
}

impl<T> std::convert::From<HashMap<String, Column<T>>> for ColumnSet<T> {
    fn from(x: HashMap<String, Column<T>>) -> Self {
        ColumnSet { cols: x }
    }
}

impl<T: std::cmp::Ord + std::marker::Copy> ColumnSet<T> {
    pub fn insert_composite<S: AsRef<str>>(
        &mut self,
        name: S,
        e: &Expression,
        allow_dup: bool,
    ) -> Result<()> {
        let name: String = name.as_ref().into();
        if self.cols.contains_key(&name) && !allow_dup {
            Err(eyre!("`{}` already exists", name))
        } else {
            self.cols.insert(name, Column::composite(e));
            Ok(())
        }
    }

    pub fn insert_sorted(&mut self, name: &str, from: &str, allow_dup: bool) -> Result<()> {
        if self.cols.contains_key(name) && !allow_dup {
            Err(eyre!("`{}` already exists", name))
        } else {
            self.cols.insert(name.into(), Column::sorted(from));
            Ok(())
        }
    }
}

#[derive(Debug)]
pub enum Column<T> {
    Atomic(Vec<T>, Type),
    Array {
        range: Vec<usize>,
        content: Vec<Vec<T>>,
    },
    Composite {
        value: Option<Vec<T>>,
        exp: Expression,
    },
    Sorted {
        value: Option<Vec<T>>,
        from: String,
    },
    Interleaved {
        value: Option<Vec<T>>,
        from: Vec<String>,
    },
}

impl<T: std::cmp::Ord + std::marker::Copy> Column<T> {
    pub fn len(&self) -> usize {
        match self {
            Column::Atomic(v, _) => v.len(),
            Column::Array { content, .. } => content.first().unwrap().len(),
            Column::Composite { value, .. } => value.as_ref().unwrap().len(),
            Column::Sorted { value, .. } => value.as_ref().unwrap().len(),
            Column::Interleaved { value, .. } => value.as_ref().unwrap().len(),
        }
    }
    pub fn get(&self, i: usize) -> Option<&T> {
        match self {
            Column::Atomic(v, _) => v.get(i),
            Column::Array { .. } => None,
            Column::Composite { value, .. } => value.as_ref().and_then(|v| v.get(i)),
            Column::Sorted { value, .. } => value.as_ref().and_then(|v| v.get(i)),
            Column::Interleaved { value, .. } => value.as_ref().and_then(|v| v.get(i)),
        }
    }

    pub fn atomic(v: Vec<T>, t: Type) -> Self {
        Column::Atomic(v, t)
    }

    pub fn composite(e: &Expression) -> Self {
        Column::Composite {
            exp: e.clone(),
            value: None,
        }
    }

    pub fn sorted(c: &str) -> Self {
        Column::Sorted {
            value: None,
            from: c.into(),
        }
    }

    pub fn interleaved<S: AsRef<str>>(c: &[S]) -> Self {
        Column::Interleaved {
            value: None,
            from: c.iter().map(|x| x.as_ref().to_string()).collect(),
        }
    }

    fn compute(&mut self) {
        match self {
            Column::Atomic(..) => {}
            Column::Array { .. } => {}
            Column::Composite { exp, value } => {
                if value.is_none() {
                    todo!()
                }
            }
            Column::Sorted { value, from } => {
                if value.is_none() {
                    todo!()
                }
            }
            Column::Interleaved { value, from } => {
                if value.is_none() {
                    *value = Some(vec![]);
                }
            }
        }
    }
}
