use crate::compiler::{Expression, Type};

#[derive(Debug)]
pub enum Column<T> {
    Atomic(Vec<T>, Type),
    Array {
        range: Vec<usize>,
        content: Vec<Vec<T>>,
        t: Type,
    },
    Composite(Expression),
}
