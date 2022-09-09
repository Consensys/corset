use crate::compiler::Expression;

#[derive(Debug)]
pub enum Column<T> {
    Atomic(Vec<T>),
    Array {
        range: Vec<usize>,
        content: Vec<Vec<T>>,
    },
    Composite(Expression),
}
