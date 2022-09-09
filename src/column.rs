pub enum Column {
    Empty,
}
// pub trait Column<T = ()>: std::ops::Index<isize, Output = T> {
//     fn len(&self) -> usize;
// }

// #[derive(Default)]
// pub struct Empty<T> {
//     _t: std::marker::PhantomData<T>,
// }

// impl<T> Column<T> for Empty<T> {
//     fn len(&self) -> usize {
//         0
//     }
// }

// impl<T> std::ops::Index<isize> for Empty<T> {
//     type Output = T;
//     fn index(&self, _: isize) -> &T {
//         panic!("Can not index an empty column")
//     }
// }
