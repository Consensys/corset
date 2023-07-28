use pairing_ce::{
    bn256::Fr,
    ff::{Field as libField, PrimeField},
};
use std::{
    fmt::{Debug, Display},
    hash::Hash,
};

pub trait Field:
    Clone + Debug + PartialEq + Eq + Hash + Send + Sync + Default + Ord + Display + Copy + 'static
{
    const BYTE_CAPACITY: usize; // the number of bytes that can be stored withing one field element (16 for BN256)

    fn zero() -> Self;
    fn one() -> Self;
    fn is_zero(&self) -> bool;
    fn negate(&mut self);
    fn inverse(&self) -> Option<Self>;
    fn add_assign(&mut self, other: &Self);
    fn sub_assign(&mut self, other: &Self);
    fn mul_assign(&mut self, other: &Self);
    fn from_str(s: &str) -> Option<Self>;
    fn hex(&self) -> String;
    fn into_repr(&self) -> [u64; 4]; // TODO handle lengths different than 4
}

impl Field for Fr {
    const BYTE_CAPACITY: usize = 15;

    fn zero() -> Self {
        <Fr as libField>::zero()
    }

    fn one() -> Self {
        <Fr as libField>::one()
    }

    fn is_zero(&self) -> bool {
        <Fr as libField>::is_zero(self)
    }

    fn negate(&mut self) {
        <Fr as libField>::negate(self)
    }

    fn inverse(&self) -> Option<Self> {
        <Fr as libField>::inverse(self)
    }

    fn add_assign(&mut self, other: &Self) {
        <Fr as libField>::add_assign(self, other)
    }

    fn sub_assign(&mut self, other: &Self) {
        <Fr as libField>::sub_assign(self, other)
    }

    fn mul_assign(&mut self, other: &Self) {
        <Fr as libField>::mul_assign(self, other)
    }

    fn from_str(s: &str) -> Option<Self> {
        <Fr as PrimeField>::from_str(s)
    }

    fn hex(&self) -> String {
        <Fr as PrimeField>::into_repr(&self).to_string()
    }

    // TODO handle lengths different than 4
    fn into_repr(&self) -> [u64; 4] {
        <Fr as PrimeField>::into_repr(&self).0
    }
}
