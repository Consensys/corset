use crate::compiler::Magma;

pub const FIELD_BITSIZE: usize = 254;

pub fn col_count_bits(x: usize) -> usize {
    (x + FIELD_BITSIZE - 1) / FIELD_BITSIZE
}

pub fn col_count_bytes(x: usize) -> usize {
    col_count_bits(x * 8)
}

pub fn col_count_magma(m: Magma) -> usize {
    col_count_bits(m.bit_size())
}
