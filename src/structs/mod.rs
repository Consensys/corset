mod field;
mod handle;

pub use field::Field;
pub use handle::Handle;

const ARRAY_SEPARATOR: &str = "_";
const SUM_SEPARATOR: &str = "_+_";
pub const PERSPECTIVE_SEPARATOR: char = '/';
const MODULE_SEPARATOR: &str = "__";

// Goldilocks related separators: (cf https://github.com/Consensys/zkevm-spec/issues/63)
const OVERFLOW_SEPARATOR: &str = "_overflow_";
const BYTE_SEPARATOR: &str = "_byte_";
const ACC_SEPARATOR: &str = "_acc_";
const COUNTER_SEPARATOR: &str = "_counter";