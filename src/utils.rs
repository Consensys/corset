use pairing_ce::{bn256::Fr, ff::PrimeField};

pub fn pretty(x: &Fr) -> String {
    x.into_repr().to_string()
}

pub fn export_symbol(s: &str) -> String {
    s.replace('(', "_")
        .replace(')', "_")
        .replace('{', "_")
        .replace('}', "_")
        .replace('[', "_")
        .replace(']', "_")
        .replace('/', "_")
        .replace(':', "_")
        .replace('%', "_")
}
