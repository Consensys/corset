use ark_bls12_377::Fr;
use ark_ff::{BigInteger, PrimeField};
use itertools::Itertools;
use num_traits::Zero;
use owo_colors::{colored::Color, OwoColorize};
use serde::{Deserialize, Serialize};

use crate::{
    compiler::{ColumnRef, Conditioning, Expression, Magma, Node},
    structs::Handle,
};

pub mod opcodes;

pub const COLORS: [Color; 7] = [
    Color::Green,
    Color::Yellow,
    Color::BrightBlue,
    Color::Red,
    Color::Magenta,
    Color::Cyan,
    Color::BrightWhite,
];

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum Base {
    Dec,
    Hex,
    Bin,
    Bool,
    Loob,
    Bytes,
    OpCode,
}

impl std::convert::TryFrom<&str> for Base {
    type Error = anyhow::Error;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value.to_lowercase().as_str() {
            ":bin" => Ok(Base::Bin),
            ":dec" => Ok(Base::Dec),
            ":hex" => Ok(Base::Hex),
            ":bytes" => Ok(Base::Bytes),
            ":opcode" => Ok(Base::OpCode),
            ":truthiness" => Ok(Base::Bool),
            _ => anyhow::bail!(
                ":display expects one of :hex, :dec, :bin, :bytes, :opcode, :truthiness; found {}",
                value
            ),
        }
    }
}

impl std::convert::From<Magma> for Base {
    fn from(m: Magma) -> Self {
        match m.c() {
            Conditioning::Boolean => Base::Bool,
            Conditioning::Loobean => Base::Loob,
            _ => Base::Hex,
        }
    }
}

pub trait Pretty {
    fn pretty(&self) -> String;
    fn pretty_with_base(&self, base: Base) -> String;
}

fn to_bytes<'a>(f: &'a Fr) -> Vec<u8> {
    // TODO: smallvec
    f.into_bigint()
        .0
        .iter()
        .flat_map(|bs| bs.to_be_bytes())
        .collect()
}

fn to_byte(f: &Fr) -> Option<u8> {
    let bs = f.0.to_bytes_le();
    if bs.iter().skip(1).any(|&b| b != 0) {
        None
    } else {
        Some(bs[0])
    }
}

impl Pretty for Fr {
    fn pretty(&self) -> String {
        format!("{}", self)
    }

    fn pretty_with_base(&self, base: Base) -> String {
        if self.is_zero() {
            String::from("0")
        } else {
            match base {
                Base::Dec => self.pretty(),
                Base::Hex => {
                    if self.is_zero() {
                        String::from("0")
                    } else {
                        self.pretty()
                    }
                }
                Base::Bin => to_bytes(self)
                    .into_iter()
                    .map(|b| format!("0b{:b}", b))
                    .join(""),
                Base::Bytes => {
                    if self.is_zero() {
                        String::from("00")
                    } else {
                        to_bytes(self)
                            .into_iter()
                            .rev()
                            .map(|b| format!("{:x}", b))
                            .rev()
                            .join(" ")
                    }
                }
                Base::Bool => if self.is_zero() { "false" } else { "true" }.to_string(),
                Base::Loob => if self.is_zero() { "true" } else { "false" }.to_string(),
                Base::OpCode => to_byte(self)
                    .map(|b| opcodes::to_str(b))
                    .unwrap_or_else(|| self.pretty()),
            }
        }
    }
}

impl Pretty for Node {
    fn pretty(&self) -> String {
        fn rec_pretty(s: &Node, depth: usize) -> String {
            let depth = depth
                + match s.e() {
                    Expression::Funcall { .. } | Expression::List(_) => 1,
                    _ => 0,
                };
            let c = &COLORS[depth % COLORS.len()];
            match s.e() {
                Expression::Const(x) => format!("{}", x).color(*c).to_string(),
                Expression::Column { handle, .. } | Expression::ExoColumn { handle, .. } => {
                    handle.to_string().color(*c).to_string()
                }
                Expression::ArrayColumn { handle, domain, .. } => {
                    format!("{}{}", handle.as_handle().name, domain)
                        .color(*c)
                        .to_string()
                }
                Expression::List(cs) => format!("{{{}}}", format_list(cs, depth))
                    .color(*c)
                    .to_string(),
                Expression::Funcall { func, args } => {
                    format!("({} {})", func, format_list(args, depth))
                        .color(*c)
                        .to_string()
                }
                Expression::Void => "nil".color(*c).to_string(),
            }
        }
        fn format_list(cs: &[Node], depth: usize) -> String {
            cs.iter()
                .map(|c| rec_pretty(c, depth))
                .collect::<Vec<_>>()
                .join(" ")
        }
        rec_pretty(self, 0)
    }
    fn pretty_with_base(&self, _base: Base) -> String {
        self.pretty()
    }
}

impl Pretty for Handle {
    fn pretty(&self) -> String {
        if self.module != crate::compiler::MAIN_MODULE {
            format!("{}.{}", self.module.blue(), self.name.white().bold())
        } else {
            format!("{}", self.name.white().bold())
        }
    }
    fn pretty_with_base(&self, _base: Base) -> String {
        self.pretty()
    }
}

impl Pretty for ColumnRef {
    fn pretty(&self) -> String {
        self.map(|id| format!("Col.#{}", id), |h| h.pretty())
    }
    fn pretty_with_base(&self, _base: Base) -> String {
        self.pretty()
    }
}

lazy_static::lazy_static! {
    pub static ref SUBSCRIPT: std::collections::HashMap<char,char> = maplit::hashmap!{
        'a' => 'ₐ',
        'e' => 'ₑ',
        'x' => 'ₓ',
        '0' => '₀',
        '1' => '₁',
        '2' => '₂',
        '3' => '₃',
        '4' => '₄',
        '5' => '₅',
        '6' => '₆',
        '7' => '₇',
        '8' => '₈',
        '9' => '₉',
        '-' => '₋',
        '+' => '₊',
    };
    pub static ref SUPERSCRIPT: std::collections::HashMap<char,char> = maplit::hashmap!{
        'a' => 'ᵃ',
        'b' => 'ᵇ',
        'c' => 'ᶜ',
        'd' => 'ᵈ',
        'e' => 'ᵉ',
        'f' => 'ᶠ',
        'x' => 'ˣ',
        '0' => '⁰',
        '1' => '¹',
        '2' => '²',
        '3' => '³',
        '4' => '⁴',
        '5' => '⁵',
        '6' => '⁶',
        '7' => '⁷',
        '8' => '⁸',
        '9' => '⁹',
        '-' => '⁻',
        '+' => '⁺',
    };
}

pub fn subscript(s: &str) -> String {
    s.chars()
        .map(|c| SUBSCRIPT.get(&c).cloned().unwrap_or('ᵋ'))
        .collect()
}
