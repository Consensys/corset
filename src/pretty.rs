use owo_colors::{colored::Color, OwoColorize};
use pairing_ce::{
    bn256::Fr,
    ff::{Field, PrimeField},
};
use serde::{Deserialize, Serialize};

use crate::{
    compiler::{ColumnRef, Expression, Node},
    structs::Handle,
};

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
    Bytes,
}
impl std::convert::TryFrom<&str> for Base {
    type Error = anyhow::Error;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value.to_lowercase().as_str() {
            ":bin" => Ok(Base::Bin),
            ":dec" => Ok(Base::Dec),
            ":hex" => Ok(Base::Hex),
            ":bytes" => Ok(Base::Bytes),
            _ => anyhow::bail!(
                ":display expects one of :hex, :dec, :bin, :bytes; found {}",
                value
            ),
        }
    }
}

pub trait Pretty {
    fn pretty(&self) -> String;
    fn pretty_with_base(&self, base: Base) -> String;
}

impl Pretty for Fr {
    fn pretty(&self) -> String {
        let hex = self.into_repr().to_string();
        u64::from_str_radix(&hex[2..], 16)
            .map(|x| x.to_string())
            .unwrap_or(format!("0x0{}", hex[2..].trim_start_matches('0')))
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
                        format!(
                            "0x{}",
                            self.into_repr().to_string()[2..].trim_start_matches('0')
                        )
                    }
                }
                Base::Bin => format!(
                    "0b{:b}",
                    u64::from_str_radix(
                        self.into_repr().to_string()[2..].trim_start_matches('0'),
                        16
                    )
                    .expect("too big to represent as binary"),
                ),
                Base::Bytes => {
                    // ugly, but works
                    if self.is_zero() {
                        String::from("0")
                    } else {
                        let mut bytes = self.into_repr().to_string()[2..]
                            .trim_start_matches('0')
                            .to_string();
                        if bytes.len() % 2 != 0 {
                            bytes.insert(0, '0');
                        }

                        let mut out = String::with_capacity(2 * bytes.len());
                        let mut z = bytes.chars().peekable();
                        while z.peek().is_some() {
                            out.push_str(z.by_ref().take(2).collect::<String>().as_str());
                            out.push(' ');
                        }
                        out
                    }
                }
            }
        }
    }
}

impl Pretty for Node {
    fn pretty(&self) -> String {
        fn rec_pretty(s: &Node, depth: usize) -> String {
            let c = &COLORS[depth % COLORS.len()];
            match s.e() {
                Expression::Const(x, _) => format!("{}", x).color(*c).to_string(),
                Expression::Column { handle, .. } => handle.to_string().color(*c).to_string(),
                Expression::ArrayColumn { handle, domain, .. } => {
                    format!("{}{}", handle.as_handle().name, domain)
                        .color(*c)
                        .to_string()
                }
                Expression::List(cs) => format!("{{{}}}", format_list(cs, depth + 1))
                    .color(*c)
                    .to_string(),
                Expression::Funcall { func, args } => {
                    format!("({:?} {})", func, format_list(args, depth + 1))
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
