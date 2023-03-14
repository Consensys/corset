use colored::{Color, ColoredString, Colorize};
use pairing_ce::{
    bn256::Fr,
    ff::{Field, PrimeField},
};
use serde::{Deserialize, Serialize};

use crate::{
    compiler::{Expression, Node},
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
                Base::Bin => todo!(),
            }
        }
    }
}

impl Pretty for Node {
    fn pretty(&self) -> String {
        fn rec_pretty(s: &Node, depth: usize) -> ColoredString {
            let c = &COLORS[depth % COLORS.len()];
            match s.e() {
                Expression::Const(x, _) => format!("{}", x).color(*c),
                Expression::Column(handle, ..) => handle.name.to_string().color(*c),
                Expression::ArrayColumn(handle, range, ..) => format!(
                    "{}[{}:{}]",
                    handle.name,
                    range.first().unwrap(),
                    range.last().unwrap(),
                )
                .color(*c),
                Expression::List(cs) => format!("{{{}}}", format_list(cs, depth + 1)).color(*c),
                Expression::Funcall { func, args } => {
                    format!("({:?} {})", func, format_list(args, depth + 1)).color(*c)
                }
                Expression::Void => "nil".color(*c),
            }
        }
        fn format_list(cs: &[Node], depth: usize) -> String {
            cs.iter()
                .map(|c| rec_pretty(c, depth).to_string())
                .collect::<Vec<_>>()
                .join(" ")
        }

        format!("{}", rec_pretty(self, 0))
    }
    fn pretty_with_base(&self, _base: Base) -> String {
        self.pretty()
    }
}

impl Pretty for Handle {
    fn pretty(&self) -> String {
        format!("{}::{}", self.module.blue(), self.name.white().bold())
    }
    fn pretty_with_base(&self, _base: Base) -> String {
        self.pretty()
    }
}
