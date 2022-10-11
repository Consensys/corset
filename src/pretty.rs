use colored::{Color, ColoredString, Colorize};
use pairing_ce::{bn256::Fr, ff::PrimeField};

use crate::compiler::Expression;

pub const COLORS: [Color; 7] = [
    Color::Green,
    Color::Yellow,
    Color::BrightBlue,
    Color::Red,
    Color::Magenta,
    Color::Cyan,
    Color::BrightWhite,
];

pub trait Pretty {
    fn pretty(&self) -> String;
}

impl Pretty for Fr {
    fn pretty(&self) -> String {
        let hex = self.into_repr().to_string();
        i64::from_str_radix(&hex[2..], 16)
            .map(|x| x.to_string())
            .unwrap_or(format!("0x0{}", hex[2..].trim_start_matches('0')))
    }
}

impl Pretty for Expression {
    fn pretty(&self) -> String {
        fn rec_pretty(s: &Expression, depth: usize) -> ColoredString {
            let c = &COLORS[depth % COLORS.len()];
            match s {
                Expression::Const(x, _) => format!("{}", x).color(*c),
                Expression::Column(handle, _t, _k) => handle.name.to_string().color(*c),
                Expression::ArrayColumn(handle, range, _t) => format!(
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
        fn format_list(cs: &[Expression], depth: usize) -> String {
            cs.iter()
                .map(|c| rec_pretty(c, depth).to_string())
                .collect::<Vec<_>>()
                .join(" ")
        }

        format!("{}", rec_pretty(self, 0))
    }
}
