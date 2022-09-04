use eyre::*;
use std::fmt;
use std::fmt::Debug;

#[derive(Clone)]
pub enum Constraint {
    TopLevel {
        name: String,
        expr: Box<Constraint>,
    },
    Funcall {
        func: Builtin,
        args: Vec<Constraint>,
    },
    Const(i32),
    Column(String),
    ArrayColumn(String, Vec<usize>),
    ArrayColumnElement(String, usize),
    List(Vec<Constraint>),
}
impl Constraint {
    pub fn flat_fold<T>(&self, f: &dyn Fn(&Constraint) -> T) -> Vec<T> {
        let mut ax = vec![];
        match self {
            Constraint::List(xs) => {
                for x in xs {
                    ax.push(f(x));
                }
            }
            x => ax.push(f(x)),
        }
        ax
    }
}
impl Debug for Constraint {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fn format_list(cs: &[Constraint]) -> String {
            cs.iter()
                .map(|c| format!("{:?}", c))
                .collect::<Vec<_>>()
                .join(" ")
        }

        match self {
            Constraint::TopLevel { name, expr } => write!(f, "{}: {:?}", name, expr),
            Constraint::Const(x) => write!(f, "{}:CONST", x),
            Constraint::Column(name) => write!(f, "{}:COLUMN", name),
            Constraint::ArrayColumn(name, range) => {
                write!(
                    f,
                    "{}[{}:{}]:ARRAYCOLUMN",
                    name,
                    range.first().unwrap(),
                    range.last().unwrap()
                )
            }
            Constraint::ArrayColumnElement(name, i) => {
                write!(f, "{}[{}]:COLUMN", name, i)
            }
            Constraint::List(cs) => write!(f, "'({})", format_list(cs)),
            Self::Funcall { func, args } => write!(f, "({:?} {})", func, format_list(args)),
        }
    }
}

#[derive(Debug)]
pub struct ConstraintsSet {
    pub constraints: Vec<Constraint>,
}
impl ConstraintsSet {
    pub fn from_sources<S: AsRef<str>>(sources: &[(&str, S)]) -> Result<Self> {
        crate::compiler::compile(
            &sources
                .iter()
                .map(|(n, s)| (*n, s.as_ref()))
                .collect::<Vec<_>>(),
        )
    }
}

// In opposition to a `Form`, a `Builtin` defines a function executable by the backend
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Builtin {
    Add,
    Sub,
    Mul,
    IfZero,
    Shift,
    Neg,
    Inv,
    Nth,

    Begin,

    // Don't like it :/
    BranchIfZero,
    BranchIfZeroElse,
    BranchIfNotZero,
    BranchIfNotZeroElse,
    // BranchBinIfOne,
    // BranchBinIfZero,

    // BranchBinIfOneElse,
    // BranchBinIfZeroElse,
}
