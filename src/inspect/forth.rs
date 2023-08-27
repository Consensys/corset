use crate::compiler::ColumnRef;
use anyhow::*;
use either::Either;
use num_bigint::BigInt;
use num_traits::Num;
use pairing_ce::{
    bn256::Fr,
    ff::{Field, PrimeField},
};
use std::collections::HashMap;

#[derive(Clone, Debug)]
pub enum Combinator {
    And,
    Or,
    Not,
}
impl Combinator {
    fn apply(&self, args: &[Either<Fr, bool>]) -> bool {
        let a1 = *args[0].as_ref().right().unwrap();
        match self {
            Combinator::And => {
                let a2 = *args[1].as_ref().right().unwrap();
                a1 && a2
            }
            Combinator::Or => {
                let a2 = *args[1].as_ref().right().unwrap();
                a1 || a2
            }
            Combinator::Not => !a1,
        }
    }
}
impl From<&str> for Combinator {
    fn from(s: &str) -> Self {
        match s {
            "&" => Combinator::And,
            "|" => Combinator::Or,
            "!" => Combinator::Not,
            _ => panic!("not a Combinator"),
        }
    }
}
impl std::fmt::Display for Combinator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Combinator::And => write!(f, "&"),
            Combinator::Or => write!(f, "|"),
            Combinator::Not => write!(f, "!"),
        }
    }
}

#[derive(Clone, Debug)]
pub enum Relation {
    Eq,
    Gt,
    Lt,
    Gte,
    Lte,
}
impl Relation {
    fn apply(&self, args: &[Either<Fr, bool>]) -> bool {
        let a1 = args[0].as_ref().left().unwrap();
        let a2 = args[1].as_ref().left().unwrap();
        match self {
            Relation::Eq => a1.eq(a2),
            Relation::Gt => a1.gt(a2),
            Relation::Lt => a1.lt(a2),
            Relation::Gte => a1.ge(a2),
            Relation::Lte => a1.le(a2),
        }
    }
}
impl From<&str> for Relation {
    fn from(s: &str) -> Self {
        match s {
            "=" => Relation::Eq,
            ">" => Relation::Gt,
            "<" => Relation::Lt,
            ">=" => Relation::Gte,
            "<=" => Relation::Lte,
            _ => panic!("not a Relation"),
        }
    }
}
impl std::fmt::Display for Relation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Relation::Eq => write!(f, "="),
            Relation::Gt => write!(f, ">"),
            Relation::Lt => write!(f, "<"),
            Relation::Gte => write!(f, ">="),
            Relation::Lte => write!(f, "<="),
        }
    }
}

#[derive(Clone, Debug)]
pub enum Function {
    Add,
    Sub,
    Mul,
}
impl Function {
    fn apply(&self, args: &[Fr]) -> Fr {
        match self {
            Function::Add => {
                let mut x = args[0].clone();
                x.add_assign(&args[1]);
                x
            }
            Function::Sub => {
                let mut x = args[0].clone();
                x.sub_assign(&args[1]);
                x
            }
            Function::Mul => {
                let mut x = args[0].clone();
                x.mul_assign(&args[1]);
                x
            }
        }
    }
}
impl From<&str> for Function {
    fn from(s: &str) -> Self {
        match s {
            "+" => Function::Add,
            "-" => Function::Sub,
            "*" => Function::Mul,
            _ => panic!("not a Function"),
        }
    }
}
impl std::fmt::Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Function::Add => write!(f, "+"),
            Function::Sub => write!(f, "-"),
            Function::Mul => write!(f, "×"),
        }
    }
}

#[derive(Clone, Debug)]
pub enum Node {
    Combinator(Combinator, Vec<Node>),
    Comparison(Relation, Vec<Node>),
    Funcall(Function, Vec<Node>),
    Column(ColumnRef),
    Const(Fr),
}
impl Node {
    fn is_bool(&self) -> bool {
        match self {
            Node::Comparison(..) | Node::Combinator(..) => true,
            _ => false,
        }
    }
    fn is_value(&self) -> bool {
        match self {
            Node::Comparison(_, _) | Node::Combinator(..) => false,
            _ => true,
        }
    }
}
impl Node {
    pub fn scan<F: Fn(isize, &ColumnRef) -> Option<Fr>>(
        &self,
        get: &F,
        max: isize,
    ) -> Option<isize> {
        for i in 0..max {
            let r = self.eval(i, &get);
            match r {
                Some(Either::Right(b)) => {
                    if b {
                        return Some(i);
                    }
                }
                Some(Either::Left(_)) => panic!("not a boolean"),
                None => {}
            }
        }
        return None;
    }

    fn eval<F: Fn(isize, &ColumnRef) -> Option<Fr>>(
        &self,
        i: isize,
        get: &F,
    ) -> Option<Either<Fr, bool>> {
        match self {
            Node::Combinator(c, args) => {
                let args = args
                    .iter()
                    .map(|a| a.eval(i, get))
                    .collect::<Option<Vec<_>>>();
                args.map(|args| Either::Right(c.apply(&args)))
            }
            Node::Comparison(r, args) => {
                let args = args
                    .iter()
                    .map(|a| a.eval(i, get))
                    .collect::<Option<Vec<_>>>();
                args.map(|args| Either::Right(r.apply(&args)))
            }
            Node::Funcall(f, args) => {
                let args = args
                    .iter()
                    .map(|a| a.eval(i, get).map(|x| x.left().unwrap()))
                    .collect::<Option<Vec<_>>>();
                args.map(|args| Either::Left(f.apply(&args)))
            }
            Node::Column(cref) => get(i, cref).map(|x| Either::Left(x)),
            Node::Const(x) => Some(Either::Left(x.clone())),
        }
    }
}
impl std::fmt::Display for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Node::Combinator(c, args) => write!(f, "({}) {} ({})", args[0], c, args[1]),
            Node::Comparison(r, args) => write!(f, "({}) {} ({})", args[0], r, args[1]),
            Node::Funcall(ff, args) => write!(f, "({}) {} ({})", args[0], ff, args[1]),
            Node::Const(x) => write!(f, "{}", x.to_string()),
            Node::Column(name) => write!(f, "{}", name),
        }
    }
}

enum Token {
    Combinator(Combinator),
    Relation(Relation),
    Function(Function),
    Const(BigInt),
    Column(ColumnRef),
}
fn parse_token(s: &str, module: &str, columns: &HashMap<String, ColumnRef>) -> Result<Token> {
    match s {
        "&" | "|" | "!" => Ok(Token::Combinator(s.into())),
        "=" | ">" | ">=" | "<" | "<=" => Ok(Token::Relation(s.into())),
        "+" | "-" | "*" => Ok(Token::Function(s.into())),
        _ => {
            if s.chars().all(|c| c.is_digit(10))
                || (s.starts_with("0x") && s.chars().all(|c| c.is_digit(16)))
                || s.starts_with('0')
            {
                if let Some(s) = s.strip_prefix("0x") {
                    BigInt::from_str_radix(s, 16)
                } else if let Some(s) = s.strip_prefix("0b") {
                    BigInt::from_str_radix(s, 2)
                } else {
                    BigInt::from_str_radix(s, 10)
                }
                .map(|x| Token::Const(x))
                .map_err(anyhow::Error::msg)
            } else {
                if let Some(r) = columns.get(s) {
                    Ok(Token::Column(r.clone()))
                } else {
                    bail!("{} unknown in {}", s, module)
                }
            }
        }
    }
}

fn take_one(stack: &mut Vec<Node>, fname: &str) -> Result<Node> {
    let r1 = stack
        .pop()
        .ok_or_else(|| anyhow!("{} expects an argument", fname))?;
    Ok(r1)
}

fn take_two(stack: &mut Vec<Node>, fname: &str) -> Result<Vec<Node>> {
    let r2 = stack
        .pop()
        .ok_or_else(|| anyhow!("{} expects two arguments", fname))?;
    let r1 = stack
        .pop()
        .ok_or_else(|| anyhow!("{} expects two arguments", fname))?;
    Ok(vec![r1, r2])
}

pub fn parse(s: &str, module: &str, columns: &HashMap<String, ColumnRef>) -> Result<Node> {
    let tokens = s.split_whitespace();
    let mut stack = Vec::new();

    for token in tokens.map(|t| parse_token(t, module, columns)) {
        match token? {
            Token::Combinator(c) => match c {
                Combinator::And | Combinator::Or => {
                    let args = take_two(&mut stack, &c.to_string())?;
                    if !args.iter().all(|n| n.is_bool()) {
                        bail!("{} expects conditions", c.to_string());
                    }
                    stack.push(Node::Combinator(c, args))
                }
                Combinator::Not => {
                    let arg = take_one(&mut stack, &c.to_string())?;
                    if !arg.is_bool() {
                        bail!("{} expects conditions", c.to_string());
                    }
                    stack.push(Node::Combinator(c, vec![arg]))
                }
            },
            Token::Relation(r) => {
                let args = take_two(&mut stack, &r.to_string())?;
                match r {
                    Relation::Eq | Relation::Gt | Relation::Lt | Relation::Gte | Relation::Lte => {
                        if !args.iter().all(|n| n.is_value()) {
                            bail!("{} expects values", r.to_string());
                        }
                        stack.push(Node::Comparison(r, args))
                    }
                }
            }
            Token::Function(f) => {
                let args = take_two(&mut stack, &f.to_string())?;
                if !args.iter().all(|n| n.is_value()) {
                    bail!("{} expects values", f.to_string());
                }
                stack.push(match f {
                    Function::Add => {
                        if let (Node::Const(c1), Node::Const(c2)) = (&args[0], &args[1]) {
                            let mut r = c1.clone();
                            r.add_assign(&c2);
                            Node::Const(r)
                        } else {
                            Node::Funcall(Function::Add, args)
                        }
                    }
                    Function::Sub => {
                        if let (Node::Const(c1), Node::Const(c2)) = (&args[0], &args[1]) {
                            let mut r = c1.clone();
                            r.sub_assign(&c2);
                            Node::Const(r)
                        } else {
                            Node::Funcall(Function::Sub, args)
                        }
                    }
                    Function::Mul => {
                        if let (Node::Const(c1), Node::Const(c2)) = (&args[0], &args[1]) {
                            let mut r = c1.clone();
                            r.mul_assign(&c2);
                            Node::Const(r)
                        } else {
                            Node::Funcall(Function::Mul, args)
                        }
                    }
                });
            }
            Token::Const(x) => stack.push(Node::Const(Fr::from_str(&x.to_string()).unwrap())),
            Token::Column(c) => stack.push(Node::Column(c)),
        }
    }

    if stack.is_empty() || stack.len() > 1 {
        bail!("expected a single epxression")
    }
    if stack[0].is_value() {
        bail!("expected a comparison")
    }

    Ok(stack[0].to_owned())
}
