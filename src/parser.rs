use pest::{iterators::Pair, Parser, error::Error};
use std::collections::HashMap;
use color_eyre::eyre::*;

#[derive(Parser)]
#[grammar = "corset.pest"]
pub struct CorsetParser;


pub struct ConstraintsSet {
    pub constraints: Vec<AstNode>,
}
impl ConstraintsSet {
    pub fn from_str<S: AsRef<str>>(s: S, settings: &crate::Args) -> Result<Self> {
        let constraints = parse(s.as_ref())?;

        Ok(ConstraintsSet { constraints })
    }

    pub fn from_file<S: AsRef<str>>(filename: S, settings: &crate::Args) -> Result<Self> {
        let file_content = std::fs::read_to_string(filename.as_ref())?;
        Self::from_str(&file_content, settings)
    }
}

fn make_chain(xs: &[AstNode], operand: &str) -> Result<String> {
    let head = xs[0].render()?;
    let tail = xs[1..]
        .iter()
        .map(|x| AstNode::render(x).map(|s| format!("{}({})", operand, s)))
        .collect::<Result<Vec<_>>>()?
        .join(".");
    Ok(format!("{}{}", head, tail))
}

#[derive(Debug, PartialEq)]
enum Verb {
    Add,
    Sub,
    Mul,
    And,
    Equals,

    IfZeroThen,
    Neg,
    Inv,
    Shift,
    Vanishes,

    BinIfOneElse,
    BinIfZeroElse,
    BinIfZero,
    BinIfNotZero,
    BinIfOne,
    IfNotZero,
    Inc,
    Dec,
    RemainsConstant,
    DidntChange,
    WillEqual,
    WasEqual,

    IfNotZeroElse,
    BranchIfZero,
    BranchIfNotZero,
    BranchBinIfZero,
    BranchBinIfOne,
    BranchIfZeroElse,
    BranchIfNotZeroElse,

    BranchBinIfZeroElse,
    BranchBinIfOneElse,

    Or,
    Xor,
    IsZeroBinary,
    IsNonzeroBinary,
    IfZeroElse,
    IsBinary,
}
impl Verb {
    pub fn render(&self, args: &[AstNode]) -> Result<String> {
        match self {
            Verb::BranchIfZero => {
                let cond = args[0].render()?;
                let then = args[1..]
                    .iter()
                    .map(AstNode::render)
                    .collect::<Result<Vec<_>>>()?
                    .join(", ");
                Ok(format!("{}.BranchIfZero({})", cond, then))
            }
            Verb::Vanishes => {
                assert!(args.len() == 1);
                args[0].render()
            }
            Verb::Add => make_chain(args, ".Add"),
            Verb::And => make_chain(args, ".Mul"),
            Verb::Mul => make_chain(args, ".Mul"),
            Verb::Sub => make_chain(args, ".Sub"),
            Verb::Equals => make_chain(args, ".Equals"),
            Verb::BinIfOne => {
                if args.len() != 2 {
                    Err(anyhow!("if-one takes two argument {}", 3))
                } else {
                    let lhs = args[0].render()?;
                    let rhs = args[1].render()?;
                    Ok(format!("{}.BinIfOne({})", lhs, rhs))
                }
            }
            Verb::BinIfZero => {
                if args.len() != 2 {
                    Err(anyhow!("if-one takes two argument {}", 3))
                } else {
                    let lhs = args[0].render()?;
                    let rhs = args[1].render()?;
                    Ok(format!("{}.BinIfZero({})", lhs, rhs))
                }
            }
            x @ _ => {
                dbg!(x);
                todo!()
            }
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum AstNode {
    Ignore,
    Value(i32),
    Column(String),
    Function { verb: Verb, exprs: Vec<AstNode> },
}
impl AstNode {
    pub fn render(&self) -> Result<String> {
        match self {
            Self::Value(x) => match x {
                0..=2 | 127 | 256 => Ok(format!("column.CONST_{}()", x)),
                x @ _ => Ok(format!("column.CONST_UINT64({})", x)),
            },
            Self::Column(name) => Ok(format!("CE[{}.Name()]", name)),
            Self::Function { verb, exprs } => verb.render(exprs),
            _ => Ok("??".into()),
        }
    }
}

fn build_ast_from_expr(pair: Pair<Rule>) -> AstNode {
    // println!("parsing {:?}", pair.as_rule());
    match pair.as_rule() {
        Rule::expr | Rule::constraint => build_ast_from_expr(pair.into_inner().next().unwrap()),
        Rule::sexpr => {
            let mut content = pair.into_inner();
            let verb = parse_verb(content.next().unwrap());
            let exprs = content
                .map(|p| build_ast_from_expr(p))
                .filter(|x| *x != AstNode::Ignore)
                .collect();
            AstNode::Function { verb, exprs }
        }
        Rule::column => AstNode::Column(pair.as_str().into()),
        Rule::value => AstNode::Value(pair.as_str().parse().unwrap()),
        // Rule::function => {}
        x @ _ => {
            dbg!(&x);
            AstNode::Ignore
        }
    }
}

fn parse_verb(pair: Pair<Rule>) -> Verb {
    match pair.as_str().to_lowercase().as_str() {
        "add" | "+" => Verb::Add,
        "and" => Verb::And,
        "and" | "mul" | "*" => Verb::Mul,
        "branch-if-zero" => Verb::BranchIfZero,
        "sub" | "-" => Verb::Sub,
        "eq" => Verb::Equals,
        "if-one" | "if-1" => Verb::BinIfOne,
        "if-zero" | "if-0" => Verb::BinIfZero,
        "vanishes" | "⪝" => Verb::Vanishes,
        x @ _ => {
            eprintln!("`{}`: unknown function", x);
            unimplemented!()
        }
    }
}

pub fn parse(source: &str) -> Result<Vec<AstNode>, Error<Rule>> {
    let mut ast = vec![];

    let pairs = CorsetParser::parse(Rule::corset, source)?.collect::<Vec<_>>();

    for pair in pairs {
        match &pair.as_rule() {
            Rule::corset => {
                for constraint in pair.into_inner() {
                    if constraint.as_rule() != Rule::EOI {
                        ast.push(build_ast_from_expr(constraint));
                    }
                }
            }
            _ => {}
        }
    }

    Ok(ast)
}
