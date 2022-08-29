use color_eyre::eyre::*;
use pest::{error::Error, iterators::Pair, Parser};
use std::collections::HashMap;

#[derive(Parser)]
#[grammar = "corset.pest"]
pub struct CorsetParser;

lazy_static::lazy_static! {
    static ref BUILTINS: HashMap<&'static str, Builtin> = maplit::hashmap!{
        "add" => Builtin::Add, "+" => Builtin::Add,


        "and" => Builtin::Mul,
        "mul" => Builtin::Mul,
        "*" => Builtin::Mul,

        "branch-if-zero" => Builtin::BranchIfZero,

        "sub" => Builtin::Sub,
        "-" => Builtin::Sub,

        "eq" => Builtin::Equals,
        "=" => Builtin::Equals,

        "if-one" => Builtin::BinIfOne,
        "if-zero" => Builtin::BinIfZero,
        "vanishes" => Builtin::Vanishes,
    };
}

pub(crate) trait Transpiler {
    fn render(&self, cs: &ConstraintsSet) -> Result<String>;
}

pub struct ConstraintsSet {
    pub constraints: Vec<AstNode>,
}
impl ConstraintsSet {
    pub fn from_str<S: AsRef<str>>(s: S) -> Result<Self> {
        let constraints = parse(s.as_ref())?;

        Ok(ConstraintsSet { constraints })
    }

    pub fn from_file<S: AsRef<str>>(filename: S) -> Result<Self> {
        let file_content = std::fs::read_to_string(filename.as_ref())?;
        Self::from_str(&file_content)
    }
}

#[derive(Debug, PartialEq)]
pub enum Builtin {
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
impl Builtin {}

#[derive(Debug, PartialEq)]
pub enum Verb {
    Ready(Builtin),
    Raw(String),
}

#[derive(Debug, PartialEq)]
enum Column {
    Ready(String),
    Raw(String),
}

#[derive(Debug, PartialEq)]
pub enum AstNode {
    Ignore,
    Value(i32),
    Column(String),
    Funcall { verb: Verb, args: Vec<AstNode> },
}
// impl AstNode {
// }

fn build_ast_from_expr(pair: Pair<Rule>) -> Result<AstNode> {
    // println!("parsing {:?}", pair.as_rule());
    match pair.as_rule() {
        Rule::expr | Rule::constraint => build_ast_from_expr(pair.into_inner().next().unwrap()),
        Rule::sexpr => {
            let mut content = pair.into_inner();
            let verb = content.next().unwrap().as_str();
            let args = content
                .map(|p| build_ast_from_expr(p))
                .collect::<Result<Vec<_>>>()?
                .into_iter()
                .filter(|x| *x != AstNode::Ignore)
                .collect();
            Ok(AstNode::Funcall {
                verb: Verb::Raw(verb.into()),
                args,
            })
        }
        Rule::column => Ok(AstNode::Column(pair.as_str().into())),
        Rule::value => Ok(AstNode::Value(pair.as_str().parse().unwrap())),
        // Rule::function => {}
        x @ _ => {
            dbg!(&x);
            Ok(AstNode::Ignore)
        }
    }
}

fn parse_verb(pair: Pair<Rule>) -> Builtin {
    match pair.as_str().to_lowercase().as_str() {
        x @ _ => {
            eprintln!("`{}`: unknown function", x);
            unimplemented!()
        }
    }
}

pub fn parse(source: &str) -> Result<Vec<AstNode>> {
    let mut ast = vec![];

    let pairs = CorsetParser::parse(Rule::corset, source)?.collect::<Vec<_>>();

    for pair in pairs {
        match &pair.as_rule() {
            Rule::corset => {
                for constraint in pair.into_inner() {
                    if constraint.as_rule() != Rule::EOI {
                        ast.push(build_ast_from_expr(constraint)?);
                    }
                }
            }
            _ => {}
        }
    }

    Ok(ast)
}
