use pest::{iterators::Pair, Parser, error::Error};
use std::collections::HashMap;
use color_eyre::eyre::*;

#[derive(Parser)]
#[grammar = "corset.pest"]
pub struct CorsetParser;


pub struct ConstraintsSet {
    settings: crate::Args,
    constraints: Vec<AstNode>
}
impl ConstraintsSet {
    pub fn from_str<S: AsRef<str>>(s: S, settings: &crate::Args) -> Result<Self> {
        let constraints = parse(s.as_ref())?;

        Ok(ConstraintsSet{
            settings: settings.clone(),
            constraints,
        })
    }

    pub fn from_file<S: AsRef<str>>(filename: S, settings: &crate::Args) -> Result<Self> {
        let file_content = std::fs::read_to_string(filename.as_ref())?;
        Self::from_str(&file_content, settings)
    }

    pub fn render(&self) -> Vec<String> {
        self.constraints.iter().map(|c| c.render()).collect()
    }
}

fn make_chain(xs: &[AstNode], operand: &str) -> String {
    let head = xs[0].render();
    let tail = xs[1..].iter()
        .map(|x| format!("{}({})", operand, AstNode::render(x))).collect::<Vec<_>>().join(".");
    format!("{}{}", head, tail)
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
    pub fn render(&self, args: &[AstNode]) -> String {
        match self {
            Verb::BranchIfZero => {
                let cond = args[0].render();
                let then = args[1..].iter().map(AstNode::render).collect::<Vec<_>>().join(", ");
                format!("{}.BranchIfZero({})", cond, then)
            }
            Verb::Add => {
                format!("({}.Add({}))", args[0].render(), args[1].render())
            }
            Verb::And => {
                let others = args[1..].iter()
                    .map(|x| format!(".Mul({})", x.render())).collect::<Vec<_>>().join("");
                format!("{}{}", args[0].render(), others)
            }
            Verb::Mul => {
                let others = args[1..].iter()
                    .map(|x| format!(".Mul({})", x.render())).collect::<Vec<_>>().join("");
                format!("{}{}", args[0].render(), others)
            }
            Verb::Sub => {
                let others = args[1..].iter()
                    .map(|x| format!(".Sub({})", x.render())).collect::<Vec<_>>().join("");
                format!("({}{})", args[0].render(), others)
            }
            Verb::Equals => {
                make_chain(args, ".Equals")
            }
            _ => todo!()
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum AstNode {
    Ignore,
    Value(i32),
    Column(String),
    Function {
        verb: Verb,
        exprs: Vec<AstNode>
    },
}
impl AstNode {
    pub fn render(&self) -> String {
        match self {
            Self::Value(x) => match x {
                0..=2 | 127 | 256 => format!("CONST_{}()", x),
                x @ _ => format!("CONST_UINT64({})", x)
            }
            Self::Column(name) => format!("CE[{}]", name),
            Self::Function { verb, exprs } => {
                verb.render(exprs)
            }
            _ => "??".into()
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
            let exprs = content.map(|p| build_ast_from_expr(p)).filter(|x| *x != AstNode::Ignore).collect();
            AstNode::Function {
                verb,
                exprs
            }
        }
        Rule::column => {
            AstNode::Column(pair.as_str().into())
        }
        Rule::value => {
            AstNode::Value(pair.as_str().parse().unwrap())
        }
        // Rule::function => {}
        x @ _ => {
            dbg!(&x);
            AstNode::Ignore
        }
    }
}

fn parse_verb(pair: Pair<Rule>) -> Verb {
    match pair.as_str().to_lowercase().as_str() {
        "add" => Verb::Add,
        "and" => Verb::And,
        "mul" => Verb::Mul,
        "branch-if-zero" => Verb::BranchIfZero,
        "sub" => Verb::Sub,
        "eq" | "=" => Verb::Equals,
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
