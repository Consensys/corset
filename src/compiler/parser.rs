use color_eyre::eyre::*;
use pest::{iterators::Pair, Parser};
use std::fmt;
use std::fmt::Debug;

#[derive(Parser)]
#[grammar = "corset.pest"]
struct CorsetParser;

#[derive(Debug)]
pub struct ParsingAst {
    pub exprs: Vec<AstNode>,
}

#[derive(Debug, PartialEq, Clone)]
struct Verb {
    name: String,
}

#[derive(PartialEq, Clone)]
pub struct AstNode {
    pub class: Token,
    pub src: String,
    pub lc: (usize, usize),
}

impl AstNode {
    pub fn transform(self, f: &dyn Fn(AstNode) -> AstNode) -> Self {
        match self {
            AstNode {
                class: Token::TopLevelForm { args },
                src,
                lc,
            } => AstNode {
                class: Token::TopLevelForm {
                    args: args.into_iter().map(|n| n.transform(f)).collect(),
                },
                src,
                lc,
            },
            AstNode {
                class: Token::Form { args },
                src,
                lc,
            } => AstNode {
                class: Token::Form {
                    args: args.into_iter().map(|n| n.transform(f)).collect(),
                },
                src,
                lc,
            },
            _ => f(self),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Ignore,
    Value(i32),
    Symbol(String),
    Form { args: Vec<AstNode> },
    TopLevelForm { args: Vec<AstNode> },
    Range(Vec<usize>),
}
impl Debug for AstNode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fn format_list(cs: &[AstNode]) -> String {
            cs.iter()
                .map(|c| format!("{:?}", c))
                .collect::<Vec<_>>()
                .join(" ")
        }

        match self.class {
            Token::Ignore => write!(f, "IGNORED VALUE"),
            Token::Value(x) => write!(f, "{}:IMMEDIATE VALUE", x),
            Token::Symbol(ref name) => write!(f, "{}:SYMBOL", name),
            Token::Form { ref args } => write!(f, "'({})", format_list(args)),
            Token::TopLevelForm { ref args } => write!(f, "{}", format_list(args)),
            Token::Range(ref args) => write!(f, "{:?}", args),
        }
    }
}

fn build_ast_from_expr(pair: Pair<Rule>, in_def: bool) -> Result<AstNode> {
    let lc = pair.as_span().start_pos().line_col();
    let src = pair.as_str().to_owned();

    match pair.as_rule() {
        Rule::expr | Rule::constraint => {
            build_ast_from_expr(pair.into_inner().next().unwrap(), in_def)
        }
        Rule::definition | Rule::defcolumns => {
            let args = pair
                .into_inner()
                .into_iter()
                .map(|p| build_ast_from_expr(p, in_def))
                .collect::<Result<Vec<_>>>()?
                .into_iter()
                .filter(|x| x.class != Token::Ignore)
                .collect::<Vec<_>>();
            Ok(AstNode {
                class: Token::TopLevelForm { args },
                lc,
                src,
            })
        }
        Rule::list => {
            let args = pair
                .into_inner()
                .map(|p| build_ast_from_expr(p, in_def))
                .collect::<Result<Vec<_>>>()?
                .into_iter()
                .filter(|x| x.class != Token::Ignore)
                .collect::<Vec<_>>();
            Ok(AstNode {
                class: Token::Form { args },
                lc,
                src,
            })
        }
        Rule::symbol | Rule::defcolumn | Rule::definition_kw | Rule::defcolumn_kw => Ok(AstNode {
            class: Token::Symbol(pair.as_str().to_owned()),
            lc,
            src,
        }),
        Rule::integer => Ok(AstNode {
            class: Token::Value(pair.as_str().parse().unwrap()),
            lc,
            src,
        }),
        Rule::forloop => {
            let mut pairs = pair.into_inner();
            let for_token = AstNode {
                class: Token::Symbol("for".into()),
                lc,
                src: src.chars().take(3).collect::<String>(),
            };

            Ok(AstNode {
                class: Token::Form {
                    args: vec![
                        for_token,
                        build_ast_from_expr(pairs.next().unwrap(), in_def)?,
                        build_ast_from_expr(pairs.next().unwrap(), in_def)?,
                        build_ast_from_expr(pairs.next().unwrap(), in_def)?,
                    ],
                },
                lc,
                src,
            })
        }
        Rule::interval => {
            let mut pairs = pair.into_inner();
            let start = pairs.next().unwrap().as_str().parse::<usize>().unwrap();
            let stop = pairs.next().unwrap().as_str().parse::<usize>().unwrap();
            Ok(AstNode {
                class: Token::Range((start..=stop).collect()),
                lc,
                src,
            })
        }
        Rule::immediate_range => Ok(AstNode {
            class: Token::Range(
                pair.into_inner()
                    .map(|x| x.as_str().parse::<usize>().unwrap())
                    .collect(),
            ),
            lc,
            src,
        }),
        x => unimplemented!("{:?}", x),
    }
}

pub fn parse(source: &str) -> Result<ParsingAst> {
    let mut ast = ParsingAst { exprs: vec![] };

    for pair in CorsetParser::parse(Rule::corset, source)? {
        if pair.as_rule() == Rule::corset {
            for constraint in pair.into_inner() {
                if constraint.as_rule() != Rule::EOI {
                    ast.exprs.push(build_ast_from_expr(constraint, false)?);
                }
            }
        }
    }

    Ok(ast)
}
