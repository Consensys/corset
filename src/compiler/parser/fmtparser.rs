use super::{Ast, Token};
use anyhow::Result;
use num_bigint::BigInt;
use num_traits::One;
use pest::{
    iterators::{Pair, Pairs},
    Parser,
};
use std::fmt::Debug;

use super::AstNode;

#[derive(Parser)]
#[grammar = "corset-fmt.pest"]
struct FmtParser;

#[allow(dead_code)]
#[derive(PartialEq, Eq, Clone)]
/// a symbol can either be:
///   - Symbol::Local, i.e. relative to the current symbol table;
///   - Symbol::Path, i.e. a fully specified path (especially useful for lookups)
pub enum Symbol {
    Local(String),
    Path(Vec<String>),
}
impl std::fmt::Display for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Symbol::Local(s) => write!(f, "{}", &s),
            Symbol::Path(ss) => write!(f, "{}", ss.join(":")),
        }
    }
}

struct Commenter<'i> {
    pairs: Pairs<'i, Rule>,
    source: &'i str,
    current_inline: Option<String>,
}
impl<'i> Commenter<'i> {
    fn new(source: &'i str, pairs: Pairs<'i, Rule>) -> Self {
        Self {
            source,
            pairs,
            current_inline: None,
        }
    }
}

impl<'i> std::iter::Iterator for Commenter<'i> {
    type Item = Result<AstNode>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(comment) = self.current_inline.as_ref() {
            let class = Token::InlineComment(comment.to_string());
            self.current_inline = None;
            Some(Ok(AstNode {
                class,
                src: Default::default(),
                lc: (0, 0),
            }))
        } else if let Some(pair) = self.pairs.next() {
            match pair.as_rule() {
                Rule::EOI => None,
                // Free-standing are aggregated into blocks
                Rule::COMMENT => {
                    let mut acc = pair.as_str().to_string();
                    'look_for_comment: while let Some(next) = self.pairs.peek() {
                        if let Rule::COMMENT = next.as_rule() {
                            acc.push('\n');
                            acc.push_str(next.as_str());
                            let _ = self.pairs.next();
                        } else {
                            break 'look_for_comment;
                        }
                    }
                    Some(Ok(AstNode {
                        class: Token::BlockComment(acc),
                        src: Default::default(),
                        lc: (0, 0),
                    }))
                }
                // Inline comments are attached the node they follow
                _ => {
                    let span = pair.as_span();
                    let node = rec_parse(self.source, pair);
                    if node.is_ok() {
                        if let Some(next) = self.pairs.peek() {
                            if let Rule::COMMENT = next.as_rule() {
                                let comment = next.as_str();
                                let start = span.end();
                                let end = next.as_span().start();
                                if !self
                                    .source
                                    .bytes()
                                    .skip(start)
                                    .take(end - start + 1)
                                    .any(|c| c == b'\n')
                                {
                                    self.current_inline = Some(comment.to_owned());
                                    let _ = self.pairs.next();
                                }
                            }
                        }
                    }
                    Some(node)
                }
            }
        } else {
            None
        }
    }
}

fn rec_parse(source: &str, pair: Pair<Rule>) -> Result<AstNode> {
    use num_traits::{FromPrimitive, Num};

    use crate::compiler::Domain;

    let lc = pair.line_col();
    let src = pair.as_str().to_owned();

    match pair.as_rule() {
        Rule::expr => Commenter::new(source, pair.into_inner()).next().unwrap(),
        Rule::sexpr => {
            let args = Commenter::new(source, pair.into_inner()).collect::<Result<Vec<_>>>()?;
            Ok(AstNode {
                class: Token::List(args),
                lc,
                src,
            })
        }
        Rule::symbol => Ok(AstNode {
            class: Token::Symbol(pair.as_str().to_owned()),
            lc,
            src,
        }),
        Rule::integer => {
            let s = pair.as_str();
            let sign = if s.starts_with('-') {
                BigInt::from_i64(-1)
            } else {
                BigInt::from_i64(1)
            }
            .unwrap();
            let s = s.trim_start_matches('-');

            let value = if let Some(s) = s.strip_prefix("0x") {
                BigInt::from_str_radix(s, 16)
            } else if let Some(s) = s.strip_prefix("0b") {
                BigInt::from_str_radix(s, 2)
            } else {
                BigInt::from_str_radix(s, 10)
            };

            Ok(AstNode {
                class: Token::Value(value.unwrap() * sign),
                lc,
                src,
            })
        }
        Rule::interval => {
            let mut pairs = pair.into_inner();
            let x1 = pairs.next().map(|x| rec_parse(source, x)).transpose()?;
            let x2 = pairs.next().map(|x| rec_parse(source, x)).transpose()?;
            let x3 = pairs.next().map(|x| rec_parse(source, x)).transpose()?;
            let range = match (x1, x2, x3) {
                (Some(length), None, None) => Domain::Range(
                    AstNode {
                        class: Token::Value(BigInt::one()),
                        src: length.src.clone(),
                        lc,
                    },
                    length,
                ),
                (Some(start), Some(stop), None) => Domain::Range(start, stop),
                (Some(start), Some(stop), Some(step)) => Domain::SteppedRange(start, step, stop),
                x => unimplemented!("{} -> {:?}", src, x),
            };
            Ok(AstNode {
                class: Token::Domain(Box::new(range)),
                lc,
                src,
            })
        }
        Rule::immediate_range => Ok(AstNode {
            class: Token::Domain(Box::new(Domain::Set(
                pair.into_inner()
                    .map(|x| rec_parse(source, x))
                    .collect::<Result<Vec<_>>>()?,
            ))),
            lc,
            src,
        }),
        Rule::keyword => Ok(AstNode {
            class: Token::Keyword(pair.as_str().to_owned()),
            src,
            lc,
        }),
        Rule::nth => {
            let mut args = pair
                .into_inner()
                .map(|r| rec_parse(source, r))
                .collect::<Result<Vec<_>>>()?;
            let name = args[0].as_symbol().unwrap().to_owned();
            let index = Box::new(args.remove(1));
            Ok(AstNode {
                class: Token::IndexedSymbol { name, index },
                lc,
                src,
            })
        }
        Rule::COMMENT => Ok(AstNode {
            class: Token::BlockComment(pair.as_str().to_owned()),
            lc,
            src,
        }),
        x => {
            unimplemented!("{:?}", x)
        }
    }
}

pub fn parse(source: &str) -> Result<Ast> {
    let mut ast = Ast { exprs: vec![] };

    for pair in FmtParser::parse(Rule::corset, source)? {
        if pair.as_rule() == Rule::corset {
            for constraint in Commenter::new(source, pair.into_inner()) {
                ast.exprs.push(constraint?);
            }
        }
    }

    Ok(ast)
}
