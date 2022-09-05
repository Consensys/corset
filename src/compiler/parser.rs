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

type LinCol = (usize, usize);
#[derive(PartialEq, Clone)]
pub struct AstNode {
    pub class: Token,
    pub src: String,
    pub lc: LinCol,
}
impl Debug for AstNode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        Debug::fmt(&self.class, f)
    }
}

#[derive(PartialEq, Clone)]
pub enum Token {
    Ignore,
    Value(i32),
    Symbol(String),
    Form(Vec<AstNode>),
    TopLevelForm(Vec<AstNode>),
    Range(Vec<usize>),

    DefConst(String, usize),
    DefColumns(Vec<AstNode>),
    DefColumn(String),
    DefArrayColumn(String, Vec<usize>),
    DefConstraint(String, Box<AstNode>),
    Defun(String, Vec<String>, Box<AstNode>),
    DefAliases(Vec<AstNode>),
    DefAlias(String, String),
    DefunAlias(String, String),
}
impl Debug for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fn format_list(cs: &[AstNode]) -> String {
            if cs.len() <= 2 {
                cs.iter()
                    .map(|c| format!("{:?}", c))
                    .collect::<Vec<_>>()
                    .join(" ")
            } else {
                cs.iter()
                    .take(2)
                    .map(|c| format!("{:?}", c))
                    .collect::<Vec<_>>()
                    .join(" ")
                    + " ..."
            }
        }

        match self {
            Token::Ignore => write!(f, "IGNORED VALUE"),
            Token::Value(x) => write!(f, "{}:IMMEDIATE", x),
            Token::Symbol(ref name) => write!(f, "{}:SYMBOL", name),
            Token::Form(ref args) => write!(f, "({})", format_list(args)),
            Token::TopLevelForm(ref args) => write!(f, "{}", format_list(args)),
            Token::Range(ref args) => write!(f, "{:?}", args),

            Token::DefConst(name, value) => write!(f, "{}:CONST({})", name, value),
            Token::DefColumns(cols) => write!(f, "DECLARATIONS {:?}", cols),
            Token::DefColumn(name) => write!(f, "DECLARATION {}", name),
            Token::DefArrayColumn(name, range) => write!(f, "DECLARATION {}{:?}", name, range),
            Token::DefConstraint(name, _) => write!(f, "{:?}:CONSTRAINT", name),
            Token::Defun(name, args, content) => {
                write!(f, "{}:({:?}) -> {:?}", name, args, content)
            }
            Token::DefAliases(cols) => write!(f, "ALIASES {:?}", cols),
            Token::DefAlias(from, to) => write!(f, "{} -> {}", from, to),
            Token::DefunAlias(from, to) => write!(f, "{} -> {}", from, to),
        }
    }
}

impl AstNode {
    fn from(args: Vec<AstNode>, src: String, lc: LinCol) -> Result<Self> {
        let tokens = args
            .iter()
            .filter(|x| x.class != Token::Ignore)
            .map(|x| x.class.clone())
            .collect::<Vec<_>>();
        match tokens.get(0) {
            Some(Token::Symbol(defkw)) if defkw == "defconst" => {
                match (tokens.get(1), tokens.get(2)) {
                    (Some(Token::Symbol(name)), Some(Token::Value(x))) => Ok(AstNode {
                        class: Token::DefConst(name.into(), *x as usize),
                        src,
                        lc,
                    }),
                    _ => Err(eyre!(
                        "DEFCONST expects (SYMBOL, VALUE); received {:?}",
                        &tokens[1..]
                    )),
                }
            }

            Some(Token::Symbol(defkw)) if defkw == "defun" => {
                match (&tokens.get(1), tokens.get(2), tokens.get(3)) {
                    (Some(Token::Symbol(name)), Some(Token::Form(fargs)), Some(_))
                        if fargs.iter().all(|x| matches!(x.class, Token::Symbol(_))) =>
                    {
                        Ok(AstNode {
                            class: Token::Defun(
                                name.into(),
                                fargs
                                    .into_iter()
                                    .map(|a| {
                                        if let Token::Symbol(ref aa) = a.class {
                                            aa.to_owned()
                                        } else {
                                            unreachable!()
                                        }
                                    })
                                    .collect::<Vec<_>>(),
                                Box::new(args[3].clone()),
                            ),
                            src,
                            lc,
                        })
                    }
                    _ => Err(eyre!(
                        "DEFUN expects (SYMBOL (SYMBOL*), FORM); received {:?}",
                        &tokens[1..]
                    )),
                }
            }

            Some(Token::Symbol(defkw)) if defkw == "defconstraint" => {
                match (tokens.get(1), tokens.get(2)) {
                    (Some(Token::Symbol(name)), Some(_)) => Ok(AstNode {
                        class: Token::DefConstraint(name.into(), Box::new(args[2].clone())),
                        src,
                        lc,
                    }),
                    _ => Err(eyre!(
                        "DEFCONSTRAINT expects (SYMBOL, *); received {:?}",
                        &tokens[1..]
                    )),
                }
            }

            Some(Token::Symbol(defkw)) if defkw == "defalias" => {
                if tokens.len() % 2 != 1 {
                    Err(eyre!("DEFALIAS expects an even number of arguments"))
                } else {
                    if tokens.iter().skip(1).all(|x| matches!(x, Token::Symbol(_))) {
                        let mut defs = vec![];
                        for pair in tokens[1..].chunks(2) {
                            if let (Token::Symbol(from), Token::Symbol(to)) = (&pair[0], &pair[1]) {
                                defs.push(AstNode {
                                    class: Token::DefAlias(from.into(), to.into()),
                                    src: src.clone(),
                                    lc,
                                })
                            }
                        }
                        Ok(AstNode {
                            class: Token::DefAliases(defs),
                            src,
                            lc,
                        })
                    } else {
                        Err(eyre!(
                            "DEFALIAS expects (SYMBOL, SYMBOL)*; received {:?}",
                            &tokens[1..]
                        ))
                    }
                }
            }

            Some(Token::Symbol(defkw)) if defkw == "defunalias" => {
                match (tokens.get(1), tokens.get(2)) {
                    (Some(Token::Symbol(from)), Some(Token::Symbol(to))) => Ok(AstNode {
                        class: Token::DefunAlias(from.into(), to.into()),
                        src,
                        lc,
                    }),
                    _ => Err(eyre!(
                        "DEFUNALIAS expects (SYMBOL, SYMBOL); received {:?}",
                        &tokens[1..]
                    )),
                }
            }

            x => unimplemented!("{:?}", x),
        }
    }
}

fn rec_parse(pair: Pair<Rule>) -> Result<AstNode> {
    let lc = pair.as_span().start_pos().line_col();
    let src = pair.as_str().to_owned();

    match pair.as_rule() {
        Rule::expr | Rule::constraint => rec_parse(pair.into_inner().next().unwrap()),
        Rule::definition => {
            let args = pair
                .into_inner()
                .into_iter()
                .map(rec_parse)
                .collect::<Result<Vec<_>>>()?;

            AstNode::from(args, src, lc)
        }
        Rule::list => {
            let args = pair
                .into_inner()
                .map(rec_parse)
                .collect::<Result<Vec<_>>>()?
                .into_iter()
                .filter(|x| x.class != Token::Ignore)
                .collect::<Vec<_>>();
            Ok(AstNode {
                class: Token::Form(args),
                lc,
                src,
            })
        }
        Rule::symbol | Rule::definition_kw => Ok(AstNode {
            class: Token::Symbol(pair.as_str().to_owned()),
            lc,
            src,
        }),
        Rule::defcolumns => {
            let defs = pair
                .into_inner()
                .map(rec_parse)
                .collect::<Result<Vec<_>>>()?;
            Ok(AstNode {
                class: Token::DefColumns(defs),
                lc,
                src,
            })
        }
        Rule::defcolumn => {
            let mut pairs = pair.into_inner();
            let name = pairs.next().unwrap().as_str();
            if let Some(Ok(AstNode {
                class: Token::Range(range),
                ..
            })) = pairs.next().map(rec_parse)
            {
                Ok(AstNode {
                    class: Token::DefArrayColumn(name.into(), range),
                    lc,
                    src,
                })
            } else {
                Ok(AstNode {
                    class: Token::DefColumn(name.into()),
                    lc,
                    src,
                })
            }
        }
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
                class: Token::Form(vec![
                    for_token,
                    rec_parse(pairs.next().unwrap())?,
                    rec_parse(pairs.next().unwrap())?,
                    rec_parse(pairs.next().unwrap())?,
                ]),
                lc,
                src,
            })
        }
        Rule::interval => {
            let mut pairs = pair.into_inner();
            let x1 = pairs
                .next()
                .map(|x| x.as_str())
                .and_then(|x| x.parse::<usize>().ok());
            let x2 = pairs
                .next()
                .map(|x| x.as_str())
                .and_then(|x| x.parse::<usize>().ok());
            let x3 = pairs
                .next()
                .map(|x| x.as_str())
                .and_then(|x| x.parse::<usize>().ok());
            let range = match (x1, x2, x3) {
                (Some(start), None, None) => (1..=start).collect(),
                (Some(start), Some(stop), None) => (start..=stop).collect(),
                (Some(start), Some(stop), Some(step)) => (start..=stop).step_by(step).collect(),
                _ => unimplemented!(),
            };
            Ok(AstNode {
                class: Token::Range(range),
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
                    ast.exprs.push(rec_parse(constraint)?);
                }
            }
        }
    }

    Ok(ast)
}
