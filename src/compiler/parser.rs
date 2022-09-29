use color_eyre::eyre::*;
use num_bigint::BigInt;
use num_traits::ToPrimitive;
use pest::{iterators::Pair, Parser};
use std::fmt;
use std::fmt::Debug;

use super::common::Type;

#[derive(Parser)]
#[grammar = "corset.pest"]
struct CorsetParser;

#[derive(Debug)]
pub struct Ast {
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
impl AstNode {
    pub fn into_symbol(&self) -> Option<String> {
        if let AstNode {
            class: Token::Symbol(x),
            ..
        } = self
        {
            Some(x.to_owned())
        } else {
            None
        }
    }
}
impl Debug for AstNode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        Debug::fmt(&self.class, f)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Kind<T> {
    Atomic,
    Composite(Box<T>),
    Interleaved(Vec<String>),
}

#[derive(PartialEq, Clone)]
pub enum Token {
    Value(BigInt),
    Symbol(String),
    Keyword(String),
    List(Vec<AstNode>),
    Range(Vec<usize>),
    Type(Type),

    DefModule(String),
    DefConsts(Vec<(String, BigInt)>),
    DefColumns(Vec<AstNode>),
    DefColumn(String, Type, Kind<AstNode>),
    DefSort(Vec<AstNode>, Vec<AstNode>),
    DefArrayColumn(String, Vec<usize>, Type),
    DefConstraint(String, Option<Vec<isize>>, Box<AstNode>),
    Defun(String, Vec<String>, Box<AstNode>),
    DefAliases(Vec<AstNode>),
    DefAlias(String, String),
    DefunAlias(String, String),
    DefPlookup(Vec<AstNode>, Vec<AstNode>),
}
const LIST_DISPLAY_THRESHOLD: usize = 4;
impl Debug for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fn format_list(cs: &[AstNode]) -> String {
            if cs.len() <= LIST_DISPLAY_THRESHOLD {
                cs.iter()
                    .map(|c| format!("{:?}", c))
                    .collect::<Vec<_>>()
                    .join(" ")
            } else {
                cs.iter()
                    .take(LIST_DISPLAY_THRESHOLD)
                    .map(|c| format!("{:?}", c))
                    .collect::<Vec<_>>()
                    .join(" ")
                    + " [...]"
            }
        }

        match self {
            Token::Value(x) => write!(f, "{}:IMMEDIATE", x),
            Token::Symbol(ref name) => write!(f, "{}:SYMBOL", name),
            Token::Keyword(ref name) => write!(f, "{}", name),
            Token::List(ref args) => write!(f, "({})", format_list(args)),
            Token::Range(ref args) => write!(f, "{:?}", args),
            Token::Type(t) => write!(f, "{:?}", t),

            Token::DefModule(name) => write!(f, "MODULE {}", name),
            Token::DefConsts(v) => {
                write!(
                    f,
                    "{}",
                    v.iter().fold(String::new(), |mut ax, c| {
                        ax.push_str(&format!("{}:CONST({})", c.0, c.1));
                        ax
                    })
                )
            }
            Token::DefColumns(cols) => write!(f, "DECLARATIONS {:?}", cols),
            Token::DefColumn(name, t, kind) => write!(f, "DECLARATION {}:{:?}{:?}", name, t, kind),
            Token::DefSort(to, from) => write!(f, "({:?}):PERMUTATION({:?})", to, from),
            Token::DefArrayColumn(name, range, t) => {
                write!(f, "DECLARATION {}{:?}{{{:?}}}", name, range, t)
            }
            Token::DefConstraint(name, ..) => write!(f, "{:?}:CONSTRAINT", name),
            Token::Defun(name, args, content) => {
                write!(f, "{}:({:?}) -> {:?}", name, args, content)
            }
            Token::DefAliases(cols) => write!(f, "ALIASES {:?}", cols),
            Token::DefAlias(from, to) => write!(f, "{} -> {}", from, to),
            Token::DefunAlias(from, to) => write!(f, "{} -> {}", from, to),
            Token::DefPlookup(parent, child) => write!(f, "{:?} ⊂ {:?}", parent, child),
        }
    }
}

impl AstNode {
    fn column_from(args: Vec<Pair<Rule>>, src: String, lc: LinCol) -> Result<Self> {
        let mut pairs = args.into_iter();
        let name = pairs.next().unwrap().as_str();

        let mut t = Type::Numeric;
        let mut range = None;
        let mut kind = Kind::Atomic;

        while let Some(x) = pairs.next() {
            match x.as_rule() {
                Rule::keyword => match x.as_str() {
                    ":ARRAY" => {
                        let n = pairs.next().map(rec_parse);
                        if let Some(Ok(AstNode {
                            class: Token::Range(r),
                            ..
                        })) = n
                        {
                            range = Some(r);
                        } else {
                            return Err(eyre!(
                                "expected RANGE, found `{}`",
                                n.map(|n| format!("{:?}", n.unwrap().class))
                                    .unwrap_or_else(|| "nothing".to_string())
                            ));
                        }
                    }
                    ":NATURAL" => t = Type::Numeric,
                    ":BOOLEAN" => t = Type::Boolean,
                    ":COMP" => {
                        let n = pairs.next().map(rec_parse);
                        if let Some(Ok(AstNode {
                            class: Token::List(_),
                            ..
                        })) = n
                        {
                            kind = Kind::Composite(Box::new(n.unwrap().unwrap()))
                        } else {
                            return Err(eyre!(
                                ":COMP expects FORM, found `{}`",
                                n.map(|n| format!("{:?}", n.unwrap().class))
                                    .unwrap_or_else(|| "nothing".to_string())
                            ));
                        }
                    }
                    ":INTERLEAVED" => {
                        let p = pairs.next().map(rec_parse);
                        if let Some(Ok(AstNode {
                            class: Token::List(ref froms),
                            ..
                        })) = p
                        {
                            if froms.iter().all(|f| {
                                matches!(
                                    f,
                                    AstNode {
                                        class: Token::Symbol(_),
                                        ..
                                    }
                                )
                            }) {
                                if kind != Kind::Atomic {
                                    return Err(eyre!(
                                        "`{}` can not be interleaved; is already {:?}",
                                        name,
                                        kind
                                    ));
                                } else {
                                    kind = Kind::Interleaved(
                                        froms
                                            .iter()
                                            .map(|f| {
                                                if let AstNode {
                                                    class: Token::Symbol(from),
                                                    ..
                                                } = f
                                                {
                                                    from.to_owned()
                                                } else {
                                                    unreachable!()
                                                }
                                            })
                                            .collect::<Vec<String>>(),
                                    );
                                }
                            } else {
                                return Err(eyre!(
                                    ":INTERLEAVED expects (SYMBOLS...), found `{:?}`",
                                    p
                                ));
                            }
                        } else {
                            return Err(eyre!(
                                ":INTERLEAVED expects (SYMBOLS...), found `{:?}`",
                                p
                            ));
                        }
                    }
                    x => unreachable!("{:?}", x),
                },
                _ => {
                    return Err(eyre!("expected :KEYWORD, found `{}`", x.as_str()));
                }
            }
        }

        if let Some(range) = range {
            if kind != Kind::Atomic {
                Err(eyre!("array columns must be atomic"))
            } else {
                Ok(AstNode {
                    class: Token::DefArrayColumn(name.into(), range, t),
                    lc,
                    src,
                })
            }
        } else {
            Ok(AstNode {
                class: Token::DefColumn(name.into(), t, kind),
                lc,
                src,
            })
        }
    }
    fn def_from(args: Vec<AstNode>, src: &str, lc: LinCol) -> Result<Self> {
        let tokens = args.iter().map(|x| x.class.clone()).collect::<Vec<_>>();
        match tokens.get(0) {
            Some(Token::Symbol(defkw)) if defkw == "defconst" => {
                if tokens.len() % 2 != 1 {
                    return Err(eyre!("DEFCONST expects an even number of arguments"));
                } else {
                    Ok(AstNode {
                        class: Token::DefConsts(
                            tokens[1..]
                                .chunks(2)
                                .map(|w| match (&w[0], &w[1]) {
                                    (Token::Symbol(name), Token::Value(x)) => {
                                        Ok((name.to_owned(), x.to_owned()))
                                    }
                                    _ => Err(eyre!(
                                        "DEFCONST expects (SYMBOL VALUE); received {:?}",
                                        &tokens[1..]
                                    )),
                                })
                                .collect::<Result<Vec<_>>>()?,
                        ),
                        lc,
                        src: src.to_string(),
                    })
                }
            }

            Some(Token::Symbol(defkw)) if defkw == "defun" => {
                match (&tokens.get(1), tokens.get(2)) {
                    (Some(Token::List(fargs)), Some(_))
                        if !fargs.is_empty()
                            && fargs.iter().all(|x| matches!(x.class, Token::Symbol(_))) =>
                    {
                        Ok(AstNode {
                            class: Token::Defun(
                                if let Token::Symbol(ref name) = fargs[0].class {
                                    name.to_string()
                                } else {
                                    unreachable!()
                                },
                                fargs
                                    .iter()
                                    .skip(1)
                                    .map(|a| {
                                        if let Token::Symbol(ref aa) = a.class {
                                            aa.to_owned()
                                        } else {
                                            unreachable!()
                                        }
                                    })
                                    .collect::<Vec<_>>(),
                                Box::new(args[2].clone()),
                            ),
                            src: src.into(),
                            lc,
                        })
                    }
                    _ => Err(eyre!(
                        "DEFUN expects ((SYMBOL SYMBOL*) FORM); received {:?}",
                        &tokens[1..]
                    )),
                }
            }

            Some(Token::Symbol(defkw)) if defkw == "defconstraint" => {
                match (tokens.get(1), tokens.get(2), tokens.get(3)) {
                    (Some(Token::Symbol(name)), Some(Token::List(domain)), Some(_))
                        if domain.is_empty()
                            || domain.iter().all(|d| {
                                matches!(
                                    d,
                                    AstNode {
                                        class: Token::Value(_),
                                        ..
                                    }
                                )
                            }) =>
                    {
                        let domain = if domain.is_empty() {
                            None
                        } else {
                            Some(
                                domain
                                    .iter()
                                    .map(|d| {
                                        if let AstNode {
                                            class: Token::Value(x),
                                            ..
                                        } = d
                                        {
                                            x.to_isize().unwrap()
                                        } else {
                                            unreachable!()
                                        }
                                    })
                                    .collect::<Vec<_>>(),
                            )
                        };
                        Ok(AstNode {
                            class: Token::DefConstraint(
                                name.into(),
                                domain,
                                Box::new(args[3].clone()),
                            ),
                            src: src.into(),
                            lc,
                        })
                    }
                    _ => Err(eyre!(
                        "DEFCONSTRAINT expects (NAME DOMAIN (EXP)); received {:?}",
                        &tokens[1..]
                    )),
                }
            }

            Some(Token::Symbol(defkw)) if defkw == "defalias" => {
                if tokens.len() % 2 != 1 {
                    Err(eyre!("DEFALIAS expects an even number of arguments"))
                } else if tokens.iter().skip(1).all(|x| matches!(x, Token::Symbol(_))) {
                    let mut defs = vec![];
                    for pair in tokens[1..].chunks(2) {
                        if let (Token::Symbol(from), Token::Symbol(to)) = (&pair[0], &pair[1]) {
                            defs.push(AstNode {
                                class: Token::DefAlias(from.into(), to.into()),
                                src: src.to_string(),
                                lc,
                            })
                        }
                    }
                    Ok(AstNode {
                        class: Token::DefAliases(defs),
                        src: src.into(),
                        lc,
                    })
                } else {
                    Err(eyre!(
                        "DEFALIAS expects (SYMBOL SYMBOL)*; received {:?}",
                        &tokens[1..]
                    ))
                }
            }

            Some(Token::Symbol(defkw)) if defkw == "defunalias" => {
                match (tokens.get(1), tokens.get(2)) {
                    (Some(Token::Symbol(from)), Some(Token::Symbol(to))) => Ok(AstNode {
                        class: Token::DefunAlias(from.into(), to.into()),
                        src: src.into(),
                        lc,
                    }),
                    _ => Err(eyre!(
                        "DEFUNALIAS expects (SYMBOL SYMBOL); received {:?}",
                        &tokens[1..]
                    )),
                }
            }

            Some(Token::Symbol(defkw)) if defkw == "defplookup" => {
                match (tokens.get(1), tokens.get(2)) {
                    (Some(Token::List(parent)), Some(Token::List(child))) => Ok(AstNode {
                        class: Token::DefPlookup(parent.to_owned(), child.to_owned()),
                        src: src.into(),
                        lc,
                    }),
                    _ => Err(eyre!(
                        "DEFPLOOKUP expects (PARENT:LIST CHILD:LIST); received {:?}",
                        &tokens[1..]
                    )),
                }
            }

            Some(Token::Symbol(defkw)) if defkw == "defpermutation" => {
                match (tokens.get(1), tokens.get(2), tokens.get(3)) {
                    (
                        Some(Token::List(to)),
                        Some(Token::List(from)),
                        Some(Token::List(sorters)),
                    ) => Ok(AstNode {
                        class: Token::DefSort(to.to_owned(), from.to_owned()),
                        src: src.into(),
                        lc,
                    }),
                    _ => Err(eyre!(
                        "DEFPERMUTATION expects (TO:LIST FROM:LIST SORTERS:LIST); received {:?}",
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

            Ok(AstNode::def_from(args, &src, lc).with_context(|| eyre!("parsing `{}`", &src))?)
        }
        Rule::list => {
            let args = pair
                .into_inner()
                .map(rec_parse)
                .collect::<Result<Vec<_>>>()?
                .into_iter()
                .collect::<Vec<_>>();
            Ok(AstNode {
                class: Token::List(args),
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
            let msg = pair.as_str().to_string();
            let pairs = pair.into_inner().collect::<Vec<_>>();
            AstNode::column_from(pairs, src, lc).with_context(|| format!("parsing `{}`", msg))
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
                class: Token::List(vec![
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
        Rule::typing => Ok(AstNode {
            class: Token::Type(match pair.as_str() {
                "NATURAL" => Type::Numeric,
                "BOOLEAN" => Type::Boolean,
                _ => unreachable!(),
            }),
            src,
            lc,
        }),
        Rule::defmodule => Ok(AstNode {
            class: Token::DefModule(pair.into_inner().next().unwrap().as_str().to_owned()),
            lc,
            src,
        }),
        Rule::keyword => Ok(AstNode {
            class: Token::Keyword(pair.as_str().to_owned()),
            src,
            lc,
        }),
        x => unimplemented!("{:?}", x),
    }
}

pub fn parse(source: &str) -> Result<Ast> {
    let mut ast = Ast { exprs: vec![] };

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
