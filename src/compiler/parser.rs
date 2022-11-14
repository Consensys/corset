use anyhow::{anyhow, Context, Result};
use num_bigint::BigInt;
use num_traits::ToPrimitive;
#[cfg(feature = "interactive")]
use pest::{iterators::Pair, Parser};
use serde::{Deserialize, Serialize};
use std::fmt;
use std::fmt::Debug;

use super::common::Type;
use super::{Handle, Magma};

#[cfg(feature = "interactive")]
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
    pub fn as_symbol(&self) -> Option<String> {
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

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Kind<T> {
    Atomic,
    Phantom,
    Composite(T),
    Interleaved(Vec<Handle>),
}
impl<T> Kind<T> {
    pub fn to_nil(&self) -> Kind<()> {
        match self {
            Kind::Atomic => Kind::Atomic,
            Kind::Phantom => Kind::Phantom,
            Kind::Composite(_) => Kind::Composite(()),
            Kind::Interleaved(froms) => Kind::Interleaved(froms.clone()),
        }
    }
}

#[allow(dead_code)]
#[derive(PartialEq, Clone)]
pub enum Symbol {
    Local(String),
    Path(Vec<String>),
}
impl std::fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Symbol::Local(s) => write!(f, "{}", &s),
            Symbol::Path(ss) => write!(f, "{}", ss.join(":")),
        }
    }
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
    DefColumn(String, Type, Kind<Box<AstNode>>),
    DefSort(Vec<AstNode>, Vec<AstNode>),

    DefInrange(Box<AstNode>, usize),
    DefArrayColumn(String, Vec<usize>, Type),
    DefConstraint(String, Option<Vec<isize>>, Box<AstNode>),
    Defun(String, Vec<String>, Box<AstNode>),
    DefAliases(Vec<AstNode>),
    DefAlias(String, String),
    DefunAlias(String, String),
    DefPlookup(String, Vec<AstNode>, Vec<AstNode>),
}
impl Token {
    pub fn depth(&self) -> usize {
        match self {
            Token::List(xs) => {
                let func = xs[0].as_symbol().unwrap();
                (if func == "begin" { 0 } else { 1 })
                    + xs.iter().map(|x| x.depth()).max().unwrap_or(0)
            }
            _ => 0,
        }
    }
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
            Token::DefInrange(exp, max) => write!(f, "{:?}E{}", exp, max),
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
            Token::DefPlookup(name, parent, child) => {
                write!(f, "{}: {:?} ⊂ {:?}", name, parent, child)
            }
        }
    }
}

impl AstNode {
    pub fn depth(&self) -> usize {
        self.class.depth()
    }
    #[cfg(feature = "interactive")]
    fn column_from(args: Vec<Pair<Rule>>, src: String, lc: LinCol) -> Result<Self> {
        let mut pairs = args.into_iter();
        let name = pairs.next().unwrap().as_str();

        let mut t = Type::Column(Magma::Integer);
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
                            return Err(anyhow!(
                                "expected RANGE, found `{}`",
                                n.map(|n| format!("{:?}", n.unwrap().class))
                                    .unwrap_or_else(|| "nothing".to_string())
                            ));
                        }
                    }
                    ":NATURAL" => t = Type::Column(Magma::Integer),
                    ":BOOLEAN" => t = Type::Column(Magma::Boolean),
                    ":COMP" => {
                        let n = pairs.next().map(rec_parse);
                        if let Some(Ok(AstNode {
                            class: Token::List(_),
                            ..
                        })) = n
                        {
                            kind = Kind::Composite(Box::new(n.unwrap().unwrap()))
                        } else {
                            return Err(anyhow!(
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
                                    return Err(anyhow!(
                                        "`{}` can not be interleaved; is already {:?}",
                                        name,
                                        kind
                                    ));
                                } else {
                                    kind = Kind::Interleaved(
                                        froms
                                            .iter()
                                            .map(|f| Handle::new("??", f.as_symbol().unwrap()))
                                            .collect(),
                                    );
                                }
                            } else {
                                return Err(anyhow!(
                                    ":INTERLEAVED expects (SYMBOLS...), found `{:?}`",
                                    p
                                ));
                            }
                        } else {
                            return Err(anyhow!(
                                ":INTERLEAVED expects (SYMBOLS...), found `{:?}`",
                                p
                            ));
                        }
                    }
                    x => unreachable!("{:?}", x),
                },
                _ => {
                    return Err(anyhow!("expected :KEYWORD, found `{}`", x.as_str()));
                }
            }
        }

        if let Some(range) = range {
            if kind != Kind::Atomic {
                Err(anyhow!("array columns must be atomic"))
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
                    return Err(anyhow!("DEFCONST expects an even number of arguments"));
                } else {
                    Ok(AstNode {
                        class: Token::DefConsts(
                            tokens[1..]
                                .chunks(2)
                                .map(|w| match (&w[0], &w[1]) {
                                    (Token::Symbol(name), Token::Value(x)) => {
                                        Ok((name.to_owned(), x.to_owned()))
                                    }
                                    _ => Err(anyhow!(
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
                    _ => Err(anyhow!(
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
                    _ => Err(anyhow!(
                        "DEFCONSTRAINT expects (NAME DOMAIN (EXP)); received {:?}",
                        &tokens[1..]
                    )),
                }
            }

            Some(Token::Symbol(defkw)) if defkw == "defalias" => {
                if tokens.len() % 2 != 1 {
                    Err(anyhow!("DEFALIAS expects an even number of arguments"))
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
                    Err(anyhow!(
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
                    _ => Err(anyhow!(
                        "DEFUNALIAS expects (SYMBOL SYMBOL); received {:?}",
                        &tokens[1..]
                    )),
                }
            }

            Some(Token::Symbol(defkw)) if defkw == "defplookup" => {
                match (tokens.get(1), tokens.get(2), tokens.get(3)) {
                    (
                        Some(Token::Symbol(name)),
                        Some(Token::List(parent)),
                        Some(Token::List(child)),
                    ) => Ok(AstNode {
                        class: Token::DefPlookup(
                            name.to_owned(),
                            parent.to_owned(),
                            child.to_owned(),
                        ),
                        src: src.into(),
                        lc,
                    }),
                    _ => Err(anyhow!(
                        "DEFPLOOKUP expects (NAME PARENT:LIST CHILD:LIST); received {:?}",
                        &tokens[1..]
                    )),
                }
            }

            Some(Token::Symbol(defkw)) if defkw == "definrange" => {
                match (tokens.get(1), tokens.get(2)) {
                    (Some(_), Some(Token::Value(range))) => Ok(AstNode {
                        class: Token::DefInrange(
                            Box::new(args[1].to_owned()),
                            range.to_usize().unwrap(),
                        ),
                        src: src.into(),
                        lc,
                    }),
                    _ => Err(anyhow!(
                        "DEFINRANGE expects (EXPRESSION RANGE); received {:?}",
                        &tokens[1..]
                    )),
                }
            }

            Some(Token::Symbol(defkw)) if defkw == "defpermutation" => {
                match (tokens.get(1), tokens.get(2)) {
                    (Some(Token::List(to)), Some(Token::List(from))) => Ok(AstNode {
                        class: Token::DefSort(to.to_owned(), from.to_owned()),
                        src: src.into(),
                        lc,
                    }),
                    _ => Err(anyhow!(
                        "DEFPERMUTATION expects (TO:LIST FROM:LIST SORTERS:LIST); received {:?}",
                        &tokens[1..]
                    )),
                }
            }

            x => unimplemented!("{:?}", x),
        }
    }
}

#[cfg(feature = "interactive")]
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

            Ok(AstNode::def_from(args, &src, lc).with_context(|| anyhow!("parsing `{}`", &src))?)
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
            let columns = pair
                .into_inner()
                .map(rec_parse)
                .collect::<Result<Vec<_>>>()?;
            Ok(AstNode {
                class: Token::DefColumns(columns),
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
                x => unimplemented!("{:?}", x),
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
                "NATURAL" => Type::Column(Magma::Integer),
                "BOOLEAN" => Type::Column(Magma::Boolean),
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

#[cfg(feature = "interactive")]
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
