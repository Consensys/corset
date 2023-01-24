use anyhow::{anyhow, Context, Result};
use colored::Colorize;
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

pub fn make_src_error(src: &str, lc: (usize, usize)) -> String {
    let src_str = src
        .chars()
        .take_while(|x| *x != '\n')
        .collect::<String>()
        .bold()
        .bright_white()
        .to_string();

    format!(
        "at line {}: {}{}",
        lc.0.to_string().blue(),
        src_str,
        if src_str.len() < src.len() { "..." } else { "" }.bright_white()
    )
}

#[derive(Debug)]
pub struct Ast {
    pub exprs: Vec<AstNode>,
}

type LinCol = (usize, usize);
#[derive(PartialEq, Clone)]
pub struct AstNode {
    /// the token in which this node devolves
    pub class: Token,
    /// the piece of code that produced this node
    pub src: String,
    /// position in the source file of the code of this node
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

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum Kind<T> {
    /// an atomic column is directly filled from traces
    Atomic,
    /// a phantom column is present, but will be filled later on
    Phantom,
    /// a composite column is similar to a phantom columns, but the expression
    /// computing it is known
    Composite(T),
    /// an interleaved column maintain references to its components
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
#[derive(PartialEq, Eq, Clone)]
/// a symbol can either be:
///   - Symbol::Local, i.e. relative to the current symbol table;
///   - Symbol::Path, i.e. a fully specified path (especially useful for plookups)
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
    /// an immediate value; can be “arbitrarily” large
    Value(BigInt),
    /// a symbol referencing another element of the tree
    Symbol(String),
    /// a keyword (typically a def*) that will be interpreted later on
    Keyword(String),
    /// a list of nodes
    List(Vec<AstNode>),
    /// a range; typically used in discrete constraints declaration and loops
    Range(Vec<usize>),
    /// the type of the node
    Type(Type),

    /// definition of a module; this will derive a symbol table
    DefModule(String),
    /// a list of constant definition: (name, value)
    DefConsts(Vec<(String, Box<AstNode>)>),
    /// a list of columns declaration, normally only DefColumn
    DefColumns(Vec<AstNode>),
    DefColumn {
        /// name of the column; unique in its module
        name: String,
        /// type of the column
        t: Type,
        /// how the values of the column are filled
        kind: Kind<Box<AstNode>>,
    },
    /// defines an array
    DefArrayColumn {
        name: String,
        domain: Vec<usize>,
        t: Type,
    },
    /// definition of a function
    Defun {
        /// name of the function; must be unique in its module
        name: String,
        /// the arguments are free strings, that will be resolved at evaluation
        args: Vec<String>,
        /// the body is any reasonable expression (should it be enforced?)
        body: Box<AstNode>,
    },
    Defpurefun(String, Vec<String>, Box<AstNode>),
    /// a list of aliases declaration, normally only DefAlias -- XXX should probably be removed
    DefAliases(Vec<AstNode>),
    DefAlias(String, String),
    /// Declaration of a function alias -- XXX should probably be removed
    DefunAlias(String, String),

    /// Declaration of a constraint;
    DefConstraint {
        /// the given name of the constraint -- TODO enforce uniqueness
        name: String,
        /// if the domain of the constraint is `None`, it is supposed to hold everywhere
        domain: Option<Vec<isize>>,
        /// this expression has to reduce to 0 for the constraint to be satisfied
        exp: Box<AstNode>,
    },
    /// declaration of a permutation constraint between two sets of columns
    DefPermutation {
        from: Vec<AstNode>,
        to: Vec<AstNode>,
    },
    /// declaration of a plookup constraint between two sets of columns
    DefPlookup {
        name: String,
        including: Vec<AstNode>,
        included: Vec<AstNode>,
    },
    /// this constraint ensures that exp remains lesser than max
    DefInrange(Box<AstNode>, usize),
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
            Token::Value(x) => write!(f, "{}", x),
            Token::Symbol(ref name) => write!(f, "{}", name),
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
                        ax.push_str(&format!("{}:CONST({:?})", c.0, c.1));
                        ax
                    })
                )
            }
            Token::DefColumns(cols) => write!(f, "DECLARATIONS {:?}", cols),
            Token::DefColumn { name, t, kind } => {
                write!(f, "DECLARATION {}:{:?}{:?}", name, t, kind)
            }
            Token::DefPermutation { from, to } => {
                write!(f, "({:?}):PERMUTATION({:?})", to, from)
            }
            Token::DefInrange(exp, max) => write!(f, "{:?}E{}", exp, max),
            Token::DefArrayColumn {
                name,
                domain: range,
                t,
            } => {
                write!(f, "DECLARATION {}{:?}{{{:?}}}", name, range, t)
            }
            Token::DefConstraint { name, .. } => write!(f, "{:?}:CONSTRAINT", name),
            Token::Defun {
                name,
                args,
                body: content,
            } => {
                write!(f, "{}:({:?}) -> {:?}", name, args, content)
            }
            Token::Defpurefun(name, args, content) => {
                write!(f, "{}:({:?}) -> {:?}", name, args, content)
            }
            Token::DefAliases(cols) => write!(f, "ALIASES {:?}", cols),
            Token::DefAlias(from, to) => write!(f, "{} -> {}", from, to),
            Token::DefunAlias(from, to) => write!(f, "{} -> {}", from, to),
            Token::DefPlookup {
                name,
                including,
                included,
            } => {
                write!(f, "{}: {:?} ⊂ {:?}", name, including, included)
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

        let mut t: Option<Type> = None;
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
                    ":NATURAL" => {
                        if let Some(tt) = t {
                            return Err(anyhow!(
                                "{} is already of type {:?}; can not be of type {:?}",
                                name,
                                tt,
                                x.as_str()
                            ));
                        } else {
                            t = Some(Type::Column(Magma::Integer))
                        }
                    }
                    ":BYTE" => {
                        if let Some(tt) = t {
                            return Err(anyhow!(
                                "{} is already of type {:?}; can not be of type {:?}",
                                name,
                                tt,
                                x.as_str()
                            ));
                        } else {
                            t = Some(Type::Column(Magma::Byte))
                        }
                    }
                    ":NIBBLE" => {
                        if let Some(tt) = t {
                            return Err(anyhow!(
                                "{} is already of type {:?}; can not be of type {:?}",
                                name,
                                tt,
                                x.as_str()
                            ));
                        } else {
                            t = Some(Type::Column(Magma::Nibble))
                        }
                    }
                    ":BOOLEAN" => {
                        if let Some(tt) = t {
                            return Err(anyhow!(
                                "{} is already of type {:?}; can not be of type {:?}",
                                name,
                                tt,
                                x.as_str()
                            ));
                        } else {
                            t = Some(Type::Column(Magma::Boolean))
                        }
                    }
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
                    x => return Err(anyhow!("unknown column attribute {:?}", x)),
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
                    class: Token::DefArrayColumn {
                        name: name.into(),
                        domain: range,
                        t: t.unwrap_or(Type::Column(Magma::Integer)),
                    },
                    lc,
                    src,
                })
            }
        } else {
            Ok(AstNode {
                class: Token::DefColumn {
                    name: name.into(),
                    t: t.unwrap_or(Type::Column(Magma::Integer)),
                    kind,
                },
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
                    Err(anyhow!("DEFCONST expects an even number of arguments"))
                } else {
                    Ok(AstNode {
                        class: Token::DefConsts(
                            args[1..]
                                .chunks(2)
                                .map(|w| match &w[0] {
                                    AstNode {
                                        class: Token::Symbol(name),
                                        ..
                                    } => Ok((name.to_owned(), Box::new(w[1].clone()))),
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
                if tokens.len() > 3 {
                    return Err(anyhow!(
                        "DEFUN expects one body, {} found",
                        tokens.len() - 2,
                    ));
                }
                match (&tokens.get(1), tokens.get(2)) {
                    (Some(Token::List(fargs)), Some(_))
                        if !fargs.is_empty()
                            && fargs.iter().all(|x| matches!(x.class, Token::Symbol(_))) =>
                    {
                        Ok(AstNode {
                            class: Token::Defun {
                                name: if let Token::Symbol(ref name) = fargs[0].class {
                                    name.to_string()
                                } else {
                                    unreachable!()
                                },
                                args: fargs
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
                                body: Box::new(args[2].clone()),
                            },
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

            Some(Token::Symbol(defkw)) if defkw == "defpurefun" => {
                if tokens.len() > 3 {
                    return Err(anyhow!(
                        "DEFPUREFUN expects one body, {} found",
                        tokens.len() - 2,
                    ));
                }
                match (&tokens.get(1), tokens.get(2)) {
                    (Some(Token::List(fargs)), Some(_))
                        if !fargs.is_empty()
                            && fargs.iter().all(|x| matches!(x.class, Token::Symbol(_))) =>
                    {
                        Ok(AstNode {
                            class: Token::Defpurefun(
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
                            class: Token::DefConstraint {
                                name: name.into(),
                                domain,
                                exp: Box::new(args[3].clone()),
                            },
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
                        Some(Token::List(including)),
                        Some(Token::List(included)),
                    ) => Ok(AstNode {
                        class: Token::DefPlookup {
                            name: name.to_owned(),
                            including: including.to_owned(),
                            included: included.to_owned(),
                        },
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
                        class: Token::DefPermutation {
                            from: from.to_owned(),
                            to: to.to_owned(),
                        },
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

            Ok(AstNode::def_from(args, &src, lc).with_context(|| make_src_error(&src, lc))?)
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
            let pairs = pair.into_inner().collect::<Vec<_>>();
            AstNode::column_from(pairs, src.clone(), lc).with_context(|| make_src_error(&src, lc))
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
                "NATURAL" | "BYTE" => Type::Column(Magma::Integer),
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
