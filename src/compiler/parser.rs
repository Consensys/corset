use anyhow::{anyhow, bail, Context, Result};
use colored::Colorize;
use itertools::Itertools;
use num_bigint::BigInt;
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
    pub fn depth(&self) -> usize {
        self.class.depth()
    }
    pub fn as_u64(&self) -> Result<u64> {
        if let Token::Value(r) = &self.class {
            r.try_into().map_err(|e| anyhow!("{:?}", e))
        } else {
            bail!("expected usize, found `{:?}`", self)
        }
    }
    pub fn as_range(&self) -> Result<&[isize]> {
        if let Token::Range(r) = &self.class {
            Ok(r)
        } else {
            bail!("expected range, found `{:?}`", self)
        }
    }
    pub fn as_symbol(&self) -> Result<&str> {
        if let Token::Symbol(x) = &self.class {
            Ok(x)
        } else {
            bail!("expected symbol, found `{:?}`", self)
        }
    }
    pub fn as_list(&self) -> Result<&[AstNode]> {
        if let Token::List(xs) = &self.class {
            Ok(xs)
        } else {
            bail!("expected list, found `{:?}`", self)
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
    Range(Vec<isize>),
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
    Defpurefun {
        name: String,
        args: Vec<String>,
        body: Box<AstNode>,
    },
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
        /// an expression that enables the constraint only when it is non zero
        guard: Option<Box<AstNode>>,
        /// this expression has to reduce to 0 for the constraint to be satisfied
        body: Box<AstNode>,
    },
    /// declaration of a permutation constraint between two sets of columns
    DefPermutation {
        from: Vec<String>,
        to: Vec<String>,
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
            Token::Defpurefun { name, args, body } => {
                write!(f, "{}:({:?}) -> {:?}", name, args, body)
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

fn parse_defconstraint<I: Iterator<Item = Result<AstNode>>>(
    mut tokens: I,
    lc: (usize, usize),
    src: String,
) -> Result<AstNode> {
    enum GuardParser {
        Begin,
        Guard,
        Domain,
    }

    let name = tokens
        .next()
        .with_context(|| anyhow!("missing constraint name"))??
        .as_symbol()?
        .to_owned();

    let (domain, guard) = {
        let guards = tokens
            .next()
            .with_context(|| anyhow!("missing guards in constraint definitions"))??
            .as_list()?
            .to_vec();
        let mut status = GuardParser::Begin;
        let mut guard_tokens = guards.iter();
        let mut domain = None;
        let mut guard = None;
        while let Some(x) = guard_tokens.next() {
            match status {
                GuardParser::Begin => match x.class {
                    Token::Keyword(ref kw) if kw == ":guard" => status = GuardParser::Guard,
                    Token::Keyword(ref kw) if kw == ":domain" => status = GuardParser::Domain,
                    _ => bail!("expected :guard or :domain, found `{:?}`", x),
                },
                GuardParser::Guard => {
                    if guard.is_some() {
                        bail!("guard already defined: `{:?}`", guard.unwrap())
                    } else {
                        guard = Some(Box::new(x.clone()));
                        status = GuardParser::Begin;
                    }
                }
                GuardParser::Domain => {
                    if domain.is_some() {
                        bail!("domain already defined: `{:?}`", domain.unwrap())
                    } else {
                        if let Token::Range(range) = &x.class {
                            domain = Some(range.to_owned())
                        } else {
                            bail!("expected range, found `{:?}`", x)
                        }
                        status = GuardParser::Begin;
                    }
                }
            }
        }

        match status {
            GuardParser::Begin => {}
            GuardParser::Guard => bail!("expected guard expression, found nothing"),
            GuardParser::Domain => bail!("expected domain value, found nothing"),
        }

        (domain, guard)
    };

    let body = Box::new(
        tokens
            .next()
            .with_context(|| anyhow!("missing constraint name"))??
            .to_owned(),
    );

    Ok(AstNode {
        class: Token::DefConstraint {
            name,
            domain,
            guard,
            body,
        },
        src: src.into(),
        lc,
    })
}

fn parse_defcolumns<I: Iterator<Item = Result<AstNode>>>(
    pair: I,
    lc: (usize, usize),
    src: String,
) -> Result<AstNode> {
    enum ColumnParser {
        Begin,
        Array,
        Computation,
        Interleaved,
    }

    // A columns definition is a list of column definition
    let columns = pair
        .map(|c| {
            c.and_then(|c| {
                let name;
                let mut t = None;
                let mut kind = Kind::Atomic;
                let mut range = None;
                let mut state = ColumnParser::Begin;
                // A column is either defined by...
                match c.class {
                    // ...a name, in which case the column is an atomic fr column...
                    Token::Symbol(_name) => name = _name.to_owned(),
                    // ...or a list, specifying attributes for this column.
                    Token::List(xs) => {
                        let mut xs = xs.iter();
                        let name_token = xs
                            .next()
                            .ok_or(anyhow!("expected column name, found empty list"))?;
                        // The first element of the llist *has* to be the name of the column
                        if let Token::Symbol(ref _name) = name_token.class {
                            name = _name.to_owned();
                        } else {
                            bail!("expected column name, found `{:?}`", name_token)
                        }
                        // Then can come all the attributes, in no particular order.
                        while let Some(x) = xs.next() {
                            state = match state {
                                ColumnParser::Begin => match x.class {
                                    Token::Keyword(ref kw) => {
                                        // e.g. (A ... :integer ...)
                                        match kw.to_lowercase().as_str() {
                                            ":boolean" | ":bool" | ":nibble" | ":byte"
                                                | ":integer" => {
                                                    if t.is_some() {
                                                        bail!(
                                                            "trying to redefine column {} of type {:?} as {}",
                                                            name, t.unwrap(), kw
                                                        )
                                                    } else {
                                                        t = Some(Type::Column(
                                                            kw.as_str().try_into()?,
                                                        ));
                                                    }
                                                    ColumnParser::Begin
                                                }
                                            // e.g. (A ... :interleaved (X Y Z) ...)
                                            ":interleaved" => ColumnParser::Interleaved,
                                            // not really used for now.
                                            ":comp" => ColumnParser::Computation,
                                            // e.g. (A :array {1 3 5}) or (A :array [5])
                                            ":array" => ColumnParser::Array,
                                            _ => {
                                                bail!("unexpected keyword found: {}", kw)
                                            }
                                        }
                                    }
                                    // A range alone treated as if it were preceded by :array
                                    Token::Range(ref _range) => {
                                        range = Some(_range.to_owned());
                                        ColumnParser::Begin
                                    },
                                    _ => bail!("expected keyword, found `{:?}`", x),
                                },
                                // :array expects a range defining the domain of the column array
                                ColumnParser::Array => {
                                    range = Some(x.as_range()?.to_owned());
                                    ColumnParser::Begin
                                }
                                ColumnParser::Computation => todo!(),
                                // :interleaved expects a list of column to interleave into the one
                                // being defined
                                ColumnParser::Interleaved => {
                                    if kind != Kind::Atomic {
                                        bail!("column {} can not be interleaved; is already {:?}", name, kind)
                                    }
                                    kind = Kind::Interleaved(
                                        x.as_list()?
                                            .iter()
                                            .map(|f| {
                                                f.as_symbol().map(|n| Handle::new("??", n))
                                            })
                                            .collect::<Result<Vec<_>>>()?,
                                    );
                                    ColumnParser::Begin
                                }
                            };
                        }
                    }
                    _ => unreachable!(),
                };
                // Ensure that we are in a clean state
                match state {
                    ColumnParser::Begin =>
                        Ok(AstNode {
                            class: if let Some(range) = range {
                                if kind != Kind::Atomic {
                                    bail!("array columns must be atomic")
                                }
                                Token::DefArrayColumn {
                                    name,
                                    t: t.unwrap_or(Type::Column(Magma::Integer)),
                                    domain: range
                                        .iter()
                                        .map(|&x| x.try_into().map_err(|e| anyhow!("{:?}", e)))
                                        .collect::<Result<Vec<_>>>()?,
                                }
                            } else {
                                Token::DefColumn {
                                    name,
                                    t: t.unwrap_or(Type::Column(Magma::Integer)),
                                    kind,
                                }
                            },
                            lc: c.lc,
                            src: c.src.clone(),
                        }),
                    ColumnParser::Array => bail!("incomplete :array definition"),
                    ColumnParser::Computation => bail!("incomplate :comp definition"),
                    ColumnParser::Interleaved => bail!("incomplete :interleaved definition"),
                }
            })
        })
        .collect::<Result<Vec<_>>>()
        .with_context(|| make_src_error(&src, lc))?;

    Ok(AstNode {
        class: Token::DefColumns(columns),
        lc,
        src,
    })
}

fn parse_definition(pair: Pair<Rule>) -> Result<AstNode> {
    let lc = pair.as_span().start_pos().line_col();
    let src = pair.as_str().to_owned();

    let mut tokens = pair.into_inner().map(rec_parse);

    match tokens.next().unwrap().unwrap().as_symbol()? {
        "module" => {
            let name = tokens
                .next()
                .with_context(|| anyhow!("module name missing"))??
                .as_symbol()?
                .to_owned();
            Ok(AstNode {
                class: Token::DefModule(name),
                lc,
                src,
            })
        }
        "defcolumns" => parse_defcolumns(tokens, lc, src),
        "defconst" => Ok(AstNode {
            class: Token::DefConsts(
                tokens
                    .chunks(2)
                    .into_iter()
                    .map(|mut chunk| {
                        let name = chunk
                            .next()
                            .ok_or_else(|| anyhow!("adsf"))??
                            .as_symbol()
                            .with_context(|| anyhow!("invalid constant name"))?
                            .to_owned();
                        let value = chunk
                            .next()
                            .ok_or_else(|| anyhow!("expected value for {}", name))??;
                        Ok((name, Box::new(value)))
                    })
                    .collect::<Result<Vec<_>>>()?,
            ),
            lc,
            src: src.to_string(),
        }),
        kw @ ("defun" | "defpurefun") => {
            let mut decl = tokens
                .next()
                .ok_or(anyhow!("expected function declaration"))??
                .as_list()
                .with_context(|| anyhow!("invalid function declaration"))?
                .to_vec()
                .into_iter();

            let name = decl
                .next()
                .with_context(|| anyhow!("missing function name"))?
                .as_symbol()
                .with_context(|| anyhow!("invalid function name"))?
                .to_owned();

            let args = decl
                .map(|a| {
                    a.as_symbol()
                        .with_context(|| anyhow!("invalid function argument"))
                        .map(|x| x.to_owned())
                })
                .collect::<Result<Vec<_>>>()?;

            let body = Box::new(
                tokens
                    .next()
                    .with_context(|| anyhow!("missing function body"))??,
            );
            Ok(AstNode {
                class: if kw == "defun" {
                    Token::Defun { name, args, body }
                } else {
                    Token::Defpurefun { name, args, body }
                },
                src: src.into(),
                lc,
            })
        }
        "defalias" => {
            let mut defs = vec![];
            while let Some(from) = tokens.next() {
                let from = from?.as_symbol()?.to_owned();
                let to = tokens
                    .next()
                    .with_context(|| anyhow!("missing alias target"))??
                    .as_symbol()?
                    .to_owned();
                defs.push(AstNode {
                    class: Token::DefAlias(from, to),
                    src: src.to_string(),
                    lc,
                });
            }

            Ok(AstNode {
                class: Token::DefAliases(defs),
                src: src.into(),
                lc,
            })
        }
        "defunalias" => {
            let from = tokens
                .next()
                .with_context(|| anyhow!("missing function alias source"))??
                .as_symbol()?
                .to_owned();

            let to = tokens
                .next()
                .with_context(|| anyhow!("missing function alias target"))??
                .as_symbol()?
                .to_owned();

            Ok(AstNode {
                class: Token::DefunAlias(from, to),
                src: src.into(),
                lc,
            })
        }
        "defconstraint" => parse_defconstraint(tokens, lc, src),
        "definrange" => {
            let exp = tokens
                .next()
                .with_context(|| anyhow!("expected expression"))??;

            let range = tokens
                .next()
                .with_context(|| anyhow!("missing maximal value"))??
                .as_u64()?;

            Ok(AstNode {
                class: Token::DefInrange(Box::new(exp), range as usize),
                src: src.into(),
                lc,
            })
        }
        "defplookup" => {
            let name = tokens
                .next()
                .with_context(|| anyhow!("expected plookup name"))??
                .as_symbol()?
                .to_owned();

            let including = tokens
                .next()
                .with_context(|| anyhow!("missing including columns"))??
                .as_list()?
                .to_vec();

            let included = tokens
                .next()
                .with_context(|| anyhow!("missing included columns"))??
                .as_list()?
                .to_vec();

            Ok(AstNode {
                class: Token::DefPlookup {
                    name,
                    including,
                    included,
                },
                src: src.into(),
                lc,
            })
        }
        "defpermutation" => {
            let to = tokens
                .next()
                .with_context(|| anyhow!("missing target columns"))??
                .as_list()?
                .iter()
                .map(|t| t.as_symbol().map(|x| x.to_owned()))
                .collect::<Result<Vec<_>>>()?;

            let from = tokens
                .next()
                .with_context(|| anyhow!("missing source columns"))??
                .as_list()?
                .iter()
                .map(|t| t.as_symbol().map(|x| x.to_owned()))
                .collect::<Result<Vec<_>>>()?;

            Ok(AstNode {
                class: Token::DefPermutation { from, to },
                src: src.into(),
                lc,
            })
        }
        x => unimplemented!("{:?}", x),
    }
}

#[cfg(feature = "interactive")]
fn rec_parse(pair: Pair<Rule>) -> Result<AstNode> {
    let lc = pair.as_span().start_pos().line_col();
    let src = pair.as_str().to_owned();

    match pair.as_rule() {
        Rule::expr => rec_parse(pair.into_inner().next().unwrap()),
        Rule::definition => parse_definition(pair).with_context(|| make_src_error(&src, lc)),
        Rule::sexpr => {
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
                .and_then(|x| x.parse::<isize>().ok());
            let x2 = pairs
                .next()
                .map(|x| x.as_str())
                .and_then(|x| x.parse::<isize>().ok());
            let x3 = pairs
                .next()
                .map(|x| x.as_str())
                .and_then(|x| x.parse::<isize>().ok());
            let range = match (x1, x2, x3) {
                (Some(start), None, None) => (1..=start).collect(),
                (Some(start), Some(stop), None) => (start..=stop).collect(),
                (Some(start), Some(stop), Some(step)) => {
                    (start..=stop).step_by(step.try_into()?).collect()
                }
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
                    .map(|x| x.as_str().parse::<isize>().unwrap())
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
