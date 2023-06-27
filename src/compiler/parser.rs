use crate::{errors, pretty::Base};
use anyhow::{anyhow, bail, Context, Result};
use itertools::Itertools;
use num_bigint::BigInt;
#[cfg(feature = "parser")]
use pest::{iterators::Pair, Parser};
use serde::{Deserialize, Serialize};
use std::fmt;
use std::fmt::Debug;

use super::{ColumnRef, Magma, Type};

#[cfg(feature = "parser")]
#[derive(Parser)]
#[grammar = "corset.pest"]
struct CorsetParser;

#[derive(Debug)]
pub struct Ast {
    pub exprs: Vec<AstNode>,
}

type LinCol = (usize, usize);
#[derive(Clone)]
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
    pub fn as_i64(&self) -> Result<i64> {
        if let Token::Value(r) = &self.class {
            r.try_into().map_err(|e| anyhow!("{:?}", e))
        } else {
            bail!("expected i64, found `{:?}`", self)
        }
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
    /// A formatting function optimizing for debug informations
    pub fn debug_info(&self) -> Option<String> {
        self.class.debug_info()
    }
}
impl Debug for AstNode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        Debug::fmt(&self.class, f)
    }
}
impl std::fmt::Display for AstNode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        Debug::fmt(&self.class, f)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum Kind<T> {
    /// an atomic column is directly filled from traces and has a padding value
    Atomic,
    /// a phantom column is present, but will be filled later on
    Phantom,
    /// a composite column is similar to a phantom column, but the expression
    /// computing it is known
    Composite(Box<T>),
    Interleaved(Vec<T>, Option<Vec<ColumnRef>>),
}
impl<T> Kind<T> {
    pub fn to_nil(&self) -> Kind<()> {
        match self {
            Kind::Atomic => Kind::Atomic,
            Kind::Phantom => Kind::Phantom,
            Kind::Composite(_) => Kind::Composite(Box::new(())),
            Kind::Interleaved(_, xs) => Kind::Interleaved(vec![], xs.clone()),
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

#[derive(Clone)]
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

    /// definition of a module; this will derive a symbol table
    DefModule(String),
    /// a list of constant definition: (name, value)
    DefConsts(Vec<(String, Box<AstNode>)>),
    /// a list of columns declaration, normally only DefColumn
    DefColumns(Vec<AstNode>),
    /// a list of columns declaration, normally only DefColumn, only enabled when trigger is non-zero
    DefPerspective {
        name: String,
        trigger: Box<AstNode>,
        columns: Vec<AstNode>,
    },
    /// defines an atomic column
    DefColumn {
        /// name of the column; unique in its module
        name: String,
        /// type of the column
        t: Type,
        /// how the values of the column are filled
        kind: Kind<AstNode>,
        /// the value to pad the column with; defaults to 0 if None
        padding_value: Option<i64>,
        /// which numeric base should be used to display column values; this is a purely aesthetic setting
        base: Base,
    },
    /// defines an array
    DefArrayColumn {
        name: String,
        domain: Vec<usize>,
        t: Type,
        /// which numeric base should be used to display column values; this is a purely aesthetic setting
        base: Base,
    },
    /// definition of a function
    Defun {
        /// name of the function; must be unique in its module
        name: String,
        /// the arguments are free strings, that will be resolved at evaluation
        args: Vec<String>,
        /// the magmas of the arguments
        in_types: Vec<Type>,
        /// the output magma
        out_type: Option<Type>,
        /// the body is any reasonable expression (should it be enforced?)
        body: Box<AstNode>,
        /// if set, do not warn on type override
        nowarn: bool,
    },
    Defpurefun {
        name: String,
        args: Vec<String>,
        in_types: Vec<Type>,
        out_type: Option<Type>,
        body: Box<AstNode>,
        nowarn: bool,
    },
    /// a list of aliases declaration, normally only DefAlias -- FIXME: should probably be removed
    DefAliases(Vec<AstNode>),
    DefAlias(String, String),
    /// Declaration of a function alias -- FIXME: should probably be removed
    DefunAlias(String, String),

    /// Declaration of a constraint;
    DefConstraint {
        /// the given name of the constraint -- TODO enforce uniqueness
        name: String,
        /// if the domain of the constraint is `None`, it is supposed to hold everywhere
        domain: Option<Vec<isize>>,
        /// an expression that enables the constraint only when it is non zero
        guard: Option<Box<AstNode>>,
        /// if the constraint is set in a perspective, it is automatically
        /// guarded and additional rules are applied to symbol resolution
        perspective: Option<String>,
        /// this expression has to reduce to 0 for the constraint to be satisfied
        body: Box<AstNode>,
    },
    /// declaration of a permutation constraint between two sets of columns
    DefPermutation {
        from: Vec<String>,
        to: Vec<String>,
        signs: Vec<bool>,
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
const LIST_DISPLAY_THRESHOLD: usize = 4;
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

    pub fn debug_info(&self) -> Option<String> {
        match self {
            Token::Value(x) => Some(format!("{}", x)),
            Token::Symbol(ref name) => Some(format!("{}", name)),
            Token::Keyword(ref name) => Some(format!("{}", name)),
            Token::List(ref args) => {
                if let Some(verb) = args.get(0) {
                    if let Ok(verb) = verb.as_symbol() {
                        match verb {
                            "if-zero" | "if-not-zero" => {
                                Some(format!("({})", Token::format_list_debug(args, 2)))
                            }
                            "if-eq" | "if-eq-else" => {
                                Some(format!("({})", Token::format_list_debug(args, 3)))
                            }
                            _ => Some(format!(
                                "({})",
                                Token::format_list_debug(args, LIST_DISPLAY_THRESHOLD)
                            )),
                        }
                    } else {
                        Some(format!(
                            "({})",
                            Token::format_list_debug(args, LIST_DISPLAY_THRESHOLD)
                        ))
                    }
                } else {
                    Some(format!(
                        "({})",
                        Token::format_list_debug(args, LIST_DISPLAY_THRESHOLD)
                    ))
                }
            }
            Token::Range(ref args) => Some(format!("{:?}", args)),
            _ => None,
        }
    }

    fn format_list(cs: &[AstNode], list_cut: usize) -> String {
        if cs.len() <= list_cut {
            cs.iter()
                .map(|c| format!("{:?}", c))
                .collect::<Vec<_>>()
                .join(" ")
        } else {
            cs.iter()
                .take(list_cut)
                .map(|c| format!("{:?}", c))
                .collect::<Vec<_>>()
                .join(" ")
                + " [...]"
        }
    }

    fn format_list_debug(cs: &[AstNode], list_cut: usize) -> String {
        if cs.len() <= list_cut {
            cs.iter()
                .filter_map(|c| c.debug_info())
                .collect::<Vec<_>>()
                .join(" ")
        } else {
            cs.iter()
                .take(list_cut)
                .filter_map(|c| c.debug_info())
                .collect::<Vec<_>>()
                .join(" ")
                + " [...]"
        }
    }
}
impl Debug for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::Value(x) => write!(f, "{}", x),
            Token::Symbol(ref name) => write!(f, "{}", name),
            Token::Keyword(ref name) => write!(f, "{}", name),
            Token::List(ref args) => {
                write!(f, "({})", Token::format_list(args, LIST_DISPLAY_THRESHOLD))
            }
            Token::Range(ref args) => write!(f, "{:?}", args),

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
            Token::DefColumn { name, t, kind, .. } => {
                write!(f, "DECLARATION {}:{:?}{:?}", name, t, kind)
            }
            Token::DefPermutation { from, to, .. } => {
                write!(f, "({:?}):PERMUTATION({:?})", to, from)
            }
            Token::DefInrange(exp, max) => write!(f, "{:?}E{}", exp, max),
            Token::DefArrayColumn {
                name,
                domain: range,
                t,
                ..
            } => {
                write!(f, "DECLARATION {}{:?}{{{:?}}}", name, range, t)
            }
            Token::DefConstraint { name, .. } => write!(f, "{:?}:CONSTRAINT", name),
            Token::Defun {
                name,
                args,
                body: content,
                ..
            } => {
                write!(f, "{}:({:?}) -> {:?}", name, args, content)
            }
            Token::Defpurefun {
                name, args, body, ..
            } => {
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
            Token::DefPerspective {
                name,
                trigger,
                columns,
            } => write!(f, "SET {}/{:?} {:?}", name, trigger, columns),
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
        Perspective,
    }

    let name = tokens
        .next()
        .with_context(|| anyhow!("missing constraint name"))??
        .as_symbol()?
        .to_owned();

    let (domain, guard, perspective) = {
        let guards = tokens
            .next()
            .with_context(|| anyhow!("missing guards in constraint definitions"))??
            .as_list()?
            .to_vec();
        let mut status = GuardParser::Begin;
        let mut domain = None;
        let mut guard = None;
        let mut perspective = None;
        for x in guards.iter() {
            match status {
                GuardParser::Begin => match x.class {
                    Token::Keyword(ref kw) if kw == ":guard" => status = GuardParser::Guard,
                    Token::Keyword(ref kw) if kw == ":domain" => status = GuardParser::Domain,
                    Token::Keyword(ref kw) if kw == ":perspective" => {
                        status = GuardParser::Perspective
                    }
                    _ => bail!("expected :guard, :domain or :perspective, found `{:?}`", x),
                },
                GuardParser::Guard => {
                    if guard.is_some() {
                        bail!("guard already defined: `{:?}`", guard.unwrap())
                    } else {
                        guard = Some(Box::new(x.clone()));
                        status = GuardParser::Begin;
                    }
                }
                GuardParser::Perspective => {
                    if guard.is_some() {
                        bail!("perspective already defined: `{:?}`", guard.unwrap())
                    } else {
                        perspective = Some(x.as_symbol()?.to_owned());
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
            GuardParser::Perspective => bail!("expected perspective name, found nothing"),
        }

        (domain, guard, perspective)
    };

    let body = Box::new(
        tokens
            .next()
            .with_context(|| anyhow!("missing constraint name"))??,
    );

    if let Some(last) = tokens.next() {
        bail!("too many arguments found for DEFCONSTRAINT: {}", last?.src)
    }

    Ok(AstNode {
        class: Token::DefConstraint {
            name,
            domain,
            guard,
            perspective,
            body,
        },
        src,
        lc,
    })
}

fn parse_defperspective<I: Iterator<Item = Result<AstNode>>>(mut tokens: I) -> Result<AstNode> {
    let name = tokens
        .next()
        .with_context(|| anyhow!("missing perspective name"))??
        .as_symbol()
        .with_context(|| "expected perspective name")?
        .to_owned();

    let trigger = Box::new(
        tokens
            .next()
            .with_context(|| anyhow!("expected perspective trigger"))??,
    );

    let columns_tokens = tokens
        .next()
        .with_context(|| anyhow!("missing columns declaration in perspective"))??;
    let lc = columns_tokens.lc;
    let src = columns_tokens.src.to_owned();
    let columns = columns_tokens.as_list()?.iter().cloned().map(Ok);
    let columns = parse_defcolumns(columns, lc, src.to_owned())?;
    if let Token::DefColumns(columns) = columns.class {
        Ok(AstNode {
            class: Token::DefPerspective {
                name,
                trigger,
                columns,
            },
            src,
            lc,
        })
    } else {
        unreachable!()
    }
}

fn parse_defcolumns<I: Iterator<Item = Result<AstNode>>>(
    tokens: I,
    lc: (usize, usize),
    src: String,
) -> Result<AstNode> {
    enum ColumnParser {
        Begin,
        Array,
        Computation,
        PaddingValue,
        Base,
    }
    // A columns definition is a list of column definition
    let columns = tokens
        .map(|c| {
            c.and_then(|c| {
                let name;
                let mut t = None;
                let mut range = None;
                let mut padding_value = None;
                let mut base = Base::Dec;

                let mut state = ColumnParser::Begin;
                // A column is either defined by...
                match c.class {
                    // ...a name, in which case the column is an atomic fr column...
                    Token::Symbol(_name) => name = _name,
                    // ...or a list, specifying attributes for this column.
                    Token::List(xs) => {
                        let mut xs = xs.iter();
                        let name_token = xs
                            .next()
                            .ok_or_else(|| anyhow!("expected column name, found empty list"))?;
                        // The first element of the llist *has* to be the name of the column
                        if let Token::Symbol(ref _name) = name_token.class {
                            name = _name.to_owned();
                        } else {
                            bail!("expected column name, found `{:?}`", name_token)
                        }
                        // Then can come all the attributes, in no particular order.
                        for x in xs {
                            state = match state {
                                ColumnParser::Begin => match x.class {
                                    Token::Keyword(ref kw) => {
                                        // e.g. (A ... :integer ...)
                                        match kw.to_lowercase().as_str() {
                                            ":boolean" | ":bool" | ":nibble" | ":byte"
                                                | ":integer" | ":natural" => {
                                                    if t.is_some() {
                                                        bail!(
                                                            "trying to redefine column {} of type {:?} as {}",
                                                            name, t.unwrap(), kw
                                                        )
                                                    } else {
                                                        t = Some(kw.as_str().try_into()?);
                                                    }
                                                    ColumnParser::Begin
                                                }
                                            // not really used for now.
                                            ":comp" => ColumnParser::Computation,
                                            // e.g. (A :array {1 3 5}) or (A :array [5])
                                            ":array" => ColumnParser::Array,
                                            // a specific padding value, e.g. (NOT :padding 255)
                                            ":padding" => ColumnParser::PaddingValue,
                                            // how to display the column values in debug
                                            ":display" => ColumnParser::Base,
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
                                ColumnParser::PaddingValue => {
                                    padding_value = Some(x.as_i64()?);
                                    ColumnParser::Begin
                                },
                                ColumnParser::Base => {
                                    base = if let Token::Keyword(ref kw) = x.class {
                                        kw.as_str().try_into()?
                                    } else {
                                        bail!("missing argument to :display")
                                    };
                                    ColumnParser::Begin
                                },
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
                                Token::DefArrayColumn {
                                    name,
                                    t: Type::ArrayColumn(t.unwrap_or(Magma::Integer)),
                                    domain: range
                                        .iter()
                                        .map(|&x| x.try_into().map_err(|e| anyhow!("{:?}", e)))
                                        .collect::<Result<Vec<_>>>()?,
                                    base,
                                }
                            } else {
                                Token::DefColumn {
                                    name,
                                    t: Type::Column(t.unwrap_or(Magma::Integer)),
                                    kind: Kind::Atomic,
                                    padding_value,
                                    base
                                }
                            },
                            lc: c.lc,
                            src: c.src.clone(),
                        }),
                    ColumnParser::Array => bail!("incomplete :array definition"),
                    ColumnParser::Computation => bail!("incomplate :comp definition"),
                    ColumnParser::PaddingValue => bail!("incomplete :padding definition"),
                    ColumnParser::Base => bail!("incomplete :display definition"),
                }
            })
        })
        .collect::<Result<Vec<_>>>()
        .with_context(|| errors::parser::make_src_error(&src, lc))?;

    Ok(AstNode {
        class: Token::DefColumns(columns),
        lc,
        src,
    })
}

#[cfg(feature = "parser")]
fn parse_definition(pair: Pair<Rule>) -> Result<AstNode> {
    use owo_colors::OwoColorize;

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
        "defperspective" => parse_defperspective(tokens),
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
            src,
        }),
        kw @ ("defun" | "defpurefun") => {
            fn parse_typed_symbols(l: AstNode) -> Result<(String, Option<Type>, bool)> {
                // TODO: revamp type parsing to add column/scalar/any
                match l.class {
                    Token::Symbol(s) => Ok((s, None, false)),
                    Token::List(xs) => match xs.as_slice() {
                        [AstNode {
                            class: Token::Symbol(s),
                            ..
                        }, AstNode {
                            class: Token::Keyword(t),
                            ..
                        }] => Ok((
                            s.to_owned(),
                            Some(Type::Any(Magma::try_from(t.as_str())?)),
                            false,
                        )),
                        [AstNode {
                            class: Token::Symbol(s),
                            ..
                        }, AstNode {
                            class: Token::Keyword(t),
                            ..
                        }, AstNode {
                            class: Token::Keyword(n),
                            ..
                        }] => {
                            if n == ":nowarn" {
                                Ok((
                                    s.to_owned(),
                                    Some(Type::Any(Magma::try_from(t.as_str())?)),
                                    true,
                                ))
                            } else {
                                bail!("SCREW YOU {}", n)
                            }
                        }
                        _ => Err(anyhow!(
                            "invalid argument format: expected SYMBOL or (SYMBOL :TYPE)"
                        )),
                    },
                    _ => Err(anyhow!("invalid function argument")),
                }
            }

            let mut decl = tokens
                .next()
                .ok_or_else(|| anyhow!("expected function declaration"))??
                .as_list()
                .with_context(|| anyhow!("invalid function declaration"))?
                .to_vec()
                .into_iter();

            let (name, out_type, nowarn) = parse_typed_symbols(
                decl.next()
                    .with_context(|| anyhow!("missing function name"))?,
            )
            .with_context(|| anyhow!("invalid function declaration"))?;

            let (args, in_types): (Vec<String>, Vec<Type>) = decl
                .map(parse_typed_symbols)
                .collect::<Result<Vec<_>>>()?
                .into_iter()
                // if an argument type is unspecified, it can be of any type
                .map(|x| (x.0, x.1.unwrap_or(Type::Any(Magma::Any))))
                .unzip();

            let body = Box::new(
                tokens
                    .next()
                    .with_context(|| anyhow!("missing function body"))??,
            );

            if let Some(last) = tokens.next() {
                bail!("too many arguments found for DEFUN: {}", last?.src)
            }

            Ok(AstNode {
                class: if kw == "defun" {
                    Token::Defun {
                        name,
                        args,
                        in_types,
                        out_type,
                        body,
                        nowarn,
                    }
                } else {
                    Token::Defpurefun {
                        name,
                        args,
                        in_types,
                        out_type,
                        body,
                        nowarn,
                    }
                },
                src,
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
                src,
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
                src,
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
                src,
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
                src,
                lc,
            })
        }
        "defpermutation" => {
            enum SignParser {
                Running,
                Done,
            }

            let to = tokens
                .next()
                .with_context(|| anyhow!("missing source columns"))??
                .as_list()?
                .iter()
                .map(|t| t.as_symbol().map(|x| x.to_owned()))
                .collect::<Result<Vec<_>>>()?;
            let mut sign_state = SignParser::Running;
            let mut signs = Vec::new();
            let mut from = Vec::new();
            for c in tokens
                .next()
                .with_context(|| anyhow!("missing target columns"))??
                .as_list()?
                .iter()
            {
                match c.class {
                    Token::Symbol(ref x) => {
                        sign_state = SignParser::Done;
                        from.push(x.to_owned());
                    }
                    Token::List(ref xs) => {
                        let mut xs = xs.iter();
                        let sign = xs
                            .next()
                            .with_context(|| anyhow!("missing sorting criterion"))?;
                        let sign = sign
                            .as_symbol()
                            .with_context(|| {
                                anyhow!("expected + or -, found {}", sign.src.bold().white())
                            })
                            .and_then(|x| {
                                if x == "+" || x == "↓" {
                                    Ok(true)
                                } else if x == "-" || x == "↑" {
                                    Ok(false)
                                } else {
                                    Err(anyhow!(
                                        "expected + or -, found {}",
                                        sign.src.bold().white()
                                    ))
                                }
                            })?;

                        let col = xs
                            .next()
                            .with_context(|| anyhow!("missing column to sort"))?;
                        let col = col
                            .as_symbol()
                            .with_context(|| anyhow!("expected column to sort, found {}", col.src))?
                            .to_string();
                        if matches!(sign_state, SignParser::Done) {
                            bail!(
                                "found sorting column {} after non-sorting column",
                                col.white().bold()
                            )
                        }

                        signs.push(sign);
                        from.push(col);
                    }
                    _ => {
                        bail!("expected COLUMN or (SIGN COLUMN), found {}", c)
                    }
                }
            }

            if signs.is_empty() {
                bail!("no sorting criterion found")
            }
            signs.resize(from.len(), true); // ensure that signs & froms are the same size

            Ok(AstNode {
                class: Token::DefPermutation { from, to, signs },
                src,
                lc,
            })
        },
        "definterleaved" => {
            let name_ast = tokens
                .next()
                .with_context(|| anyhow!("missing column name in definterleaved"))??;
            let Ok(name) = name_ast.as_symbol().map(String::from) else {
                bail!("expected column name in definterleaved, found {}", name_ast.src);
            };
            let params_ast = tokens
                .next()
                .with_context(|| anyhow!("definterleaved: missing columns parameters"))??;
            let kind = Kind::Interleaved(
                params_ast.as_list()?.to_vec(),
                None
            );
            Ok(AstNode {
                class: Token::DefColumns(vec![
                    AstNode {
                        class: Token::DefColumn {
                            name,
                            t: Type::Column(Magma::Integer), // maybe deduce it from the parameters?
                            kind,
                            padding_value: None,
                            base: Base::Hex // maybe deduce it from the parameters?
                        },
                        src: src.clone(),
                        lc,
                    }
                ]),
                lc,
                src,
            })
        }
        x => unimplemented!("{:?}", x),
    }
}

#[cfg(feature = "parser")]
fn rec_parse(pair: Pair<Rule>) -> Result<AstNode> {
    use num_traits::{FromPrimitive, Num};

    let lc = pair.as_span().start_pos().line_col();
    let src = pair.as_str().to_owned();

    match pair.as_rule() {
        Rule::expr => rec_parse(pair.into_inner().next().unwrap()),
        Rule::definition => {
            parse_definition(pair).with_context(|| errors::parser::make_src_error(&src, lc))
        }
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
        Rule::keyword => Ok(AstNode {
            class: Token::Keyword(pair.as_str().to_owned()),
            src,
            lc,
        }),
        Rule::nth => {
            let nth_token = AstNode {
                class: Token::Symbol("nth".into()),
                lc,
                src: src.clone(),
            };
            let args = pair
                .into_inner()
                .map(rec_parse)
                .collect::<Result<Vec<_>>>()?;
            Ok(AstNode {
                class: Token::List(vec![nth_token, args[0].clone(), args[1].clone()]),
                lc,
                src,
            })
        }
        x => {
            unimplemented!("{:?}", x)
        }
    }
}

#[cfg(feature = "parser")]
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
