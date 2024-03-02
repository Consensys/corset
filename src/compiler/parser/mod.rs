use anyhow::*;
use log::*;
use num_bigint::BigInt;
use owo_colors::OwoColorize;

use self::parser::DisplayableColumn;

use crate::{
    compiler::{tables::Scope, Type},
    errors::symbols,
    pretty::Base,
};

use super::{CompileSettings, Domain, Kind};

mod constants;
mod definitions;
mod fmtparser;
pub(crate) mod parser;
mod purefuns;

#[derive(Debug)]
pub struct Ast {
    pub exprs: Vec<AstNode>,
}

pub type LinCol = (usize, usize);
#[derive(Clone)]
pub struct AstNode {
    /// the token in which this node devolves
    pub class: Token,
    /// the piece of code that produced this node
    pub src: String,
    /// position in the source file of the code of this node
    pub lc: LinCol,
}

/// An AstNode stores a machine-understandable representation of a constraint system, or part of it.
/// At this point, no semantic verification (symbol existence, type checking, etc.) has been performed yet.
impl AstNode {
    /// Returns the maximal nesting within this node
    pub fn depth(&self) -> usize {
        self.class.depth()
    }
    /// If possible, returns the i64 encoded by this node
    pub fn as_i64(&self) -> Result<i64, symbols::Error> {
        if let Token::Value(r) = &self.class {
            r.try_into()
                .map_err(|_| symbols::Error::InvalidConversion("i64", format!("{:?}", self)))
        } else {
            Err(symbols::Error::NotASomethings("i64", format!("{:?}", self)))
        }
    }
    /// If possible, returns the u64 encoded by this node
    pub fn as_u64(&self) -> Result<u64, symbols::Error> {
        if let Token::Value(r) = &self.class {
            r.try_into()
                .map_err(|_| symbols::Error::InvalidConversion("u64", format!("{:?}", self)))
        } else {
            Err(symbols::Error::NotASomethings(
                "usize",
                format!("{:?}", self),
            ))
        }
    }
    /// If possible, returns the symbol encoded by this node
    pub fn as_symbol(&self) -> Result<&str, symbols::Error> {
        if let Token::Symbol(x) = &self.class {
            Result::Ok(x)
        } else {
            Err(symbols::Error::NotASomethings(
                "symbol",
                format!("{:?}", self),
            ))
        }
    }
    /// If possible, returns the list of nodes encoded by this node
    pub fn as_list(&self) -> Result<&[AstNode], symbols::Error> {
        if let Token::List(xs) = &self.class {
            Result::Ok(xs)
        } else {
            Err(symbols::Error::NotASomethings(
                "list",
                format!("{:?}", self),
            ))
        }
    }
    /// If possible, returns the domain/range encoded by this node
    pub fn as_domain(&self) -> Result<Domain<AstNode>, symbols::Error> {
        if let Token::Domain(d) = &self.class {
            Result::Ok(*d.clone())
        } else {
            Err(symbols::Error::NotASomethings(
                "range",
                format!("{:?}", self),
            ))
        }
    }
    /// A formatting function optimizing for debug informations
    pub fn debug_info(&self) -> Option<String> {
        self.class.debug_info()
    }

    pub fn is_symbol(&self) -> bool {
        matches!(self.class, Token::Symbol(_))
    }
    pub fn is_comment(&self) -> bool {
        matches!(self.class, Token::BlockComment(_) | Token::InlineComment(_))
    }
    pub fn is_block_comment(&self) -> bool {
        matches!(self.class, Token::BlockComment(_))
    }
    pub fn is_inline_comment(&self) -> bool {
        matches!(self.class, Token::InlineComment(_))
    }
}
impl std::fmt::Debug for AstNode {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        std::fmt::Debug::fmt(&self.class, f)
    }
}
impl std::fmt::Display for AstNode {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        std::fmt::Debug::fmt(&self.class, f)
    }
}

#[derive(Clone)]
pub enum Token {
    /// an immediate value; can be “arbitrarily” large
    Value(BigInt),
    /// a symbol referencing another element of the tree
    Symbol(String),
    /// a block comment; only used by the formatting parser
    BlockComment(String),
    /// an end-of-line comment; only used by the formatting parser
    InlineComment(String),
    /// an indexed array, generated from the syntax `[symbol index]`
    IndexedSymbol {
        name: String,
        index: Box<AstNode>,
    },
    /// a keyword
    Keyword(String),
    /// a list of nodes
    List(Vec<AstNode>),
    /// a range; typically used in discrete constraints declaration and loops
    /// as it may contain any static expression, it is numerically converted
    /// from Domain<AstNode> to Domain<isize> after the parsing.
    Domain(Box<Domain<AstNode>>),

    /// definition of a module; this will derive a symbol table
    DefModule(String),
    /// a list of constant definition: (name, value)
    DefConsts(Vec<(String, Box<AstNode>)>),
    /// a list of columns declaration, normally only DefColumn
    DefColumns(Vec<AstNode>),
    /// a list of columns declaration, normally only DefColumn, only enabled
    /// when the trigger is non-zero
    DefPerspective {
        name: String,
        trigger: Box<AstNode>,
        columns: Vec<AstNode>,
    },
    /// defines a column
    DefColumn {
        /// name of the column; unique in its module
        name: String,
        /// type of the column
        t: Type,
        /// how the values of the column are filled
        kind: Kind<Box<AstNode>>,
        /// the value to pad the column with; defaults to 0 if None
        padding_value: Option<i64>,
        /// if set, generate constraint to prove the column type
        must_prove: bool,
        /// which numeric base should be used to display column values; this is a purely aesthetic setting
        base: Base,
    },
    /// defines an array
    DefArrayColumn {
        /// name of the array; unique in its module
        name: String,
        /// where is the array defined
        domain: Box<Domain<AstNode>>,
        /// type of the array
        t: Type,
        /// the value to pad the column with; defaults to 0 if None
        padding_value: Option<i64>,
        /// if set, generate constraint to prove the column type
        must_prove: bool,
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
        domain: Option<Box<Domain<AstNode>>>,
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
        from: Vec<AstNode>,
        to: Vec<DisplayableColumn>,
        signs: Vec<bool>,
    },
    DefInterleaving {
        /// new column, which will be filled by the interleaving of the source columns
        target: DisplayableColumn,
        /// the source columns to be interleaved
        froms: Vec<AstNode>, // either Token::Symbol or Token::IndexedSymbol
    },
    /// declaration of a lookup constraint between two sets of columns
    DefLookup {
        name: String,
        including: Vec<AstNode>,
        included: Vec<AstNode>,
    },
    /// this constraint ensures that exp remains lesser than max
    DefInrange(Box<AstNode>, u64),
}
const LIST_DISPLAY_THRESHOLD: usize = 4;
impl Token {
    pub fn depth(&self) -> usize {
        match self {
            Token::List(xs) => {
                (if xs.len() > 1 { 1 } else { 0 }) + xs.iter().map(|x| x.depth()).max().unwrap_or(0)
            }
            _ => 0,
        }
    }

    pub fn debug_info(&self) -> Option<String> {
        match self {
            Token::Value(x) => Some(format!("{}", x)),
            Token::Symbol(ref name) => Some(name.to_string()),
            Token::Keyword(ref name) => Some(name.to_string()),
            Token::List(ref args) => {
                if let Some(verb) = args.get(0) {
                    if let Result::Ok(verb) = verb.as_symbol() {
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
            Token::Domain(ref args) => Some(format!("{:?}", args)),
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
impl std::fmt::Debug for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Token::Value(x) => write!(f, "{}", x),
            Token::Symbol(ref name) => write!(f, "{}", name),
            Token::IndexedSymbol {
                ref name,
                ref index,
            } => write!(f, "[{} {}]", name, index),
            Token::Keyword(ref name) => write!(f, "{}", name),
            Token::List(ref args) => {
                write!(f, "({})", Token::format_list(args, LIST_DISPLAY_THRESHOLD))
            }
            Token::Domain(ref args) => write!(f, "{:?}", args),

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
            Token::DefLookup {
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
            Token::DefInterleaving {
                target,
                froms: sources,
                ..
            } => {
                write!(f, "Interleaving {} by {:?}", target.name, sources)
            }
            Token::BlockComment(s) | Token::InlineComment(s) => write!(f, "{}", s),
        }
    }
}

pub(crate) fn maybe_bail<R>(errs: Vec<Result<R>>) -> Result<Vec<R>> {
    let mut err_count = 0;
    let mut r = vec![];

    for e in errs.into_iter() {
        match e {
            Result::Ok(o) => {
                r.push(o);
            }
            Err(e) => {
                err_count += 1;
                error!("{:?}", e);
            }
        }
    }

    if err_count > 0 {
        bail!("{} errors found", err_count)
    } else {
        Ok(r)
    }
}

/// Given a list of sources and their names, parse them and return a
/// corresponding list of Ast
pub(crate) fn parse_ast<S1: AsRef<str>, S2: AsRef<str>>(
    sources: &[(S1, S2)],
) -> Result<Vec<(String, Ast)>> {
    maybe_bail(
        sources
            .iter()
            .map(|(name, content)| {
                info!("Parsing {}", name.as_ref().bright_white().bold());
                parser::parse(content.as_ref())
                    .with_context(|| anyhow!("parsing `{}`", name.as_ref()))
                    .map(|ast| (name.as_ref().to_string(), ast))
            })
            .collect::<Vec<_>>(),
    )
}

/// Given a list of sources and their names, parse them and return a
/// corresponding list of simplified Ast than can be consumed by the
/// formatter.
/// As only its AstNode-based tree output will ever be used, it does
/// not need a more complex setup like in parse(...)
pub fn parse_simple_ast<S1: AsRef<str>, S2: AsRef<str>>(
    sources: &[(S1, S2)],
) -> Result<Vec<(String, Ast)>> {
    maybe_bail(
        sources
            .iter()
            .map(|(name, content)| {
                info!("Parsing {}", name.as_ref().bright_white().bold());
                fmtparser::parse(content.as_ref())
                    .with_context(|| anyhow!("parsing `{}`", name.as_ref()))
                    .map(|ast| (name.as_ref().to_string(), ast))
            })
            .collect::<Vec<_>>(),
    )
}

pub fn parse<S1: AsRef<str>, S2: AsRef<str>>(
    sources: &[(S1, S2)],
    settings: &CompileSettings,
) -> Result<(Scope, Vec<(String, Ast)>)> {
    let ctx = Scope::new();

    //
    // Parse the source into an AST
    //
    let asts = maybe_bail(
        sources
            .iter()
            .map(|(name, content)| {
                info!("Parsing {}", name.as_ref().bright_white().bold());
                parser::parse(content.as_ref())
                    .with_context(|| anyhow!("parsing `{}`", name.as_ref()))
                    .map(|ast| (name.as_ref().to_string(), ast))
            })
            .collect::<Vec<_>>(),
    )?;

    // The parsing order is crucial to make const. expr. work. Therefore, it
    // must be:
    // 1 - pure functions, which are dependent on constants at run-time but
    //     self-standing at parse-time;
    // 2 - constants, that may be immediate or const. expr., but then pure
    //     functions are already there;
    // 3 - the remaining elements, which may be dependent on everything else.

    // 1. Pure functions
    for (name, ast) in asts.iter() {
        purefuns::pass(&ast, ctx.clone())
            .with_context(|| anyhow!("parsing definitions in `{}`", name))?;
    }
    // 2. Constants
    for (name, ast) in asts.iter() {
        constants::pass(&ast, ctx.clone(), settings)
            .with_context(|| anyhow!("parsing definitions in `{}`", name))?;
    }
    // 3. The rest
    for (name, ast) in asts.iter() {
        definitions::pass(&ast, ctx.clone(), settings)
            .with_context(|| anyhow!("parsing definitions in `{}`", name))?;
    }

    Ok((ctx, asts))
}
