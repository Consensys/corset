use color_eyre::eyre::*;
use pest::{error::Error, iterators::Pair, Parser};
use std::collections::HashMap;

#[derive(Parser)]
#[grammar = "corset.pest"]
pub struct CorsetParser;

lazy_static::lazy_static! {
    static ref BUILTINS: HashMap<&'static str, Builtin> = maplit::hashmap!{
        "defun" => Builtin::Defun,
        "defalias" => Builtin::Defalias,

        "add" => Builtin::Add,
        "mul" => Builtin::Mul,
        "sub" => Builtin::Sub,

        "eq" => Builtin::Equals,
    };
}

pub(crate) trait Transpiler {
    fn render(&self, cs: &ConstraintsSet) -> Result<String>;
}

pub struct ConstraintsSet {
    pub constraints: Vec<AstNode>,
}

struct ParsingAst {
    exprs: Vec<AstNode>,
}
impl ParsingAst {
    fn get_defuns(&self) -> Vec<&[AstNode]> {
        self.exprs
            .iter()
            .filter_map(|e| {
                if let AstNode::Funcall {
                    verb:
                        Verb {
                            status: VerbStatus::Builtin(Builtin::Defun),
                            ..
                        },
                    args,
                } = e
                {
                    Some(args.as_slice())
                } else {
                    None
                }
            })
            .collect::<Vec<_>>()
    }

    fn get_aliases(&self) -> Vec<&AstNode> {
        self.exprs
            .iter()
            .filter(|e| {
                matches!(
                    e,
                    AstNode::Funcall {
                        verb: Verb {
                            status: VerbStatus::Builtin(Builtin::Defalias),
                            ..
                        },
                        ..
                    }
                )
            })
            .collect::<Vec<_>>()
    }
}

impl ConstraintsSet {
    pub fn from_str<S: AsRef<str>>(s: S) -> Result<Self> {
        let exprs = parse(s.as_ref())?;
        Compiler::compile(exprs)
    }

    pub fn from_file<S: AsRef<str>>(filename: S) -> Result<Self> {
        let file_content = std::fs::read_to_string(filename.as_ref())?;
        Self::from_str(&file_content)
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Builtin {
    Defun,
    Defalias,

    Add,
    Sub,
    Mul,

    Equals,
}
impl Builtin {}

#[derive(Debug, PartialEq, Clone)]
pub enum VerbStatus {
    Builtin(Builtin),
    Ready(Box<AstNode>),
    Pending,
}
#[derive(Debug, PartialEq, Clone)]
pub struct Verb {
    name: String,
    status: VerbStatus,
}

#[derive(Clone, Debug, PartialEq)]
pub enum SymbolStatus {
    Pending,
    Resolved,
    Functional,
}

#[derive(Debug, PartialEq, Clone)]
pub enum AstNode {
    Ignore,
    Value(i32),
    Symbol { name: String, status: SymbolStatus },
    Funcall { verb: Verb, args: Vec<AstNode> },
}
impl AstNode {
    fn fold<T>(&self, f: &dyn Fn(T, &Self) -> T, ax: T) -> T {
        match self {
            AstNode::Ignore => ax,
            AstNode::Symbol { .. } | AstNode::Value(_) => f(ax, self),
            AstNode::Funcall { args, .. } => {
                let mut aax = ax;
                for s in args.iter() {
                    aax = s.fold(f, aax);
                }
                aax
            }
        }
    }
}

fn build_ast_from_expr(pair: Pair<Rule>, in_def: bool) -> Result<AstNode> {
    // println!("parsing {:?}", pair.as_rule());
    match pair.as_rule() {
        Rule::expr | Rule::constraint => {
            build_ast_from_expr(pair.into_inner().next().unwrap(), in_def)
        }
        Rule::sexpr => {
            let mut content = pair.into_inner();
            let verb_name = content.next().unwrap().as_str();
            let verb = BUILTINS
                .get(verb_name)
                .map(|b| Verb {
                    name: verb_name.into(),
                    status: VerbStatus::Builtin(*b),
                })
                .unwrap_or(Verb {
                    name: verb_name.into(),
                    status: VerbStatus::Pending,
                });
            let in_def = in_def
                || verb.status == VerbStatus::Builtin(Builtin::Defun)
                || verb.status == VerbStatus::Builtin(Builtin::Defalias);
            let args = content
                .map(|p| build_ast_from_expr(p, in_def))
                .collect::<Result<Vec<_>>>()?
                .into_iter()
                .filter(|x| *x != AstNode::Ignore)
                .collect();
            Ok(AstNode::Funcall { verb, args })
        }
        Rule::column => Ok(AstNode::Symbol {
            name: pair.as_str().into(),
            status: if in_def {
                SymbolStatus::Functional
            } else {
                SymbolStatus::Resolved
            },
        }),
        Rule::value => Ok(AstNode::Value(pair.as_str().parse().unwrap())),
        // Rule::function => {}
        x @ _ => {
            dbg!(&x);
            Ok(AstNode::Ignore)
        }
    }
}

fn parse(source: &str) -> Result<ParsingAst> {
    let mut ast = ParsingAst { exprs: vec![] };

    for pair in CorsetParser::parse(Rule::corset, source)? {
        match &pair.as_rule() {
            Rule::corset => {
                for constraint in pair.into_inner() {
                    if constraint.as_rule() != Rule::EOI {
                        ast.exprs.push(build_ast_from_expr(constraint, false)?);
                    }
                }
            }
            _ => {}
        }
    }

    Ok(ast)
}

#[derive(Debug)]
struct SymbolsTable {
    funcs: HashMap<String, Function>,
    symbols: HashMap<String, String>,
}
impl SymbolsTable {
    pub fn new() -> SymbolsTable {
        SymbolsTable {
            funcs: HashMap::new(),
            symbols: HashMap::new(),
        }
    }
}

#[derive(Debug)]
struct Function {
    name: String,
    args: Vec<String>,
    body: AstNode,
}
struct Compiler {
    table: SymbolsTable,
    ast: ParsingAst,
}
impl Compiler {
    fn register_columns(&mut self) -> Result<()> {
        let mut columns = vec![];
        for n in self.ast.exprs.iter() {
            let _ = n.fold(
                &|ax: &mut Vec<String>, n| {
                    if let AstNode::Symbol {
                        name,
                        status: SymbolStatus::Resolved,
                    } = n
                    {
                        ax.push(name.clone());
                    }
                    ax
                },
                &mut columns,
            );
        }
        columns.into_iter().for_each(|s| {
            self.table.symbols.insert(s.clone(), s.clone());
        });

        Ok(())
    }
    fn compile_funcs(&mut self) -> Result<()> {
        fn parse_header(header: &AstNode) -> Result<(String, Vec<String>)> {
            if let AstNode::Funcall {
                verb:
                    Verb {
                        name: n,
                        status: VerbStatus::Pending,
                    },
                args,
            } = header
            {
                let fname = n.into();
                let arg_names = args
                    .iter()
                    .map(|a| {
                        if let AstNode::Symbol {
                            status: SymbolStatus::Functional,
                            name: n,
                        } = a
                        {
                            Ok(n.to_owned())
                        } else {
                            Err(eyre!("{:?} is not a valid argument", a))
                        }
                    })
                    .collect::<Result<Vec<_>>>()
                    .with_context(|| format!("while parsing function {}", fname))?;
                Ok((fname, arg_names))
            } else {
                bail!("SSS")
            }
        }

        let defuns = self.ast.get_defuns();

        for defun in defuns.iter() {
            if defun.len() != 2 {
                bail!("Invalid DEFUN found")
            }
            let header = &defun[0];
            let body = &defun[1];
            let (name, args) = parse_header(header)?;
            body.fold(
                &|ax: Result<()>, n| {
                    if let AstNode::Symbol { name, .. } = n {
                        ax.and_then(|_| {
                            args.contains(&name)
                                .then(|| ())
                                .ok_or(eyre!("symbol `{}` unknown", name))
                        })
                    } else {
                        ax
                    }
                },
                Ok(()),
            )
            .with_context(|| format!("while parsing function `{}`", name))?;
            self.table.funcs.insert(
                name.to_owned(),
                Function {
                    name,
                    args,
                    body: body.to_owned(),
                },
            );
        }

        // dbg!(defuns);
        Ok(())
    }

    fn compile_aliases(&mut self) -> Result<()> {
        let defaliases = self.ast.get_aliases();
        for defalias in defaliases.iter() {
            dbg!(defalias);
            if let AstNode::Funcall { args, .. } = defalias {
                if args.len() != 2 {
                    return Err(eyre!(
                        "`defalias`: two arguments expected, {} found",
                        args.len()
                    ));
                }

                if let AstNode::Symbol {
                    name: from,
                    status: SymbolStatus::Functional,
                } = &args[0]
                {
                    if let AstNode::Symbol {
                        name: to,
                        status: SymbolStatus::Functional,
                    } = &args[1]
                    {
                        self.table.symbols.insert(from.into(), to.into());
                    } else {
                        return Err(eyre!("Invalid argument found in DEFALIAS: {:?}", args[1]));
                    }
                } else {
                    return Err(eyre!("Invalid argument found in DEFALIAS: {:?}", args[0]));
                }
            };
        }

        Ok(())
    }

    fn compile(ast: ParsingAst) -> Result<ConstraintsSet> {
        let mut compiler = Compiler {
            table: SymbolsTable::new(),
            ast,
        };
        compiler.register_columns()?;
        compiler.compile_funcs()?;
        compiler.compile_aliases()?;

        dbg!(&compiler.table);

        Err(eyre!("XXX"))
    }
}
