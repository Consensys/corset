use color_eyre::eyre::*;
use pest::{error::Error, iterators::Pair, Parser};
use std::collections::{HashMap, HashSet};

#[derive(Parser)]
#[grammar = "corset.pest"]
struct CorsetParser;

lazy_static::lazy_static! {
    static ref BUILTINS: HashMap<&'static str, Builtin> = maplit::hashmap!{
        "defun" => Builtin::Defun,
        "defalias" => Builtin::Defalias,
        "defunalias" => Builtin::Defalias,
        "defcolumns" => Builtin::Defcolumns,


        // monadic
        "inv" => Builtin::Inv,
        "neg" => Builtin::Neg,
        // polyadic

        "+" => Builtin::Add,
        "add" => Builtin::Add,

        "*" => Builtin::Mul,
        "mul" => Builtin::Mul,
        "and" => Builtin::Mul,

        "-" => Builtin::Sub,
        "sub" => Builtin::Sub,
        "eq" => Builtin::Sub,
        "=" => Builtin::Sub,

        "if-zero" => Builtin::IfZero,
        "if-zero-else" => Builtin::IfZeroElse,

        "begin" => Builtin::Begin,
    };
}

pub(crate) trait Transpiler {
    fn render(&self, cs: &ConstraintsSet) -> Result<String>;
}

#[derive(Debug, Clone)]
pub enum Constraint {
    Funcall {
        func: Builtin,
        args: Vec<Constraint>,
    },
    Const(i32),
    Column(String),
    List(Vec<Constraint>),
}
#[derive(Debug)]
pub struct ConstraintsSet {
    pub constraints: Vec<Constraint>,
}
impl ConstraintsSet {
    pub fn from_str<S: AsRef<str>>(s: S) -> Result<Self> {
        let exprs = parse(s.as_ref())?;
        Compiler::compile(exprs)
    }
}

struct ParsingAst {
    exprs: Vec<AstNode>,
}
impl ParsingAst {
    fn get_defbuiltin(&self, b: Builtin) -> Vec<&[AstNode]> {
        let builtin_name = match b {
            Builtin::Defcolumns => "defcolumns",
            Builtin::Defunalias => "defunalias",
            Builtin::Defalias => "defalias",
            Builtin::Defun => "defun",
            _ => unimplemented!(),
        };
        self.exprs
            .iter()
            .filter_map(|e| {
                if let AstNode::List { args } = e {
                    if let Some(AstNode::Symbol(name)) = args.iter().next() {
                        if name == builtin_name {
                            Some(&args[1..])
                        } else {
                            None
                        }
                    } else {
                        None
                    }
                } else {
                    None
                }
            })
            .collect::<Vec<_>>()
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Builtin {
    Defun,
    Defalias,
    Defunalias,
    Defcolumns,
    Begin,

    Add,
    Sub,
    Mul,
    IfZero,
    IfZeroElse,
    Neg,
    Inv,
}
impl Builtin {}

#[derive(Debug, PartialEq, Clone)]
enum VerbStatus {
    Builtin(Builtin),
    Defined,
}
#[derive(Debug, PartialEq, Clone)]
struct Verb {
    name: String,
    status: VerbStatus,
}

#[derive(Clone, Debug, PartialEq)]
enum SymbolStatus {
    Pending,
    Resolved,
    Functional,
}

#[derive(Debug, PartialEq, Clone)]
enum AstNode {
    Ignore,
    Value(i32),
    Symbol(String),
    List { args: Vec<AstNode> },
    // Funcall { verb: Verb, args: Vec<AstNode> },
}
impl AstNode {
    fn fold<T>(&self, f: &dyn Fn(T, &Self) -> T, ax: T) -> T {
        match self {
            AstNode::Ignore => ax,
            AstNode::Symbol { .. } | AstNode::Value(_) => f(ax, self),
            AstNode::List { args, .. } => {
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
    match pair.as_rule() {
        Rule::expr | Rule::constraint => {
            build_ast_from_expr(pair.into_inner().next().unwrap(), in_def)
        }
        Rule::list => {
            let content = pair.into_inner();
            let args = content
                .map(|p| build_ast_from_expr(p, in_def))
                .collect::<Result<Vec<_>>>()?
                .into_iter()
                .filter(|x| *x != AstNode::Ignore)
                .collect::<Vec<_>>();
            if args.is_empty() {
                Err(eyre!("Empty expression found"))
            } else {
                Ok(AstNode::List { args })
            }
        }
        Rule::symbol => Ok(AstNode::Symbol(pair.as_str().to_owned())),
        Rule::integer => Ok(AstNode::Value(pair.as_str().parse().unwrap())),
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

trait Resolver {
    fn resolve_symbol(&self, name: &str) -> Result<Constraint>;
    fn resolve_function(&self, name: &str) -> Result<Function>;
    fn name(&self) -> String;
}

#[derive(Debug)]
enum Symbol {
    Alias(String),
    Final(Constraint),
}
#[derive(Debug)]
struct SymbolsTable {
    funcs: HashMap<String, Function>,
    symbols: HashMap<String, Symbol>,
}
impl SymbolsTable {
    pub fn new() -> SymbolsTable {
        SymbolsTable {
            funcs: HashMap::new(),
            symbols: HashMap::new(),
        }
    }
    fn insert_symbol(&mut self, symbol: &str) -> Result<()> {
        if self.symbols.contains_key(symbol) {
            Err(anyhow!("`{}` already exists", symbol))
        } else {
            self.symbols.insert(
                symbol.into(),
                Symbol::Final(Constraint::Column(symbol.to_string())),
            );
            Ok(())
        }
    }

    fn insert_alias(&mut self, from: &str, to: &str) -> Result<()> {
        if self.symbols.contains_key(from) {
            Err(anyhow!(
                "`{}` already exists: {} -> {:?}",
                from,
                from,
                self.symbols[from]
            ))
        } else {
            self.symbols.insert(from.into(), Symbol::Alias(to.into()));
            Ok(())
        }
    }

    fn insert_funalias(&mut self, from: &str, to: &str) -> Result<()> {
        if self.symbols.contains_key(from) {
            Err(anyhow!(
                "`{}` already exists: {} -> {:?}",
                from,
                from,
                self.symbols[from]
            ))
        } else {
            self.symbols.insert(from.into(), Symbol::Alias(to.into()));
            Ok(())
        }
    }

    fn _resolve_symbol(&self, name: &str, ax: &mut HashSet<String>) -> Result<Constraint> {
        if ax.contains(name) {
            Err(eyre!("Circular definitions found for {}", name))
        } else {
            ax.insert(name.into());
            match self.symbols.get(name) {
                Some(Symbol::Alias(name)) => self._resolve_symbol(name, ax),
                Some(Symbol::Final(name)) => Ok(name.clone()),
                None => Err(eyre!("Can not find column `{}`", name)),
            }
        }
    }

    fn _resolve_function(&self, name: &str, ax: &mut HashSet<String>) -> Result<Function> {
        if ax.contains(name) {
            Err(eyre!("Circular definitions found for {}", name))
        } else {
            ax.insert(name.into());
            match self.funcs.get(name) {
                Some(Function::Alias(name)) => self._resolve_function(name, ax),
                Some(f @ Function::Defined { .. }) => Ok(f.to_owned()),
                Some(f @ Function::Builtin(..)) => Ok(f.to_owned()),
                None => Err(eyre!("Function `{}` unknown", name)),
            }
        }
    }
}
impl Resolver for SymbolsTable {
    fn resolve_symbol(&self, name: &str) -> Result<Constraint> {
        self._resolve_symbol(name, &mut HashSet::new())
    }

    fn resolve_function(&self, name: &str) -> Result<Function> {
        BUILTINS
            .get(name)
            .map(|b| Function::Builtin(*b))
            .ok_or(eyre!("fubction not found"))
            .or(self._resolve_function(name, &mut HashSet::new()))
    }

    fn name(&self) -> String {
        "Main LU table".into()
    }
}

struct FunctionTable<'a> {
    args_mapping: HashMap<String, Constraint>,
    parent: &'a dyn Resolver,
    name: String,
}
impl<'a> Resolver for FunctionTable<'a> {
    fn resolve_symbol(&self, name: &str) -> Result<Constraint> {
        self.args_mapping
            .get(&name.to_string())
            .map(|s| s.to_owned())
            .ok_or(eyre!("Symbol `{}` not found", name))
            .or_else(|_| self.parent.resolve_symbol(name))
    }

    fn resolve_function(&self, name: &str) -> Result<Function> {
        self.parent.resolve_function(name)
    }

    fn name(&self) -> String {
        format!("{} --> {}", self.parent.name(), self.name.clone())
    }
}

#[derive(Debug, Clone)]
enum Function {
    Defined {
        name: String,
        args: HashMap<String, usize>,
        body: AstNode,
    },
    Builtin(Builtin),
    Alias(String),
}
struct Compiler {
    table: SymbolsTable,
    ast: ParsingAst,
}
impl Compiler {
    fn register_columns(&mut self) -> Result<()> {
        let defcolumns = self.ast.get_defbuiltin(Builtin::Defcolumns);
        for def in defcolumns {
            for col in def {
                match col {
                    AstNode::Symbol(name) => self.table.insert_symbol(name),
                    _ => Err(eyre!("Invalid column name found in defcolumns")),
                }?
            }
        }

        Ok(())
    }
    fn compile_funcs(&mut self) -> Result<()> {
        fn parse_header(header: &AstNode) -> Result<(String, Vec<String>)> {
            if let AstNode::List { args } = header {
                if let AstNode::Symbol(fname) = &args[0] {
                    let arg_names = args
                        .iter()
                        .skip(1)
                        .map(|a| {
                            if let AstNode::Symbol(n) = a {
                                Ok(n.to_owned())
                            } else {
                                Err(eyre!("{:?} is not a valid argument", a))
                            }
                        })
                        .collect::<Result<Vec<_>>>()
                        .with_context(|| format!("while parsing function {}", fname))?;
                    Ok((fname.to_owned(), arg_names))
                } else {
                    Err(eyre!("Not a functopn: {:?}", args[0]))
                }
            } else {
                bail!("SSS")
            }
        }

        let defuns = self.ast.get_defbuiltin(Builtin::Defun);

        for defun in defuns.iter() {
            if defun.len() != 2 {
                bail!("Invalid DEFUN found")
            }
            let header = &defun[0];
            let body = &defun[1];
            let (name, args) = parse_header(header)?;
            body.fold(
                &|ax: Result<()>, n| {
                    if let AstNode::Symbol(name) = n {
                        ax.and_then(|_| {
                            self.table.resolve_function(name).map(|_| ()).or(args
                                .contains(&name)
                                .then(|| ())
                                .ok_or(eyre!("symbol `{}` unknown", name)))
                        })
                    } else {
                        ax
                    }
                },
                Ok(()),
            )
            .with_context(|| format!("while parsing function `{}`", name))?;
            if self.table.funcs.contains_key(&name) {
                return Err(eyre!("DEFUN: function `{}` already exists", name));
            } else {
                self.table.funcs.insert(
                    name.to_owned(),
                    Function::Defined {
                        name,
                        args: args.into_iter().enumerate().map(|(i, a)| (a, i)).collect(),
                        body: body.to_owned(),
                    },
                );
            }
        }

        Ok(())
    }

    fn compile_aliases(&mut self) -> Result<()> {
        let defaliases = self.ast.get_defbuiltin(Builtin::Defalias);
        for defalias in defaliases.iter() {
            if defalias.len() != 2 {
                return Err(eyre!(
                    "`defalias`: two arguments expected, {} found",
                    defalias.len()
                ));
            }

            if let AstNode::Symbol(from) = &defalias[0] {
                if let AstNode::Symbol(to) = &defalias[1] {
                    self.table
                        .insert_alias(from, to)
                        .with_context(|| format!("while defining alias {} -> {}", from, to,))?
                } else {
                    return Err(eyre!(
                        "Invalid argument found in DEFALIAS: {:?}",
                        defalias[1]
                    ));
                }
            } else {
                return Err(eyre!(
                    "Invalid argument found in DEFALIAS: {:?}",
                    defalias[0]
                ));
            }
        }

        let defunaliases = self.ast.get_defbuiltin(Builtin::Defunalias);
        for defunalias in defunaliases.iter() {
            if defunalias.len() != 2 {
                return Err(eyre!(
                    "`DEFUNALIAS`: two arguments expected, {} found",
                    defunalias.len()
                ));
            }

            if let AstNode::Symbol(from) = &defunalias[0] {
                if let AstNode::Symbol(to) = &defunalias[1] {
                    self.table.insert_funalias(from, to).with_context(|| {
                        format!("while defining function alias {} -> {}", from, to,)
                    })?
                } else {
                    return Err(eyre!(
                        "Invalid argument found in DEFUNALIAS: {:?}",
                        defunalias[1]
                    ));
                }
            } else {
                return Err(eyre!(
                    "Invalid argument found in DEFUNALIAS: {:?}",
                    defunalias[0]
                ));
            }
        }

        Ok(())
    }

    fn apply(
        &self,
        f: &Function,
        args: &[AstNode],
        ctx: &dyn Resolver,
    ) -> Result<Option<Constraint>> {
        let mut traversed_args: Vec<Constraint> = vec![];
        for arg in args.iter() {
            let traversed = self.reduce(arg, ctx)?;
            if let Some(traversed) = traversed {
                traversed_args.push(traversed);
            }
        }

        match f {
            Function::Builtin(builtin) => match builtin {
                Builtin::Defun | Builtin::Defalias | Builtin::Defcolumns | Builtin::Defunalias => {
                    Ok(None)
                }
                Builtin::Begin => Ok(Some(Constraint::List(traversed_args))),
                builtin @ _ => Ok(Some(Constraint::Funcall {
                    func: *builtin,
                    args: traversed_args,
                })),
            },
            Function::Defined {
                name: fname,
                args: f_args,
                body,
            } => {
                if f_args.len() != args.len() {
                    return Err(eyre!(
                        "Inconsistent arity: function `{}` takes {} argument{} ({}) but received {}",
                        fname,
                        f_args.len(),
                        if f_args.len() > 1 { "" } else { "s" },
                        f_args.keys().cloned().collect::<Vec<_>>().join(", "),
                        args.len()
                    ));
                }
                self.reduce(
                    body,
                    &FunctionTable {
                        args_mapping: f_args
                            .into_iter()
                            .map(|x| x.0)
                            .cloned()
                            .zip(traversed_args.into_iter())
                            .collect(),
                        parent: ctx,
                        name: format!("FU {}", fname),
                    },
                )
            }
            _ => unimplemented!(),
        }
    }

    fn reduce(&self, e: &AstNode, ctx: &dyn Resolver) -> Result<Option<Constraint>> {
        match e {
            AstNode::Ignore => Ok(None),
            AstNode::Value(x) => Ok(Some(Constraint::Const(*x))),
            AstNode::Symbol(name) => Ok(Some(ctx.resolve_symbol(name)?)),
            AstNode::List { args } => {
                if let AstNode::Symbol(verb) = &args[0] {
                    let func = self.table.resolve_function(&verb)?;
                    if matches!(
                        func,
                        Function::Builtin(Builtin::Defalias)
                            | Function::Builtin(Builtin::Defun)
                            | Function::Builtin(Builtin::Defcolumns)
                    ) {
                        return Ok(None);
                    };

                    // match func {
                    //     Function::Builtin(builtin) => match builtin {
                    //         Builtin::Defun
                    //         | Builtin::Defalias
                    //         | Builtin::Defcolumns
                    //         | Builtin::Defunalias => Ok(None),
                    //         // Builtin::Begin => Ok(Some(Constraint::List(traversed_args))),
                    //         // builtin @ _ => Ok(Some(Constraint::Funcall {
                    //         //     func: *builtin,
                    //         //     args: traversed_args,
                    //         // })),
                    //     },
                    // };
                    let func_args = match &func {
                        Function::Builtin(_) => vec![],
                        Function::Defined { ref args, .. } => args.keys().cloned().collect(),
                        _ => unimplemented!(),
                    };
                    self.apply(&func, &args[1..], ctx)
                } else {
                    Err(eyre!("Not a function: {:?}", args[0]))
                }
            }
        }
    }

    fn build_constraints(&mut self) -> Result<ConstraintsSet> {
        let mut cs = ConstraintsSet {
            constraints: vec![],
        };

        for exp in &self.ast.exprs.to_vec() {
            self.reduce(exp, &self.table)
                .with_context(|| "while assembling top-level constraint")?
                .map(|c| cs.constraints.push(c));
        }
        Ok(cs)
    }

    fn compile(ast: ParsingAst) -> Result<ConstraintsSet> {
        let mut compiler = Compiler {
            table: SymbolsTable::new(),
            ast,
        };

        compiler.register_columns()?;
        compiler.compile_funcs()?;

        compiler.compile_aliases()?;
        let cs = compiler.build_constraints()?;
        Ok(cs)

        // Err(eyre!("ADFA"))
    }
}
