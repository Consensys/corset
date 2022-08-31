use color_eyre::eyre::*;
use pest::{iterators::Pair, Parser};
use std::collections::{HashMap, HashSet};
use std::fmt;
use std::fmt::Debug;

#[derive(Parser)]
#[grammar = "corset.pest"]
struct CorsetParser;

lazy_static::lazy_static! {
    static ref BUILTINS: HashMap<&'static str, Function> = maplit::hashmap!{
        "defun" => Function {
            name: "defun".into(),
            class: FunctionClass::SpecialForm(Form::Defun),
        },

        "defunalias" => Function {
            name: "defunalias".into(),
            class: FunctionClass::SpecialForm(Form::Defunalias),
        },

        "defalias" => Function {
            name: "defalias".into(),
            class: FunctionClass::SpecialForm(Form::Defalias),
        },
        "defcolumns" => Function {
            name: "defcolumns".into(),
            class: FunctionClass::SpecialForm(Form::Defcolumns),
        },


        // monadic
        "inv" => Function{
            name: "inv".into(),
            class: FunctionClass::Builtin(Builtin::Inv)
        },
        "neg" => Function{
            name: "neg".into(),
            class: FunctionClass::Builtin(Builtin::Neg)
        },

        // Dyadic
        "if-zero" => Function{
            name: "if-zero".into(),
            class: FunctionClass::Builtin(Builtin::IfZero),
        },

        "shift" => Function{
            name: "shift".into(),
            class: FunctionClass::Builtin(Builtin::Shift),
        },


        // polyadic
        "add" => Function{
            name: "add".into(),
            class: FunctionClass::Builtin(Builtin::Add)
        },
        "+" => Function {
            name: "+".into(),
            class: FunctionClass::Alias("add".into())
        },


        "mul" => Function{
            name: "mul".into(),
            class: FunctionClass::Builtin(Builtin::Mul)
        },
        "*" => Function {
            name: "*".into(),
            class: FunctionClass::Alias("mul".into())
        },
        "and" => Function {
            name: "and".into(),
            class: FunctionClass::Alias("mul".into())
        },

        "sub" => Function{
            name: "mul".into(),
            class: FunctionClass::Builtin(Builtin::Sub,)
        },
        "-" => Function {
            name: "sub".into(),
            class: FunctionClass::Alias("sub".into())
        },
        "eq" => Function {
            name: "sub".into(),
            class: FunctionClass::Alias("sub".into())
        },
        "=" => Function {
            name: "sub".into(),
            class: FunctionClass::Alias("sub".into())
        },

        // Special form
        "begin" => Function{name: "begin".into(), class: FunctionClass::SpecialForm(Form::Begin)},


        // Special form for now, see later if implementing map...
        "branch-if-zero" => Function {
            name:"branch-if-zero".into(),
            class: FunctionClass::SpecialForm(Form::BranchIfZero)
        },
        "branch-if-zero-else" => Function {
            name:"branch-if-zero-else".into(),
            class: FunctionClass::SpecialForm(Form::BranchIfZero)
        },
    };
}

pub(crate) trait Transpiler {
    fn render(&self, cs: &ConstraintsSet) -> Result<String>;
}

#[derive(Clone)]
pub enum Constraint {
    Funcall {
        func: Builtin,
        args: Vec<Constraint>,
    },
    Const(i32),
    Column(String),
    List(Vec<Constraint>),
}
impl Debug for Constraint {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fn format_list(cs: &[Constraint]) -> String {
            cs.iter()
                .map(|c| format!("{:?}", c))
                .collect::<Vec<_>>()
                .join(" ")
        }

        match self {
            Constraint::Const(x) => write!(f, "{}", x),
            Constraint::Column(name) => write!(f, "{}", name),
            Constraint::List(cs) => write!(f, "'({})", format_list(cs)),
            Self::Funcall { func, args } => write!(f, "({:?} {})", func, format_list(args)),
        }
    }
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
    fn get_forms(&self, b: Form) -> Vec<&[AstNode]> {
        let builtin_name = match b {
            Form::Defcolumns => "defcolumns",
            Form::Defunalias => "defunalias",
            Form::Defalias => "defalias",
            Form::Defun => "defun",
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
pub enum Form {
    Defun,
    Defalias,
    Defunalias,
    Defcolumns,
    Begin,
    // Don't like it :/
    BranchIfZero,
    BranchIfNotZero,
    BranchBinIfZero,
    BranchBinIfOne,
    BranchIfZeroElse,
    BranchIfNotZeroElse,
    BranchBinIfZeroElse,
    BranchBinIfOneElse,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Builtin {
    Add,
    Sub,
    Mul,
    IfZero,
    Shift,
    Neg,
    Inv,
}

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
            funcs: BUILTINS
                .iter()
                .map(|(k, v)| (k.to_string(), v.clone()))
                .collect(),
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

    fn insert_func(&mut self, f: Function) -> Result<()> {
        if self.funcs.contains_key(&f.name) {
            Err(anyhow!("function `{}` already defined", &f.name))
        } else {
            self.funcs.insert(f.name.clone(), f);
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
            self.funcs.insert(
                from.into(),
                Function {
                    name: from.into(),
                    class: FunctionClass::Alias(to.into()),
                },
            );
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
                None => Err(eyre!("Function `{}` unknown", name)),
                Some(Function {
                    class: FunctionClass::Alias(ref name),
                    ..
                }) => self._resolve_function(name, ax),
                Some(f) => Ok(f.to_owned()),
            }
        }
    }
}
impl Resolver for SymbolsTable {
    fn resolve_symbol(&self, name: &str) -> Result<Constraint> {
        self._resolve_symbol(name, &mut HashSet::new())
    }

    fn resolve_function(&self, name: &str) -> Result<Function> {
        self._resolve_function(name, &mut HashSet::new())
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
struct Function {
    name: String,
    class: FunctionClass,
}
impl Function {
    const VARIADIC_0: isize = -1;
    const VARIADIC_1: isize = -2;
    const VARIADIC_2: isize = -3;
    fn arity(&self) -> isize {
        match &self.class {
            FunctionClass::Defined { args, .. } => args.len() as isize,
            FunctionClass::SpecialForm(f) => match f {
                Form::Defun => 2,
                Form::Begin => Self::VARIADIC_1,
                Form::Defalias => 2,
                Form::Defunalias => 2,
                Form::Defcolumns => Self::VARIADIC_1,
                Form::BranchIfZero => 2,
                Form::BranchIfZeroElse => 3,
                x @ _ => unimplemented!("TBI: {:?}", x),
            },
            FunctionClass::Builtin(f) => match f {
                Builtin::Add => Self::VARIADIC_2,
                Builtin::Sub => Self::VARIADIC_2,
                Builtin::Mul => Self::VARIADIC_2,
                Builtin::Neg => 1,
                Builtin::Inv => 1,
                Builtin::IfZero => 2,
                Builtin::Shift => 2,
            },
            FunctionClass::Alias(_) => unreachable!(),
        }
    }

    fn make_arity_error(&self, args: &[Constraint]) -> String {
        let arity = self.arity();
        return format!(
            "`{}` expects {} argument{} but received {}",
            self.name,
            match arity {
                Self::VARIADIC_1 => String::from("at least one"),
                Self::VARIADIC_2 => String::from("at least two"),
                arity @ _ => arity.to_string(),
            },
            if arity <= Self::VARIADIC_1 || arity > 1 {
                "s"
            } else {
                ""
            },
            args.len(),
        );
    }

    fn validate_arity(&self, args: &[Constraint]) -> Result<()> {
        let arity = self.arity();
        ((arity == Self::VARIADIC_1 && args.len() >= 1)
            || (arity == Self::VARIADIC_2 && args.len() >= 2)
            || (arity == args.len() as isize))
            .then(|| ())
            .ok_or(eyre!(self.make_arity_error(&args)))
    }

    fn validate_types(&self, args: &[Constraint]) -> Result<()> {
        match &self.class {
            FunctionClass::SpecialForm(f) => match f {
                Form::Begin => Ok(()),
                Form::BranchIfZero => {
                    if !matches!(args[0], Constraint::List(_))
                        && matches!(args[1], Constraint::List(_))
                    {
                        Ok(())
                    } else {
                        Err(eyre!(
                            "`{}` expects scalar arguments but received a list",
                            self.name
                        ))
                    }
                }
                Form::BranchIfZeroElse => {
                    if !matches!(args[0], Constraint::List(_))
                        && matches!(args[1], Constraint::List(_))
                        && matches!(args[2], Constraint::List(_))
                    {
                        Ok(())
                    } else {
                        Err(eyre!(
                            "`{}` expects scalar arguments but received a list",
                            self.name
                        ))
                    }
                }
                x @ _ => {
                    eprintln!("WARN: {:?} not yet checked", x);
                    Ok(())
                }
            },
            FunctionClass::Defined { .. } => Ok(()),
            FunctionClass::Builtin(b) => match b {
                Builtin::Add | Builtin::Sub | Builtin::Mul | Builtin::IfZero => {
                    if args.iter().all(|a| !matches!(a, Constraint::List(_))) {
                        Ok(())
                    } else {
                        Err(eyre!(
                            "`{}` expects scalar arguments but received a list",
                            self.name
                        ))
                    }
                }
                Builtin::Neg | Builtin::Inv => {
                    if args.iter().all(|a| !matches!(a, Constraint::List(_))) {
                        Ok(())
                    } else {
                        Err(eyre!(
                            "`{}` expects a scalar argument but received a list",
                            self.name
                        ))
                    }
                }
                Builtin::Shift => {
                    if matches!(args[0], Constraint::Column(_))
                        && matches!(args[1], Constraint::Const(x) if x != 0)
                    {
                        Ok(())
                    } else {
                        Err(eyre!(
                            "`shift` expects a COLUMN and a non-null INTEGER but received {:?}",
                            args
                        ))
                    }
                }
            },
            _ => unreachable!(),
        }
    }

    fn validate_args(&self, args: Vec<Constraint>) -> Result<Vec<Constraint>> {
        self.validate_arity(&args)
            .and(self.validate_types(&args))
            .and(Ok(args))
    }
}

#[derive(Debug, Clone)]
enum FunctionClass {
    Defined { args: Vec<String>, body: AstNode },
    SpecialForm(Form),
    Builtin(Builtin),
    Alias(String),
}
struct Compiler {
    table: SymbolsTable,
    ast: ParsingAst,
}
impl Compiler {
    fn register_columns(&mut self) -> Result<()> {
        let defcolumns = self.ast.get_forms(Form::Defcolumns);
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
                    Err(eyre!("Not a function: {:?}", args[0]))
                }
            } else {
                bail!("SSS")
            }
        }

        let defuns = self.ast.get_forms(Form::Defun);

        for defun in defuns.iter() {
            if defun.len() != 2 {
                bail!("Invalid DEFUN found")
            }
            let header = &defun[0];
            let body = &defun[1];
            let (name, args) = parse_header(header)?;
            if self.table.funcs.contains_key(&name) {
                return Err(eyre!("DEFUN: function `{}` already exists", name));
            } else {
                self.table.insert_func({
                    Function {
                        name: name.to_owned(),
                        class: FunctionClass::Defined {
                            args,
                            body: body.to_owned(),
                        },
                    }
                })?;
            }
        }

        Ok(())
    }

    fn compile_aliases(&mut self) -> Result<()> {
        let defaliases = self.ast.get_forms(Form::Defalias);
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

        let defunaliases = self.ast.get_forms(Form::Defunalias);
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

        match &f.class {
            FunctionClass::SpecialForm(Form::Begin) => Ok(Some(Constraint::List(traversed_args))),
            FunctionClass::SpecialForm(Form::BranchIfZero) => {
                println!("TA: {:?}", traversed_args);
                let cond = traversed_args[0].clone();
                if let Constraint::List(then) = &traversed_args[1] {
                    Ok(Some(Constraint::List(
                        then.into_iter()
                            .map(|a| Constraint::Funcall {
                                func: Builtin::IfZero,
                                args: vec![cond.clone(), a.clone()],
                            })
                            .collect(),
                    )))
                } else {
                    unreachable!()
                }
            }
            FunctionClass::SpecialForm(Form::BranchIfZeroElse) => {
                let cond = traversed_args[0].clone();
                if let Constraint::List(tthen) = &traversed_args[1] {
                    if let Constraint::List(eelse) = &traversed_args[2] {
                        Ok(Some(Constraint::List(
                            tthen
                                .into_iter()
                                .map(|a| Constraint::Funcall {
                                    func: Builtin::IfZero,
                                    args: vec![cond.clone(), a.clone()],
                                })
                                .chain(eelse.iter().map(|a| Constraint::Funcall {
                                    func: Builtin::Mul,
                                    args: vec![cond.clone(), a.clone()],
                                }))
                                .collect(),
                        )))
                    } else {
                        unreachable!()
                    }
                } else {
                    unreachable!()
                }
            }
            FunctionClass::SpecialForm(_) => Ok(None),

            FunctionClass::Builtin(builtin) => Ok(Some(Constraint::Funcall {
                func: *builtin,
                args: f.validate_args(traversed_args)?,
            })),

            FunctionClass::Defined { args: f_args, body } => {
                let traversed_args = f.validate_args(traversed_args)?;
                self.reduce(
                    &body,
                    &FunctionTable {
                        args_mapping: f_args
                            .into_iter()
                            .enumerate()
                            .map(|(i, f_arg)| (f_arg.to_owned(), traversed_args[i].clone()))
                            .collect(),
                        parent: ctx,
                        name: format!("FU {}", f.name),
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
                        func.class,
                        FunctionClass::SpecialForm(Form::Defun)
                            | FunctionClass::SpecialForm(Form::Defalias)
                            | FunctionClass::SpecialForm(Form::Defunalias)
                            | FunctionClass::SpecialForm(Form::Defcolumns)
                    ) {
                        return Ok(None);
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
