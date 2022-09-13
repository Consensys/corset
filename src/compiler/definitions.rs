use eyre::*;
use num_bigint::BigInt;
use num_traits::{One, Zero};
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

use super::common::BUILTINS;
use super::generator::{Defined, Expression, Function, FunctionClass};
use super::Type;
use crate::compiler::parser::*;

#[derive(Debug, Clone)]
pub enum Symbol {
    Alias(String),
    Final(Expression, bool),
}
#[derive(Debug)]
pub struct SymbolTable {
    parent: Option<Rc<RefCell<SymbolTable>>>,
    constraints: HashMap<String, HashSet<String>>, // Module -> Name -> Present?
    funcs: HashMap<String, Function>,
    symbols: HashMap<String, HashMap<String, (Symbol, Type)>>, // Module -> Name -> Symbol/Type
}
impl SymbolTable {
    pub fn new_root() -> SymbolTable {
        SymbolTable {
            funcs: BUILTINS
                .iter()
                .map(|(k, v)| (k.to_string(), v.clone()))
                .collect(),
            symbols: Default::default(),
            parent: None,
            constraints: Default::default(),
        }
    }

    pub fn derived(parent: Rc<RefCell<SymbolTable>>) -> Rc<RefCell<SymbolTable>> {
        Rc::new(RefCell::new(SymbolTable {
            funcs: Default::default(),
            symbols: Default::default(),
            constraints: Default::default(),
            parent: Some(parent),
        }))
    }

    pub fn symbols(&self) -> impl Iterator<Item = (&String, &(Symbol, Type))> {
        self.symbols
            .iter()
            .flat_map(|(k, m)| m.values().map(move |s| (k, s)))
    }

    fn _resolve_symbol(
        &mut self,
        module: &str,
        name: &str,
        ax: &mut HashSet<String>,
    ) -> Result<(Expression, Type)> {
        if ax.contains(name) {
            Err(eyre!("Circular definitions found for {}", name))
        } else {
            ax.insert(name.into());
            // Ugly, but required for borrowing reasons
            if let Some((Symbol::Alias(name), _)) = self
                .symbols
                .get(module)
                .and_then(|module| module.get(name))
                .cloned()
            {
                self._resolve_symbol(module, &name, ax)
            } else {
                match self
                    .symbols
                    .get_mut(module)
                    .and_then(|module| module.get_mut(name))
                {
                    Some((Symbol::Final(constraint, visited), t)) => {
                        *visited = true;
                        Ok((constraint.clone(), *t))
                    }
                    None => self.parent.as_ref().map_or(
                        Err(eyre!("Column `{}` unknown in module `{}`", name, module)),
                        |parent| parent.borrow_mut().resolve_symbol(module, name),
                    ),
                    _ => unimplemented!(),
                }
            }
        }
    }

    fn _edit_symbol(
        &mut self,
        module: &str,
        name: &str,
        f: &dyn Fn(&mut Expression),
        ax: &mut HashSet<String>,
    ) -> Result<()> {
        if ax.contains(name) {
            Err(eyre!("Circular definitions found for {}", name))
        } else {
            ax.insert(name.into());
            // Ugly, but required for borrowing reasons
            if let Some((Symbol::Alias(name), _)) = self
                .symbols
                .get(module)
                .and_then(|module| module.get(name))
                .cloned()
            {
                self._edit_symbol(module, &name, f, ax)
            } else {
                match self
                    .symbols
                    .get_mut(module)
                    .and_then(|module| module.get_mut(name))
                {
                    Some((Symbol::Final(constraint, _), _)) => {
                        f(constraint);
                        Ok(())
                    }
                    None => self.parent.as_ref().map_or(
                        Err(eyre!("Column `{}` unknown in module `{}`", name, module)),
                        |parent| parent.borrow_mut().edit_symbol(module, name, f),
                    ),
                    _ => unimplemented!(),
                }
            }
        }
    }

    fn _resolve_function(&self, name: &str, ax: &mut HashSet<String>) -> Result<Function> {
        if ax.contains(name) {
            Err(eyre!("Circular definitions found for {}", name))
        } else {
            ax.insert(name.into());
            match self.funcs.get(name) {
                Some(Function {
                    class: FunctionClass::Alias(ref to),
                    ..
                }) => self._resolve_function(to, ax),
                Some(f) => Ok(f.to_owned()),
                None => self
                    .parent
                    .as_ref()
                    .map_or(Err(eyre!("Function `{}` unknown", name)), |parent| {
                        parent.borrow().resolve_function(name)
                    }),
            }
        }
    }

    pub fn insert_constraint(&mut self, module: &str, name: &str) -> Result<()> {
        if self
            .constraints
            .entry(module.into())
            .or_default()
            .contains(name)
        {
            eprintln!("WARN redefining constraint `{}`", name);
        }
        self.constraints
            .entry(module.into())
            .or_default()
            .insert(name.to_owned());
        Ok(())
        // .then(|| ())
        // .ok_or_else(|| eyre!("Constraint `{}` already defined", name))
    }

    pub fn insert_symbol(&mut self, module: &str, symbol: &str, e: Expression) -> Result<()> {
        let t = match e {
            Expression::Column(_, t, _) => t,
            Expression::ArrayColumn(_, _, t) => t,
            Expression::Const(ref x) => {
                if Zero::is_zero(x) || One::is_one(x) {
                    Type::Boolean
                } else {
                    Type::Numeric
                }
            }
            _ => Type::Numeric, // TODO FIXME
        };
        if self
            .symbols
            .entry(module.into())
            .or_default()
            .contains_key(symbol)
        {
            Err(anyhow!(
                "column `{}` already exists in module `{}`",
                symbol,
                module
            ))
        } else {
            self.symbols
                .entry(module.into())
                .or_default()
                .insert(symbol.into(), (Symbol::Final(e, false), t));
            Ok(())
        }
    }

    pub fn insert_func(&mut self, f: Function) -> Result<()> {
        if self.funcs.contains_key(&f.name) {
            Err(anyhow!("function `{}` already defined", &f.name))
        } else {
            self.funcs.insert(f.name.clone(), f);
            Ok(())
        }
    }

    pub fn insert_alias(&mut self, module: &str, from: &str, to: &str) -> Result<()> {
        if self.symbols.contains_key(from) {
            Err(anyhow!("`{}` already exists", from))
        } else {
            self.symbols
                .entry(module.into())
                .or_default()
                .insert(from.into(), (Symbol::Alias(to.into()), Type::Void));
            Ok(())
        }
    }

    pub fn insert_funalias(&mut self, from: &str, to: &str) -> Result<()> {
        if self.funcs.contains_key(from) {
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

    pub fn resolve_symbol(&mut self, module: &str, name: &str) -> Result<(Expression, Type)> {
        self._resolve_symbol(module, name, &mut HashSet::new())
    }

    pub fn edit_symbol(
        &mut self,
        module: &str,
        name: &str,
        f: &dyn Fn(&mut Expression),
    ) -> Result<()> {
        self._edit_symbol(module, name, f, &mut HashSet::new())
    }

    pub fn resolve_function(&self, name: &str) -> Result<Function> {
        self._resolve_function(name, &mut HashSet::new())
    }

    pub fn insert_constant(&mut self, module: &str, name: &str, value: BigInt) -> Result<()> {
        let t = if Zero::is_zero(&value) || One::is_one(&value) {
            Type::Boolean
        } else {
            Type::Numeric
        };
        if self.symbols.contains_key(name) {
            Err(anyhow!("`{}` already exists", name))
        } else {
            self.symbols.entry(module.into()).or_default().insert(
                name.into(),
                (Symbol::Final(Expression::Const(value), false), t),
            );
            Ok(())
        }
    }
}

fn reduce(e: &AstNode, ctx: Rc<RefCell<SymbolTable>>, module: &mut String) -> Result<()> {
    match &e.class {
        Token::Value(_)
        | Token::Symbol(_)
        | Token::Form(_)
        | Token::Range(_)
        | Token::Type(_)
        | Token::DefPlookup(..) => Ok(()),

        Token::DefConstraint(name, ..) => ctx
            .borrow_mut()
            .insert_constraint(module, &format!("{}-{}", module, name)),
        Token::DefModule(name) => {
            *module = String::from(name);
            Ok(())
        }
        Token::DefConst(name, x) => {
            ctx.borrow_mut()
                .insert_constant(&module, name, (*x).try_into().unwrap())
        }
        Token::DefColumns(cols) => cols
            .iter()
            .fold(Ok(()), |ax, col| ax.and(reduce(col, ctx.clone(), module))),
        Token::DefColumn(col, t, kind) => ctx.borrow_mut().insert_symbol(
            module,
            col,
            Expression::Column(
                format!("{}-{}", module, col),
                *t,
                // Convert Kind<AstNode> to Kind<Expression>
                match kind {
                    Kind::Atomic => Kind::Atomic,
                    Kind::Composite(_) => Kind::Atomic, // The actual expression is computed by the generator
                    Kind::Sorted(src) => Kind::Sorted(src.clone()),
                    Kind::Interleaved(xs) => Kind::Interleaved(xs.clone()),
                },
            ),
        ),
        Token::DefArrayColumn(col, range, t) => ctx.borrow_mut().insert_symbol(
            module,
            col,
            Expression::ArrayColumn(format!("{}-{}", module, col), range.clone(), *t),
        ),
        Token::DefAliases(aliases) => aliases.iter().fold(Ok(()), |ax, alias| {
            ax.and(reduce(alias, ctx.clone(), module))
        }),
        Token::Defun(name, args, body) => ctx.borrow_mut().insert_func(Function {
            name: name.into(),
            class: FunctionClass::UserDefined(Defined {
                args: args.to_owned(),
                body: *body.clone(),
            }),
        }),
        Token::DefAlias(from, to) => ctx
            .borrow_mut()
            .insert_alias(module, from, to)
            .with_context(|| eyre!("defining {} -> {}", from, to)),
        Token::DefunAlias(from, to) => ctx
            .borrow_mut()
            .insert_funalias(from, to)
            .with_context(|| eyre!("defining {} -> {}", from, to)),
    }
}

pub fn pass(ast: &Ast, ctx: Rc<RefCell<SymbolTable>>) -> Result<()> {
    let mut module = String::from(super::MAIN_MODULE);
    for e in ast.exprs.iter() {
        reduce(e, ctx.clone(), &mut module)?;
    }

    Ok(())
}