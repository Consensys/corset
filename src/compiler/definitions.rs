use eyre::*;
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
    constraints: HashSet<String>,
    funcs: HashMap<String, Function>,
    symbols: HashMap<String, (Symbol, Type)>,
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

    pub fn symbols(&self) -> impl Iterator<Item = &Symbol> {
        self.symbols.values().map(|(v, _)| v)
    }

    fn _resolve_symbol(
        &mut self,
        name: &str,
        ax: &mut HashSet<String>,
    ) -> Result<(Expression, Type)> {
        if ax.contains(name) {
            Err(eyre!("Circular definitions found for {}", name))
        } else {
            ax.insert(name.into());
            // Ugly, but required for borrowing reasons
            if let Some((Symbol::Alias(name), _)) = self.symbols.get(name).cloned() {
                self._resolve_symbol(&name, ax)
            } else {
                match self.symbols.get_mut(name) {
                    Some((Symbol::Final(constraint, visited), t)) => {
                        *visited = true;
                        Ok((constraint.clone(), *t))
                    }
                    None => self
                        .parent
                        .as_ref()
                        .map_or(Err(eyre!("Column `{}` unknown", name)), |parent| {
                            parent.borrow_mut().resolve_symbol(name)
                        }),
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

    pub fn insert_constraint(&mut self, name: &str) -> Result<()> {
        self.constraints
            .insert(name.into())
            .then(|| ())
            .ok_or_else(|| eyre!("Constraint `{}` already defined", name))
    }

    pub fn insert_symbol(&mut self, symbol: &str, e: Expression) -> Result<()> {
        let t = match e {
            Expression::Column(_, t) => t,
            Expression::ArrayColumn(_, _, t) => t,
            Expression::Const(x) => {
                if x == 0 || x == 1 {
                    Type::Boolean
                } else {
                    Type::Numeric
                }
            }
            _ => Type::Numeric, // TODO FIXME
        };
        if self.symbols.contains_key(symbol) {
            Err(anyhow!("column `{}` already exists", symbol))
        } else {
            self.symbols
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

    pub fn insert_alias(&mut self, from: &str, to: &str) -> Result<()> {
        if self.symbols.contains_key(from) {
            Err(anyhow!("`{}` already exists", from))
        } else {
            self.symbols
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

    pub fn resolve_symbol(&mut self, name: &str) -> Result<(Expression, Type)> {
        self._resolve_symbol(name, &mut HashSet::new())
    }

    pub fn resolve_function(&self, name: &str) -> Result<Function> {
        self._resolve_function(name, &mut HashSet::new())
    }

    pub fn insert_constant(&mut self, name: &str, value: i32) -> Result<()> {
        let t = if value == 0 || value == 1 {
            Type::Boolean
        } else {
            Type::Numeric
        };
        if self.symbols.contains_key(name) {
            Err(anyhow!("`{}` already exists", name))
        } else {
            self.symbols.insert(
                name.into(),
                (Symbol::Final(Expression::Const(value), false), t),
            );
            Ok(())
        }
    }
}

fn reduce(e: &AstNode, ctx: Rc<RefCell<SymbolTable>>) -> Result<()> {
    match &e.class {
        Token::Value(_)
        | Token::Symbol(_)
        | Token::Form(_)
        | Token::Range(_)
        | Token::Type(_)
        | Token::DefPlookup(..) => Ok(()),

        Token::DefConstraint(name, ..) => ctx.borrow_mut().insert_constraint(name),
        Token::DefConst(name, x) => ctx.borrow_mut().insert_constant(name, *x as i32),
        Token::DefColumns(cols) => cols
            .iter()
            .fold(Ok(()), |ax, col| ax.and(reduce(col, ctx.clone()))),
        Token::DefColumn(col, t) => ctx
            .borrow_mut()
            .insert_symbol(col, Expression::Column(col.into(), *t)),
        Token::DefArrayColumn(col, range, t) => ctx
            .borrow_mut()
            .insert_symbol(col, Expression::ArrayColumn(col.into(), range.clone(), *t)),
        Token::DefAliases(aliases) => aliases
            .iter()
            .fold(Ok(()), |ax, alias| ax.and(reduce(alias, ctx.clone()))),
        Token::Defun(name, args, body) => ctx.borrow_mut().insert_func(Function {
            name: name.into(),
            class: FunctionClass::UserDefined(Defined {
                args: args.to_owned(),
                body: *body.clone(),
            }),
        }),
        Token::DefAlias(from, to) => ctx
            .borrow_mut()
            .insert_alias(from, to)
            .with_context(|| eyre!("defining {} -> {}", from, to)),
        Token::DefunAlias(from, to) => ctx
            .borrow_mut()
            .insert_funalias(from, to)
            .with_context(|| eyre!("defining {} -> {}", from, to)),
    }
}

pub fn pass(ast: &Ast, ctx: Rc<RefCell<SymbolTable>>) -> Result<()> {
    for e in ast.exprs.iter() {
        reduce(e, ctx.clone())?;
    }

    Ok(())
}
