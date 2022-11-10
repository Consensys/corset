use anyhow::*;
use colored::Colorize;
use log::*;
use num_bigint::BigInt;
use num_traits::{One, Zero};
use pairing_ce::bn256::Fr;
use pairing_ce::ff::PrimeField;
use serde::{Deserialize, Serialize};
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

use super::common::BUILTINS;
use super::generator::{Defined, Expression, Function, FunctionClass};
use super::{Handle, Magma, Type};
use crate::column::Computation;
use crate::compiler::parser::*;

#[derive(Debug, Clone)]
pub enum Symbol {
    Alias(Handle),
    Final(Expression, bool),
}
#[derive(Default, Debug, Clone, Serialize, Deserialize)]
pub struct ComputationTable {
    dependencies: HashMap<Handle, usize>,
    computations: Vec<Computation>,
}
impl ComputationTable {
    pub fn dep(&self, target: &Handle) -> Option<usize> {
        self.dependencies.get(target).cloned()
    }
    pub fn get(&self, i: usize) -> Option<&Computation> {
        self.computations.get(i)
    }
    pub fn iter(&'_ self) -> impl Iterator<Item = &'_ Computation> {
        self.computations.iter()
    }
    pub fn insert(&mut self, target: &Handle, computation: Computation) -> Result<()> {
        if self.dependencies.contains_key(target) {
            return Err(anyhow!(
                "`{}` already present as a computation target",
                target
            ));
        }
        self.computations.push(computation);
        self.dependencies
            .insert(target.to_owned(), self.computations.len() - 1);
        Ok(())
    }
    pub fn insert_multiple(&mut self, targets: &[Handle], computation: Computation) -> Result<()> {
        self.computations.push(computation);
        for target in targets.iter() {
            self.dependencies
                .insert(target.to_owned(), self.computations.len() - 1);
        }
        Ok(())
    }
    pub fn computation_for(&self, target: &Handle) -> Option<&Computation> {
        self.dependencies
            .iter()
            .find(|(k, _)| *k == target)
            .map(|x| &self.computations[*x.1])
    }
}
#[derive(Debug)]
pub struct SymbolTable {
    // The parent relationship is only used for contextual
    // semantics (i.e. for & functions), not modules
    parent: Option<Rc<RefCell<SymbolTable>>>,
    constraints: HashMap<String, HashSet<String>>,
    funcs: HashMap<Handle, Function>,
    symbols: HashMap<Handle, (Symbol, Type)>,
    pub computation_table: ComputationTable,
}
impl SymbolTable {
    pub fn new_root() -> SymbolTable {
        SymbolTable {
            parent: None,
            constraints: Default::default(),
            funcs: BUILTINS
                .values()
                .map(|f| (f.handle.to_owned(), f.clone()))
                .collect(),
            symbols: Default::default(),
            computation_table: Default::default(),
        }
    }

    pub fn derived(parent: Rc<RefCell<SymbolTable>>) -> Rc<RefCell<SymbolTable>> {
        Rc::new(RefCell::new(SymbolTable {
            parent: Some(parent),
            constraints: Default::default(),
            funcs: Default::default(),
            symbols: Default::default(),
            computation_table: Default::default(),
        }))
    }

    #[allow(dead_code)]
    pub fn symbols(&self) -> impl Iterator<Item = (&Handle, &(Symbol, Type))> {
        self.symbols.iter()
    }

    pub fn symbols_mut(&mut self) -> impl Iterator<Item = (&Handle, &mut (Symbol, Type))> {
        self.symbols.iter_mut()
    }

    fn _resolve_symbol(
        &mut self,
        handle: &Handle,
        ax: &mut HashSet<Handle>,
    ) -> Result<(Expression, Type)> {
        if ax.contains(handle) {
            Err(anyhow!("Circular definitions found for {}", handle))
        } else {
            ax.insert(handle.to_owned());
            // Ugly, but required for borrowing reasons
            if let Some((Symbol::Alias(target), _)) = self.symbols.get(handle).cloned() {
                self._resolve_symbol(&target, ax)
            } else {
                match self.symbols.get_mut(handle) {
                    Some((Symbol::Final(constraint, visited), t)) => {
                        *visited = true;
                        Ok((constraint.clone(), *t))
                    }
                    None => self.parent.as_ref().map_or(
                        Err(anyhow!(
                            "Column `{}` unknown in module `{}`",
                            handle.name.red(),
                            handle.module.blue()
                        )),
                        |parent| parent.borrow_mut().resolve_symbol(handle),
                    ),
                    _ => unimplemented!(),
                }
            }
        }
    }

    fn _edit_symbol(
        &mut self,
        handle: &Handle,
        f: &dyn Fn(&mut Expression),
        ax: &mut HashSet<Handle>,
    ) -> Result<()> {
        if ax.contains(handle) {
            Err(anyhow!(
                "Circular definitions found for {}",
                handle.to_string().red()
            ))
        } else {
            ax.insert(handle.to_owned());
            // Ugly, but required for borrowing reasons
            if let Some((Symbol::Alias(_), _)) = self.symbols.get(handle).cloned() {
                self._edit_symbol(handle, f, ax)
            } else {
                match self.symbols.get_mut(handle) {
                    Some((Symbol::Final(constraint, _), _)) => {
                        f(constraint);
                        Ok(())
                    }
                    None => self.parent.as_ref().map_or(
                        Err(anyhow!(
                            "Column `{}` unknown in module `{}`",
                            handle.name.red(),
                            handle.module.blue()
                        )),
                        |parent| parent.borrow_mut().edit_symbol(handle, f),
                    ),
                    _ => unimplemented!(),
                }
            }
        }
    }

    fn _resolve_function(&self, handle: &Handle, ax: &mut HashSet<Handle>) -> Result<Function> {
        if ax.contains(handle) {
            Err(anyhow!(
                "Circular definitions found for {}",
                handle.to_string().red()
            ))
        } else {
            ax.insert(handle.to_owned());
            match self.funcs.get(handle) {
                Some(Function {
                    class: FunctionClass::Alias(ref to),
                    ..
                }) => self._resolve_function(to, ax),
                Some(f) => Ok(f.to_owned()),
                None => self.parent.as_ref().map_or(
                    Err(anyhow!("Function {} unknown", handle.name.red())),
                    |parent| parent.borrow().resolve_function(handle),
                ),
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
            warn!("redefining constraint `{}`", name);
        }
        self.constraints
            .entry(module.into())
            .or_default()
            .insert(name.to_owned())
            .then(|| ())
            .ok_or_else(|| anyhow!("Constraint `{}` already defined", name))
    }

    pub fn insert_symbol(&mut self, handle: &Handle, e: Expression) -> Result<()> {
        let t = e.t();
        if self.symbols.contains_key(handle) {
            Err(anyhow!(
                "column `{}` already exists in module `{}`",
                handle.name.red(),
                handle.module.blue()
            ))
        } else {
            self.symbols
                .insert(handle.to_owned(), (Symbol::Final(e, false), t));
            Ok(())
        }
    }

    pub fn insert_function(&mut self, handle: &Handle, f: Function) -> Result<()> {
        if self.funcs.contains_key(handle) {
            Err(anyhow!(
                "function {} already defined",
                handle.to_string().red()
            ))
        } else {
            self.funcs.insert(handle.to_owned(), f);
            Ok(())
        }
    }

    pub fn insert_alias(&mut self, from: &Handle, to: &Handle) -> Result<()> {
        if self.symbols.contains_key(from) {
            Err(anyhow!("`{}` already exists", from))
        } else {
            self.symbols
                .insert(from.to_owned(), (Symbol::Alias(to.to_owned()), Type::Void));
            Ok(())
        }
    }

    pub fn insert_funalias(&mut self, from: &Handle, to: &Handle) -> Result<()> {
        if self.funcs.contains_key(from) {
            Err(anyhow!(
                "{} already exists: {} -> {}",
                from.to_string().red(),
                from.to_string().red(),
                to.to_string().magenta(),
            ))
        } else {
            self.funcs.insert(
                from.to_owned(),
                Function {
                    handle: to.clone(),
                    class: FunctionClass::Alias(to.clone()),
                },
            );
            Ok(())
        }
    }

    pub fn resolve_symbol(&mut self, handle: &Handle) -> Result<(Expression, Type)> {
        self._resolve_symbol(handle, &mut HashSet::new())
    }

    pub fn edit_symbol(&mut self, handle: &Handle, f: &dyn Fn(&mut Expression)) -> Result<()> {
        self._edit_symbol(handle, f, &mut HashSet::new())
    }

    pub fn resolve_function(&self, handle: &Handle) -> Result<Function> {
        self._resolve_function(handle, &mut HashSet::new())
            .or_else(|_| {
                self._resolve_function(
                    &Handle::new(super::MAIN_MODULE, &handle.name),
                    &mut HashSet::new(),
                )
            })
    }

    pub fn insert_constant(&mut self, handle: &Handle, value: BigInt) -> Result<()> {
        let t = if Zero::is_zero(&value) || One::is_one(&value) {
            Type::Scalar(Magma::Boolean)
        } else {
            Type::Scalar(Magma::Integer)
        };
        if self.symbols.contains_key(handle) {
            Err(anyhow!(
                "`{}` already exists in `{}`",
                handle.name.red(),
                handle.module.blue()
            ))
        } else {
            self.symbols.insert(
                handle.to_owned(),
                (
                    Symbol::Final(
                        Expression::Const(value.clone(), Fr::from_str(&value.to_string())),
                        false,
                    ),
                    t,
                ),
            );
            Ok(())
        }
    }
}

fn reduce(e: &AstNode, ctx: Rc<RefCell<SymbolTable>>, module: &mut String) -> Result<()> {
    match &e.class {
        Token::Value(_)
        | Token::Symbol(_)
        | Token::Keyword(_)
        | Token::List(_)
        | Token::Range(_)
        | Token::Type(_)
        | Token::DefPlookup(..)
        | Token::DefInrange(..) => Ok(()),

        Token::DefConstraint(name, ..) => ctx
            .borrow_mut()
            .insert_constraint(module, &format!("{}-{}", module, name)),
        Token::DefModule(name) => {
            *module = String::from(name);
            Ok(())
        }
        Token::DefConsts(cs) => {
            for (name, value) in cs.iter() {
                ctx.borrow_mut()
                    .insert_constant(&Handle::new(&module, name), value.to_owned())?;
            }
            Ok(())
        }
        Token::DefColumns(cols) => cols
            .iter()
            .fold(Ok(()), |ax, col| ax.and(reduce(col, ctx.clone(), module))),
        Token::DefColumn(col, t, kind) => {
            let handle = Handle::new(&module, col);
            ctx.borrow_mut().insert_symbol(
                &handle,
                Expression::Column(
                    handle.clone(),
                    *t,
                    // Convert Kind<AstNode> to Kind<Expression>
                    match kind {
                        Kind::Atomic => Kind::Atomic,
                        Kind::Phantom => Kind::Phantom,
                        Kind::Composite(_) => Kind::Phantom, // The actual expression is computed by the generator
                        Kind::Interleaved(xs) => Kind::Interleaved(
                            xs.iter().map(|h| Handle::new(&module, &h.name)).collect(),
                        ),
                    },
                ),
            )
        }
        Token::DefArrayColumn(col, range, t) => {
            let handle = Handle::new(&module, col);
            ctx.borrow_mut().insert_symbol(
                &handle,
                Expression::ArrayColumn(handle.clone(), range.to_owned(), *t),
            )?;
            for i in range {
                let column_name = format!("{}_{}", col, i);
                let handle = Handle::new(&module, column_name);
                ctx.borrow_mut().insert_symbol(
                    &handle,
                    Expression::Column(handle.clone(), *t, Kind::Atomic),
                )?;
            }
            Ok(())
        }
        Token::DefSort(tos, froms) => {
            if tos.len() != froms.len() {
                return Err(anyhow!(
                    "cardinality mismatch in permutation declaration: {:?} vs. {:?}",
                    tos,
                    froms
                ));
            }

            let mut _froms = Vec::new();
            let mut _tos = Vec::new();
            for pair in tos.iter().zip(froms.iter()) {
                match pair {
                    (
                        AstNode {
                            class: Token::Symbol(to),
                            ..
                        },
                        AstNode {
                            class: Token::Symbol(from),
                            ..
                        },
                    ) => {
                        let from_handle = Handle::new(&module, &from);
                        let to_handle = Handle::new(&module, &to);
                        ctx.borrow_mut()
                            .resolve_symbol(&Handle::new(&module, from))
                            .with_context(|| "while defining permutation")?;
                        ctx.borrow_mut()
                            .insert_symbol(
                                &to_handle,
                                Expression::Column(
                                    Handle::new(&module, to),
                                    Type::Column(Magma::Integer),
                                    Kind::Phantom,
                                ),
                            )
                            .unwrap_or_else(|e| warn!("while defining permutation: {}", e));
                        _froms.push(from_handle);
                        _tos.push(to_handle);
                    }
                    _ => {
                        return Err(anyhow!(
                            "expected symbol, found `{:?}, {:?}`",
                            pair.0,
                            pair.1
                        ))
                        .with_context(|| "while defining permutation")
                    }
                }
            }

            ctx.borrow_mut().computation_table.insert_multiple(
                &_tos,
                Computation::Sorted {
                    froms: _froms,
                    tos: _tos.clone(),
                },
            )?;
            Ok(())
        }
        Token::DefAliases(aliases) => aliases.iter().fold(Ok(()), |ax, alias| {
            ax.and(reduce(alias, ctx.clone(), module))
        }),
        Token::Defun(name, args, body) => ctx.borrow_mut().insert_function(
            &Handle::new(&module, name),
            Function {
                handle: Handle::new(&module, name),
                class: FunctionClass::UserDefined(Defined {
                    args: args.to_owned(),
                    body: *body.clone(),
                }),
            },
        ),
        Token::DefAlias(from, to) => ctx
            .borrow_mut()
            .insert_alias(&Handle::new(&module, from), &Handle::new(&module, to))
            .with_context(|| anyhow!("defining {} -> {}", from, to)),
        Token::DefunAlias(from, to) => ctx
            .borrow_mut()
            .insert_funalias(&Handle::new(&module, from), &Handle::new(&module, to))
            .with_context(|| anyhow!("defining {} -> {}", from, to)),
    }
}

pub fn pass(ast: &Ast, ctx: Rc<RefCell<SymbolTable>>) -> Result<()> {
    let mut module = String::from(super::MAIN_MODULE);
    for e in ast.exprs.iter() {
        reduce(e, ctx.clone(), &mut module)?;
    }

    Ok(())
}
