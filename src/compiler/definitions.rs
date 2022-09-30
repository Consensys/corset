use eyre::*;
use log::*;
use num_bigint::BigInt;
use num_traits::{One, Zero};
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

use super::common::BUILTINS;
use super::generator::{Defined, Expression, Function, FunctionClass};
use super::{Handle, Type};
use crate::column::Computation;
use crate::compiler::parser::*;

#[derive(Debug, Clone)]
pub enum Symbol {
    Alias(Handle),
    Final(Expression, bool),
}
#[derive(Default, Debug, Clone)]
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
            return Err(eyre!(
                "`{}` already present as a computation target",
                target
            ));
        }
        self.computations.push(computation);
        self.dependencies
            .insert(target.to_owned(), self.computations.len() - 1);
        Ok(())
    }
}
#[derive(Debug)]
pub struct SymbolTable {
    parent: Option<Rc<RefCell<SymbolTable>>>,
    constraints: HashMap<String, HashSet<String>>,
    funcs: HashMap<String, Function>,
    symbols: HashMap<Handle, (Symbol, Type)>,
    pub computation_table: ComputationTable,
}
impl SymbolTable {
    pub fn new_root() -> SymbolTable {
        SymbolTable {
            parent: None,
            constraints: Default::default(),
            funcs: BUILTINS
                .iter()
                .map(|(k, v)| (k.to_string(), v.clone()))
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

    pub fn symbols(&self) -> impl Iterator<Item = (&Handle, &(Symbol, Type))> {
        self.symbols.iter()
    }

    fn _resolve_symbol(
        &mut self,
        handle: &Handle,
        ax: &mut HashSet<Handle>,
    ) -> Result<(Expression, Type)> {
        if ax.contains(handle) {
            Err(eyre!("Circular definitions found for {}", handle))
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
                        Err(eyre!(
                            "Column `{}` unknown in module `{}`",
                            handle.name,
                            handle.module
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
            Err(eyre!("Circular definitions found for {}", handle))
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
                        Err(eyre!(
                            "Column `{}` unknown in module `{}`",
                            handle.name,
                            handle.module
                        )),
                        |parent| parent.borrow_mut().edit_symbol(handle, f),
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
            warn!("redefining constraint `{}`", name);
        }
        self.constraints
            .entry(module.into())
            .or_default()
            .insert(name.to_owned());
        Ok(())
        // .then(|| ())
        // .ok_or_else(|| eyre!("Constraint `{}` already defined", name))
    }

    pub fn insert_symbol(&mut self, handle: &Handle, e: Expression) -> Result<()> {
        let t = e.t();
        if self.symbols.contains_key(handle) {
            Err(eyre!(
                "column `{}` already exists in module `{}`",
                handle.name,
                handle.module
            ))
        } else {
            self.symbols
                .insert(handle.to_owned(), (Symbol::Final(e, false), t));
            Ok(())
        }
    }

    pub fn insert_func(&mut self, f: Function) -> Result<()> {
        if self.funcs.contains_key(&f.name) {
            Err(eyre!("function `{}` already defined", &f.name))
        } else {
            self.funcs.insert(f.name.clone(), f);
            Ok(())
        }
    }

    pub fn insert_alias(&mut self, from: &Handle, to: &Handle) -> Result<()> {
        if self.symbols.contains_key(from) {
            Err(eyre!("`{}` already exists", from))
        } else {
            self.symbols
                .insert(from.to_owned(), (Symbol::Alias(to.to_owned()), Type::Void));
            Ok(())
        }
    }

    pub fn insert_funalias(&mut self, from: &str, to: &str) -> Result<()> {
        if self.funcs.contains_key(from) {
            Err(eyre!(
                "`{}` already exists: {} -> {:?}",
                from,
                to,
                self.funcs[from]
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

    pub fn insert_computation(&mut self, targets: &[Handle], c: Computation) {
        for target in targets.iter() {
            self.computation_table.insert(target, c.clone()).unwrap();
        }
    }

    pub fn resolve_symbol(&mut self, handle: &Handle) -> Result<(Expression, Type)> {
        self._resolve_symbol(handle, &mut HashSet::new())
    }

    pub fn edit_symbol(&mut self, handle: &Handle, f: &dyn Fn(&mut Expression)) -> Result<()> {
        self._edit_symbol(handle, f, &mut HashSet::new())
    }

    pub fn resolve_function(&self, name: &str) -> Result<Function> {
        self._resolve_function(name, &mut HashSet::new())
    }

    pub fn insert_constant(&mut self, handle: &Handle, value: BigInt) -> Result<()> {
        let t = if Zero::is_zero(&value) || One::is_one(&value) {
            Type::Boolean
        } else {
            Type::Numeric
        };
        if self.symbols.contains_key(handle) {
            Err(eyre!(
                "`{}` already exists in `{}`",
                handle.name,
                handle.module
            ))
        } else {
            self.symbols.insert(
                handle.to_owned(),
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
        | Token::Keyword(_)
        | Token::List(_)
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
                        Kind::Composite(_) => Kind::Atomic, // The actual expression is computed by the generator
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
                return Err(eyre!(
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
                                    Type::Numeric,
                                    Kind::Atomic,
                                ),
                            )
                            .unwrap_or_else(|e| warn!("while defining permutation: {}", e));
                        _froms.push(from_handle);
                        _tos.push(to_handle);
                    }
                    _ => {
                        return Err(eyre!("expected symbol, found `{:?}, {:?}`", pair.0, pair.1))
                            .with_context(|| "while defining permutation")
                    }
                }
            }
            ctx.borrow_mut().insert_computation(
                &_tos,
                Computation::Sorted {
                    froms: _froms,
                    tos: _tos.clone(),
                },
            );
            Ok(())
        }
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
            .insert_alias(&Handle::new(&module, from), &Handle::new(&module, to))
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
