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
use std::rc::{Rc, Weak};

use super::common::BUILTINS;
use super::generator::{Defined, Expression, Function, FunctionClass};
use super::{Handle, Magma, Type};
use crate::column::Computation;
use crate::compiler::parser::*;

#[derive(Debug, Clone)]
pub enum Symbol {
    Alias(String),
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
    pub name: String,
    parent: Weak<RefCell<Self>>,
    children: HashMap<String, Rc<RefCell<SymbolTable>>>,
    constraints: HashSet<String>,
    funcs: HashMap<String, Function>,
    symbols: HashMap<String, (Symbol, Type)>,
    pub computation_table: ComputationTable,
}
impl SymbolTable {
    pub fn new_root() -> SymbolTable {
        SymbolTable {
            name: super::MAIN_MODULE.to_owned(),
            parent: Weak::new(),
            children: Default::default(),
            constraints: Default::default(),
            funcs: BUILTINS
                .iter()
                .map(|(k, f)| (k.to_string(), f.clone()))
                .collect(),
            symbols: Default::default(),
            computation_table: Default::default(),
        }
    }

    pub fn derived(parent: Rc<RefCell<Self>>, name: &str) -> Rc<RefCell<Self>> {
        parent
            .borrow_mut()
            .children
            .entry(name.to_string())
            .or_insert_with(|| {
                Rc::new(RefCell::new(SymbolTable {
                    name: name.to_owned(),
                    parent: Rc::downgrade(&parent),
                    children: Default::default(),
                    constraints: Default::default(),
                    funcs: Default::default(),
                    symbols: Default::default(),
                    computation_table: Default::default(),
                }))
            })
            .clone()
    }

    pub fn visit_mut<T>(
        &mut self,
        f: &mut dyn FnMut((Handle, &mut (Symbol, Type))) -> Result<()>,
    ) -> Result<()> {
        for s in self
            .symbols
            .iter_mut()
            .map(|(k, v)| (Handle::new(&self.name, k), v))
        {
            f(s)?;
        }
        for c in self.children.values_mut() {
            c.borrow_mut().visit_mut::<T>(f)?;
        }
        Ok(())
    }

    fn _resolve_symbol(
        &mut self,
        name: &str,
        ax: &mut HashSet<String>,
    ) -> Result<(Expression, Type)> {
        if ax.contains(name) {
            Err(anyhow!("Circular definitions found for {}", name))
        } else {
            ax.insert(name.to_owned());
            // Ugly, but required for borrowing reasons
            if let Some((Symbol::Alias(target), _)) = self.symbols.get(name).cloned() {
                self._resolve_symbol(&target, ax)
            } else {
                match self.symbols.get_mut(name) {
                    Some((Symbol::Final(constraint, visited), t)) => {
                        *visited = true;
                        Ok((constraint.clone(), *t))
                    }
                    None => self.parent.upgrade().map_or(
                        Err(anyhow!(
                            "column `{}` unknown in module `{}`",
                            name.red(),
                            self.name.blue()
                        )),
                        |parent| parent.borrow_mut().resolve_symbol(name),
                    ),
                    _ => unimplemented!(),
                }
            }
        }
    }

    fn _edit_symbol(
        &mut self,
        name: &str,
        f: &dyn Fn(&mut Expression),
        ax: &mut HashSet<String>,
    ) -> Result<()> {
        if ax.contains(name) {
            Err(anyhow!(
                "Circular definitions found for {}",
                name.to_string().red()
            ))
        } else {
            ax.insert(name.to_owned());
            // Ugly, but required for borrowing reasons
            if let Some((Symbol::Alias(_), _)) = self.symbols.get(name).cloned() {
                self._edit_symbol(name, f, ax)
            } else {
                match self.symbols.get_mut(name) {
                    Some((Symbol::Final(constraint, _), _)) => {
                        f(constraint);
                        Ok(())
                    }
                    None => self.parent.upgrade().map_or(
                        Err(anyhow!(
                            "column `{}` unknown in module `{}`",
                            name.red(),
                            self.name.blue()
                        )),
                        |parent| parent.borrow_mut().edit_symbol(name, f),
                    ),
                    _ => unimplemented!(),
                }
            }
        }
    }

    fn _resolve_function(&self, name: &str, ax: &mut HashSet<String>) -> Result<Function> {
        if ax.contains(name) {
            Err(anyhow!(
                "Circular definitions found for {}",
                name.to_string().red()
            ))
        } else {
            ax.insert(name.to_owned());
            match self.funcs.get(name) {
                Some(Function {
                    class: FunctionClass::Alias(ref to),
                    ..
                }) => self.resolve_function(to),
                Some(f) => Ok(f.to_owned()),
                None => self
                    .parent
                    .upgrade()
                    .map_or(Err(anyhow!("function {} unknown", name.red())), |parent| {
                        parent.borrow().resolve_function(name)
                    }),
            }
        }
    }

    pub fn insert_constraint(&mut self, name: &str) -> Result<()> {
        if self.constraints.contains(name) {
            warn!("redefining constraint `{}`", name.yellow());
        }
        self.constraints
            .insert(name.to_owned())
            .then(|| ())
            .ok_or_else(|| anyhow!("Constraint `{}` already defined", name))
    }

    pub fn insert_symbol(&mut self, name: &str, e: Expression) -> Result<()> {
        let t = e.t();
        if self.symbols.contains_key(name) {
            Err(anyhow!(
                "column `{}` already exists in module `{}`",
                name.red(),
                self.name.blue()
            ))
        } else {
            self.symbols
                .insert(name.to_owned(), (Symbol::Final(e, false), t));
            Ok(())
        }
    }

    pub fn insert_function(&mut self, name: &str, f: Function) -> Result<()> {
        if self.funcs.contains_key(name) {
            Err(anyhow!(
                "function {} already defined",
                name.to_string().red()
            ))
        } else {
            self.funcs.insert(name.to_owned(), f);
            Ok(())
        }
    }

    pub fn insert_alias(&mut self, from: &str, to: &str) -> Result<()> {
        if self.symbols.contains_key(from) {
            Err(anyhow!("`{}` already exists", from))
        } else {
            self.symbols
                .insert(from.to_owned(), (Symbol::Alias(to.to_owned()), Type::Void));
            Ok(())
        }
    }

    pub fn insert_funalias(&mut self, from: &str, to: &str) -> Result<()> {
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
                    handle: Handle::new(&self.name, to),
                    class: FunctionClass::Alias(to.to_string()),
                },
            );
            Ok(())
        }
    }

    pub fn resolve_symbol(&mut self, name: &str) -> Result<(Expression, Type)> {
        self._resolve_symbol(name, &mut HashSet::new())
    }

    pub fn edit_symbol(&mut self, name: &str, f: &dyn Fn(&mut Expression)) -> Result<()> {
        self._edit_symbol(name, f, &mut HashSet::new())
    }

    pub fn resolve_function(&self, name: &str) -> Result<Function> {
        self._resolve_function(name, &mut HashSet::new())
    }

    pub fn insert_constant(&mut self, name: &str, value: BigInt) -> Result<()> {
        let t = if Zero::is_zero(&value) || One::is_one(&value) {
            Type::Scalar(Magma::Boolean)
        } else {
            Type::Scalar(Magma::Integer)
        };
        if self.symbols.contains_key(name) {
            Err(anyhow!(
                "`{}` already exists in `{}`",
                name.red(),
                self.name.blue()
            ))
        } else {
            self.symbols.insert(
                name.to_owned(),
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

fn reduce(
    e: &AstNode,
    root_ctx: Rc<RefCell<SymbolTable>>,
    ctx: &mut Rc<RefCell<SymbolTable>>,
) -> Result<()> {
    match &e.class {
        Token::Value(_)
        | Token::Symbol(_)
        | Token::Keyword(_)
        | Token::List(_)
        | Token::Range(_)
        | Token::Type(_)
        | Token::DefPlookup(..)
        | Token::DefInrange(..) => Ok(()),

        Token::DefConstraint(name, ..) => ctx.borrow_mut().insert_constraint(name),
        Token::DefModule(name) => {
            *ctx = SymbolTable::derived(root_ctx, name);
            Ok(())
        }
        Token::DefConsts(cs) => {
            for (name, value) in cs.iter() {
                ctx.borrow_mut().insert_constant(name, value.to_owned())?;
            }
            Ok(())
        }
        Token::DefColumns(cols) => cols
            .iter()
            .fold(Ok(()), |ax, col| ax.and(reduce(col, root_ctx.clone(), ctx))),
        Token::DefColumn(col, t, kind) => {
            let module_name = ctx.borrow().name.to_owned();
            ctx.borrow_mut().insert_symbol(
                col,
                Expression::Column(
                    Handle::new(&module_name, col),
                    *t,
                    // Convert Kind<AstNode> to Kind<Expression>
                    match kind {
                        Kind::Atomic => Kind::Atomic,
                        Kind::Phantom => Kind::Phantom,
                        Kind::Composite(_) => Kind::Phantom, // The actual expression is computed by the generator
                        Kind::Interleaved(xs) => Kind::Interleaved(
                            xs.iter()
                                .map(|h| Handle::new(&module_name, &h.name))
                                .collect(),
                        ),
                    },
                ),
            )
        }
        Token::DefArrayColumn(col, range, t) => {
            let handle = Handle::new(&ctx.borrow().name, col);
            ctx.borrow_mut()
                .insert_symbol(col, Expression::ArrayColumn(handle, range.to_owned(), *t))?;
            for i in range {
                let column_name = format!("{}_{}", col, i);
                let handle = Handle::new(&ctx.borrow().name, &column_name);
                ctx.borrow_mut().insert_symbol(
                    &column_name,
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
                        let from_handle = Handle::new(&ctx.borrow().name, &from);
                        let to_handle = Handle::new(&ctx.borrow().name, &to);
                        ctx.borrow_mut()
                            .resolve_symbol(from)
                            .with_context(|| "while defining permutation")?;
                        ctx.borrow_mut()
                            .insert_symbol(
                                to,
                                Expression::Column(
                                    to_handle.clone(),
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
            ax.and(reduce(alias, root_ctx.clone(), ctx))
        }),
        Token::Defun(name, args, body) => {
            let module_name = ctx.borrow().name.to_owned();
            ctx.borrow_mut().insert_function(
                name,
                Function {
                    handle: Handle::new(&module_name, name),
                    class: FunctionClass::UserDefined(Defined {
                        args: args.to_owned(),
                        body: *body.clone(),
                    }),
                },
            )
        }
        Token::DefAlias(from, to) => ctx
            .borrow_mut()
            .insert_alias(from, to)
            .with_context(|| anyhow!("defining {} -> {}", from, to)),
        Token::DefunAlias(from, to) => ctx
            .borrow_mut()
            .insert_funalias(from, to)
            .with_context(|| anyhow!("defining {} -> {}", from, to)),
    }
}

pub fn pass(ast: &Ast, ctx: Rc<RefCell<SymbolTable>>) -> Result<()> {
    let mut current_ctx = ctx.clone();
    for e in ast.exprs.iter() {
        reduce(e, ctx.clone(), &mut current_ctx)?;
    }

    Ok(())
}
