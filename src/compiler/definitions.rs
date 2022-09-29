use eyre::*;
use log::*;
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

    pub fn symbols(&self) -> impl Iterator<Item = (&String, &String, &(Symbol, Type))> {
        self.symbols
            .iter()
            .flat_map(|(module, m)| m.iter().map(move |(name, symbol)| (module, name, symbol)))
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

    pub fn insert_symbol(&mut self, module: &str, symbol: &str, e: Expression) -> Result<()> {
        let t = e.t();
        if self
            .symbols
            .entry(module.into())
            .or_default()
            .contains_key(symbol)
        {
            Err(eyre!(
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
            Err(eyre!("function `{}` already defined", &f.name))
        } else {
            self.funcs.insert(f.name.clone(), f);
            Ok(())
        }
    }

    pub fn insert_alias(&mut self, module: &str, from: &str, to: &str) -> Result<()> {
        if self.symbols.contains_key(from) {
            Err(eyre!("`{}` already exists", from))
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
            Err(eyre!(
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
            Err(eyre!("`{}` already exists", name))
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
                    .insert_constant(module, name, value.to_owned())?;
            }
            Ok(())
        }
        Token::DefColumns(cols) => cols
            .iter()
            .fold(Ok(()), |ax, col| ax.and(reduce(col, ctx.clone(), module))),
        Token::DefColumn(col, t, kind) => {
            ctx.borrow_mut().insert_symbol(
                module,
                col,
                Expression::Column(
                    module.to_owned(),
                    col.to_owned(),
                    *t,
                    // Convert Kind<AstNode> to Kind<Expression>
                    match kind {
                        Kind::Atomic => Kind::Atomic,
                        Kind::Composite(_) => Kind::Atomic, // The actual expression is computed by the generator
                        Kind::Interleaved(xs) => Kind::Interleaved(xs.clone()),
                    },
                ),
            )
        }
        Token::DefArrayColumn(col, range, t) => {
            ctx.borrow_mut().insert_symbol(
                module,
                col,
                Expression::ArrayColumn(module.clone(), col.to_owned(), range.to_owned(), *t),
            )?;
            for i in range {
                let column_name = format!("{}_{}", col, i);
                ctx.borrow_mut().insert_symbol(
                    module,
                    &column_name,
                    Expression::Column(module.clone(), column_name.to_owned(), *t, Kind::Atomic),
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
            // if sorters.is_empty() {
            //     warn!("empty sorter found in `{}`", e.src.as_str());
            // }

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
                        _froms.push(from.to_owned());
                        _tos.push(to.to_owned());
                        ctx.borrow_mut()
                            .resolve_symbol(module, from)
                            .with_context(|| "while defining permutation")?;
                        ctx.borrow_mut()
                            .insert_symbol(
                                module,
                                to,
                                Expression::Column(
                                    module.to_owned(),
                                    to.to_owned(),
                                    Type::Numeric,
                                    Kind::Atomic,
                                ),
                            )
                            .unwrap_or_else(|e| warn!("while defining permutation: {}", e));
                    }
                    _ => {
                        return Err(eyre!("expected symbol, found `{:?}, {:?}`", pair.0, pair.1))
                            .with_context(|| "while defining permutation")
                    }
                }
            }
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
