use super::{generator::Function, Expression, Magma, Node, Type};
use crate::{
    column::Computation,
    compiler::{generator::FunctionClass, Builtin, Form, Intrinsic},
    errors::symbols,
    structs::Handle,
};
use anyhow::*;
use colored::Colorize;
use log::*;
use num_bigint::BigInt;
use num_traits::{One, Zero};
use pairing_ce::ff::PrimeField;
use serde::{Deserialize, Serialize};
use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    rc::{Rc, Weak},
};

lazy_static::lazy_static! {
    pub static ref BUILTINS: HashMap<&'static str, Function> = maplit::hashmap!{
        // forms
        "for" => Function {
            handle: Handle::new(super::MAIN_MODULE, "for"),
            class: FunctionClass::Form(Form::For),
        },
        "debug" => Function {
            handle: Handle::new(super::MAIN_MODULE, "debug"),
            class: FunctionClass::Form(Form::Debug),
        },
        "todo" => Function {
            handle: Handle::new(super::MAIN_MODULE, "todo"),
            class: FunctionClass::Form(Form::Todo),
        },
        "let" => Function {
            handle: Handle::new(super::MAIN_MODULE, "let"),
            class: FunctionClass::Form(Form::Let),
        },

        // special functions
        "nth" => Function {
            handle: Handle::new(super::MAIN_MODULE, "nth"),
            class: FunctionClass::Intrinsic(Intrinsic::Nth),
        },
        "len" => Function {
            handle: Handle::new(super::MAIN_MODULE, Builtin::Len.to_string()),
            class: FunctionClass::Builtin(Builtin::Len),
        },

        // monadic
        "inv" => Function {
            handle: Handle::new(super::MAIN_MODULE, "inv"),
            class: FunctionClass::Intrinsic(Intrinsic::Inv)
        },
        "neg" => Function {
            handle: Handle::new(super::MAIN_MODULE, "neg"),
            class: FunctionClass::Intrinsic(Intrinsic::Neg)
        },
        "not" => Function {
            handle: Handle::new(super::MAIN_MODULE, "not"),
            class: FunctionClass::Intrinsic(Intrinsic::Not),
        },

        // Dyadic
        "eq" => Function{
            handle: Handle::new(super::MAIN_MODULE, "eq"),
            class: FunctionClass::Intrinsic(Intrinsic::Eq),
        },
        "shift" => Function{
            handle: Handle::new(super::MAIN_MODULE, "shift"),
            class: FunctionClass::Intrinsic(Intrinsic::Shift),
        },


        // polyadic
        "+" => Function {
            handle: Handle::new(super::MAIN_MODULE, "+"),
            class: FunctionClass::Intrinsic(Intrinsic::Add)
        },
        "*" => Function {
            handle: Handle::new(super::MAIN_MODULE, "*"),
            class: FunctionClass::Intrinsic(Intrinsic::Mul)
        },
        "^" => Function {
            handle: Handle::new(super::MAIN_MODULE, "^"),
            class: FunctionClass::Intrinsic(Intrinsic::Exp)
        },
        "-" => Function {
            handle: Handle::new(super::MAIN_MODULE, "-"),
            class: FunctionClass::Intrinsic(Intrinsic::Sub)
        },

        "begin" => Function{
            handle: Handle::new(super::MAIN_MODULE, "begin"),
            class: FunctionClass::Intrinsic(Intrinsic::Begin)
        },

        "if-zero" => Function {
            handle: Handle::new(super::MAIN_MODULE, "if-zero"),
            class: FunctionClass::Intrinsic(Intrinsic::IfZero)
        },
        "if-not-zero" => Function {
            handle: Handle::new(super::MAIN_MODULE, "if-not-zero"),
            class: FunctionClass::Intrinsic(Intrinsic::IfNotZero)
        },

        "force-bool" => Function {
            handle:Handle::new(super::MAIN_MODULE, "force-bool"),
            class: FunctionClass::Builtin(Builtin::ForceBool),
        },
        "reduce" => Function {
            handle: Handle::new(super::MAIN_MODULE, "reduce"),
            class: FunctionClass::Form(Form::Reduce)
        },
    };
}

#[derive(Default, Debug, Clone, Serialize, Deserialize)]
pub struct ComputationTable {
    dependencies: HashMap<Handle, usize>,
    computations: Vec<Computation>,
}
impl ComputationTable {
    pub fn update_ids(&mut self, set_id: &dyn Fn(&mut Handle)) {
        self.computations
            .iter_mut()
            .for_each(|x| x.add_id_to_handles(set_id));
    }
    pub fn get(&self, i: usize) -> Option<&Computation> {
        self.computations.get(i)
    }
    pub fn iter(&'_ self) -> impl Iterator<Item = &'_ Computation> {
        self.computations.iter()
    }
    pub fn insert(&mut self, target: &Handle, computation: Computation) -> Result<()> {
        if self.dependencies.contains_key(target) {
            bail!("`{}` already present as a computation target", target);
        }
        self.computations.push(computation);
        self.dependencies
            .insert(target.to_owned(), self.computations.len() - 1);
        Ok(())
    }
    pub fn insert_many(&mut self, targets: &[Handle], computation: Computation) -> Result<()> {
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
    pub fn computation_idx_for(&self, target: &Handle) -> Option<usize> {
        self.dependencies
            .iter()
            .find(|(k, _)| *k == target)
            .map(|x| *x.1)
    }
}
#[derive(Debug, Clone)]
pub enum Symbol {
    Alias(String),
    Final(Node, bool),
}
#[derive(Debug)]
pub struct SymbolTable {
    // The parent relationship is only used for contextual
    // semantics (i.e. for & functions), not modules
    closed: bool,
    // If true, then those are module definitions.
    // Otherwise, this table is a private table, e.g. function arguments
    public: bool,
    pub module: String,
    pub pretty_name: String,
    parent: Weak<RefCell<Self>>,
    children: HashMap<String, Rc<RefCell<SymbolTable>>>,
    constraints: HashSet<String>,
    funcs: HashMap<String, Function>,
    symbols: HashMap<String, Symbol>,
    pub computation_table: Rc<RefCell<ComputationTable>>,
}
impl SymbolTable {
    pub fn new_root() -> SymbolTable {
        SymbolTable {
            closed: true,
            public: true,
            module: super::MAIN_MODULE.to_owned(),
            pretty_name: "".into(),
            parent: Weak::new(),
            children: Default::default(),
            constraints: Default::default(),
            funcs: BUILTINS
                .iter()
                .map(|(k, f)| (k.to_string(), f.clone()))
                .collect(),
            symbols: Default::default(),
            computation_table: Rc::new(RefCell::new(Default::default())),
        }
    }

    pub fn derived(
        parent: Rc<RefCell<Self>>,
        name: &str,
        pretty_name: &str,
        closed: bool,
        public: bool,
    ) -> Rc<RefCell<Self>> {
        let ct = parent.borrow().computation_table.clone();
        parent
            .borrow_mut()
            .children
            .entry(name.to_string())
            .or_insert_with(|| {
                Rc::new(RefCell::new(SymbolTable {
                    closed,
                    public,
                    module: name.to_owned(),
                    pretty_name: pretty_name.to_owned(),
                    parent: Rc::downgrade(&parent),
                    children: Default::default(),
                    constraints: Default::default(),
                    funcs: Default::default(),
                    symbols: Default::default(),
                    computation_table: ct,
                }))
            })
            .clone()
    }

    pub fn visit_mut<T>(
        &mut self,
        f: &mut dyn FnMut(&str, Handle, &mut Symbol) -> Result<()>,
    ) -> Result<()> {
        for (module, handle, symbol) in self
            .symbols
            .iter_mut()
            .map(|(k, v)| (&self.pretty_name, Handle::new(&self.module, k), v))
        {
            f(module, handle, symbol)?;
        }
        for c in self.children.values_mut() {
            let public = c.borrow().public;
            if public {
                c.borrow_mut().visit_mut::<T>(f)?;
            }
        }
        Ok(())
    }

    fn _resolve_symbol(
        &mut self,
        name: &str,
        ax: &mut HashSet<String>,
        absolute_path: bool,
        pure: bool,
    ) -> Result<Node> {
        if ax.contains(name) {
            bail!("circular definition found for {}", name)
        } else {
            ax.insert(name.to_owned());
            // Ugly, but required for borrowing reasons
            if let Some(Symbol::Alias(target)) = self.symbols.get(name).cloned() {
                self._resolve_symbol(&target, ax, absolute_path, pure)
            } else {
                match self.symbols.get_mut(name) {
                    Some(Symbol::Final(exp, visited)) => {
                        if pure && !matches!(exp.e(), Expression::Const(..)) {
                            bail!(
                                "symbol {} can not be used in a pure context",
                                exp.to_string().blue()
                            )
                        } else {
                            *visited = true;
                            Ok(exp.clone())
                        }
                    }
                    None => {
                        if absolute_path {
                            Err(anyhow!(symbols::Error::SymbolNotFound(
                                name.to_owned(),
                                self.module.to_owned()
                            )))
                        } else {
                            self.parent.upgrade().map_or(
                                Err(anyhow!(symbols::Error::SymbolNotFound(
                                    name.to_owned(),
                                    self.module.to_owned(),
                                ))),
                                |parent| {
                                    parent.borrow_mut()._resolve_symbol(
                                        name,
                                        &mut HashSet::new(),
                                        false,
                                        self.closed || pure,
                                    )
                                },
                            )
                        }
                    }
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
            Err(anyhow!(symbols::Error::CircularDefinition(name.to_owned())))
        } else {
            ax.insert(name.to_owned());
            // Ugly, but required for borrowing reasons
            if let Some(Symbol::Alias(_)) = self.symbols.get(name).cloned() {
                self._edit_symbol(name, f, ax)
            } else {
                match self.symbols.get_mut(name) {
                    Some(Symbol::Final(constraint, _)) => {
                        f(constraint.e_mut());
                        Ok(())
                    }
                    None => self.parent.upgrade().map_or(
                        Err(anyhow!(symbols::Error::SymbolNotFound(
                            name.to_owned(),
                            self.module.to_owned(),
                        ))),
                        |parent| parent.borrow_mut().edit_symbol(name, f),
                    ),
                    _ => unimplemented!(),
                }
            }
        }
    }

    fn _resolve_function(&self, name: &str, ax: &mut HashSet<String>) -> Result<Function> {
        if ax.contains(name) {
            bail!(symbols::Error::CircularDefinition(name.to_owned()))
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
        if self.constraints.insert(name.to_owned()) {
            Ok(())
        } else {
            bail!("Constraint `{}` already defined", name)
        }
    }

    pub fn insert_symbol(&mut self, name: &str, e: Node) -> Result<()> {
        if self.symbols.contains_key(name) {
            bail!(symbols::Error::SymbolAlreadyExists(
                name.to_owned(),
                self.module.to_owned()
            ))
        } else {
            self.symbols
                .insert(name.to_owned(), Symbol::Final(e, false));
            Ok(())
        }
    }

    pub fn insert_function(&mut self, name: &str, f: Function) -> Result<()> {
        if self.funcs.contains_key(name) {
            bail!(symbols::Error::FunctionAlreadyExists(
                name.to_owned(),
                self.module.to_owned()
            ))
        } else {
            self.funcs.insert(name.to_owned(), f);
            Ok(())
        }
    }

    pub fn insert_alias(&mut self, from: &str, to: &str) -> Result<()> {
        if self.symbols.contains_key(from) {
            bail!(symbols::Error::SymbolAlreadyExists(
                from.to_owned(),
                self.module.to_owned()
            ))
        } else {
            self.symbols
                .insert(from.to_owned(), Symbol::Alias(to.to_owned()));
            Ok(())
        }
    }

    pub fn insert_funalias(&mut self, from: &str, to: &str) -> Result<()> {
        if self.funcs.contains_key(from) {
            bail!(symbols::Error::AliasAlreadyExists(
                from.to_owned(),
                to.to_owned()
            ))
        } else {
            self.funcs.insert(
                from.to_owned(),
                Function {
                    handle: Handle::new(&self.module, to),
                    class: FunctionClass::Alias(to.to_string()),
                },
            );
            Ok(())
        }
    }

    pub fn resolve_symbol(&mut self, name: &str) -> Result<Node> {
        if name.contains('.') {
            self.resolve_symbol_with_path(name)
        } else {
            self._resolve_symbol(name, &mut HashSet::new(), false, false)
        }
    }

    pub fn edit_symbol(&mut self, name: &str, f: &dyn Fn(&mut Expression)) -> Result<()> {
        self._edit_symbol(name, f, &mut HashSet::new())
    }

    pub fn resolve_function(&self, name: &str) -> Result<Function> {
        self._resolve_function(name, &mut HashSet::new())
    }

    pub fn insert_constant(&mut self, name: &str, value: BigInt, replace: bool) -> Result<()> {
        let t = if Zero::is_zero(&value) || One::is_one(&value) {
            Type::Scalar(Magma::Boolean)
        } else {
            Type::Scalar(Magma::Integer)
        };
        if self.symbols.contains_key(name) && !replace {
            bail!(symbols::Error::SymbolAlreadyExists(
                name.to_owned(),
                self.module.to_owned()
            ))
        } else if let Some(fr) = pairing_ce::bn256::Fr::from_str(&value.to_string()) {
            self.symbols.insert(
                name.to_owned(),
                Symbol::Final(
                    Node {
                        _e: Expression::Const(value, Some(fr)),
                        _t: Some(t),
                    },
                    false,
                ),
            );
            Ok(())
        } else {
            bail!("{} is not an Fr element", value.to_string().red().bold())
        }
    }

    fn resolve_symbol_with_path(&mut self, name: &str) -> Result<Node> {
        let mut components = name.split('.');
        let module = components.next().unwrap();
        let name = components
            .next()
            .with_context(|| anyhow!("missing name in {}", name))?;
        self._resolve_symbol_with_path(&module, &name)
    }

    fn _resolve_symbol_with_path(&mut self, module: &str, name: &str) -> Result<Node> {
        if self.module == module {
            self.resolve_symbol(name)
        } else {
            self.parent.upgrade().map_or_else(
                || self._resolve_down_symbol_with_path(module, name),
                |parent| parent.borrow_mut()._resolve_symbol_with_path(module, name),
            )
        }
    }

    fn _resolve_down_symbol_with_path<'a>(&mut self, module: &str, name: &str) -> Result<Node> {
        if let Some(submodule) = self.children.get_mut(module) {
            submodule.borrow_mut().resolve_symbol(name)
        } else {
            bail!(symbols::Error::ModuleNotFound(
                name.to_owned(),
                self.module.to_owned(),
            ))
        }
    }
}
