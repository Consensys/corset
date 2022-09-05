use eyre::*;
use std::collections::{HashMap, HashSet};
use std::cell::RefCell;
use std::rc::Rc;

use crate::utils::*;
use crate::compiler::generator::*;

lazy_static::lazy_static! {
    static ref BUILTINS: HashMap<&'static str, Function> = maplit::hashmap!{
        "defconstraint" => Function {
            name: "defconstring".into(),
            class: FunctionClass::SpecialForm(Form::Defconstraint),
        },

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

        "defconst" => Function {
            name: "defconst".into(),
            class: FunctionClass::SpecialForm(Form::Defconst),
        },

        "nth" => Function {
            name: "nth".into(),
            class: FunctionClass::Builtin(Builtin::Nth),
        },

        "for" => Function {
            name: "for".into(),
            class: FunctionClass::SpecialForm(Form::For),
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

        "begin" => Function{name: "begin".into(), class: FunctionClass::Builtin(Builtin::Begin)},

        // Special form for now, see later if implementing map...
        "branch-if-zero" => Function {
            name:"branch-if-zero".into(),
            class: FunctionClass::Builtin(Builtin::BranchIfZero)
        },

        "branch-if-zero-else" => Function {
            name:"branch-if-zero-else".into(),
            class: FunctionClass::Builtin(Builtin::BranchIfZeroElse)
        },

        "branch-if-not-zero" => Function {
            name:"branch-if-not-zero".into(),
            class: FunctionClass::Builtin(Builtin::BranchIfNotZero)
        },

        "branch-if-not-zero-else" => Function {
            name:"branch-if-not-zero-else".into(),
            class: FunctionClass::Builtin(Builtin::BranchIfNotZeroElse)
        },
    };
}

#[derive(Debug)]
pub enum Symbol {
    Alias(String),
    Final(Constraint),
}
#[derive(Debug)]
pub struct SymbolTable {
    parent: Option<Rc<RefCell<SymbolTable>>>,
    constraints: HashSet<String>,
    funcs: HashMap<String, Function>,
    symbols: HashMap<String, Symbol>,
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

    fn _resolve_symbol(&self, name: &str, ax: &mut HashSet<String>) -> Result<Constraint> {
        if ax.contains(name) {
            Err(eyre!("Circular definitions found for {}", name))
        } else {
            ax.insert(name.into());
            match self.symbols.get(name) {
                Some(Symbol::Alias(name)) => self._resolve_symbol(name, ax),
                Some(Symbol::Final(constraint)) => Ok(constraint.clone()),
                None => self
                    .parent
                    .as_ref()
                    .map_or(Err(eyre!("Column `{}` unknown", name)), |parent| {
                        parent.borrow().resolve_symbol(name)
                    }),
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
                    class: FunctionClass::Alias(ref name),
                    ..
                }) => self._resolve_function(name, ax),
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

    pub fn insert_symbol(&mut self, symbol: &str, constraint: Constraint) -> Result<()> {
        if self.symbols.contains_key(symbol) {
            Err(anyhow!("column `{}` already exists", symbol))
        } else {
            self.symbols
                .insert(symbol.into(), Symbol::Final(constraint));
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
            self.symbols.insert(from.into(), Symbol::Alias(to.into()));
            Ok(())
        }
    }

    pub fn insert_funalias(&mut self, from: &str, to: &str) -> Result<()> {
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

    pub fn resolve_symbol(&self, name: &str) -> Result<Constraint> {
        self._resolve_symbol(name, &mut HashSet::new())
    }

    pub fn resolve_function(&self, name: &str) -> Result<Function> {
        self._resolve_function(name, &mut HashSet::new())
    }

    pub fn insert_constant(&mut self, name: &str, value: i32) -> Result<()> {
        if self.symbols.contains_key(name) {
            Err(anyhow!("`{}` already exists", name))
        } else {
            self.symbols
                .insert(name.into(), Symbol::Final(Constraint::Const(value)));
            Ok(())
        }
    }
}

pub fn parse(table: Rc<RefCell<SymbolTable>>) -> Result<()> {
    Ok(())
}
