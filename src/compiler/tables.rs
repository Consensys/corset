use super::{generator::Function, ColumnRef, Expression, Magma, Node, Type};
use crate::{
    column::Computation,
    compiler::{generator::FunctionClass, Builtin, Form, Intrinsic},
    errors::symbols,
    structs::{Handle, PERSPECTIVE_SEPARATOR},
};
use anyhow::*;
use itertools::Itertools;
use log::*;
use num_bigint::BigInt;
use num_traits::{One, Zero};
use owo_colors::OwoColorize;
use pairing_ce::ff::PrimeField;
use serde::{Deserialize, Serialize};
use sorbus::{NodeID, Tree};
use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    rc::Rc,
};

lazy_static::lazy_static! {
    /// This map contains all the special forms, builtin functions and field operations that are directly
    /// implemented by Corset.
    pub static ref BUILTINS: HashMap<&'static str, Function> = maplit::hashmap!{
        // Special forms
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
        "reduce" => Function {
            handle: Handle::new(super::MAIN_MODULE, "reduce"),
            class: FunctionClass::Form(Form::Reduce)
        },

        // Builtin functions
        "len" => Function {
            handle: Handle::new(super::MAIN_MODULE, Builtin::Len.to_string()),
            class: FunctionClass::Builtin(Builtin::Len),
        },

        // Intrinsics
        "inv" => Function {
            handle: Handle::new(super::MAIN_MODULE, "inv"),
            class: FunctionClass::Intrinsic(Intrinsic::Inv)
        },
        "neg" => Function {
            handle: Handle::new(super::MAIN_MODULE, "neg"),
            class: FunctionClass::Intrinsic(Intrinsic::Neg)
        },
        "nth" => Function {
            handle: Handle::new(super::MAIN_MODULE, "nth"),
            class: FunctionClass::Intrinsic(Intrinsic::Nth),
        },
        "shift" => Function{
            handle: Handle::new(super::MAIN_MODULE, "shift"),
            class: FunctionClass::Intrinsic(Intrinsic::Shift),
        },
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
        "if!" => Function {
            handle: Handle::new(super::MAIN_MODULE, "if!"),
            class: FunctionClass::Intrinsic(Intrinsic::IfZero)
        },
        "if" => Function {
            handle: Handle::new(super::MAIN_MODULE, "if"),
            class: FunctionClass::Intrinsic(Intrinsic::IfNotZero)
        },
    };
}

#[derive(Default, Debug, Clone, Serialize, Deserialize)]
pub struct ComputationTable {
    pub(crate) dependencies: HashMap<ColumnRef, usize>,
    pub(crate) computations: Vec<Computation>,
}
impl ComputationTable {
    /// Return, if it exists, the computation of ID `id`.
    pub fn get(&self, id: usize) -> Option<&Computation> {
        self.computations.get(id)
    }

    /// Return, if it exists, the computation of ID `id`.
    pub fn get_mut(&mut self, id: usize) -> Option<&mut Computation> {
        self.computations.get_mut(id)
    }

    /// Iterate over all the defined computations.
    pub fn iter(&'_ self) -> impl Iterator<Item = &'_ Computation> {
        self.computations.iter()
    }

    /// Iterate over mutable references to all the defined computations.
    pub fn iter_mut(&'_ mut self) -> impl Iterator<Item = &'_ mut Computation> {
        self.computations.iter_mut()
    }

    /// Insert the computation defining `target`. Will fail if `target` is already defined by an existing computation.
    pub fn insert(&mut self, target: &ColumnRef, computation: Computation) -> Result<()> {
        if !target.is_id() {
            panic!("computations must be inserted by ID")
        }
        if self.dependencies.contains_key(target) {
            panic!("`{}` already present as a computation target", target);
        }
        self.computations.push(computation);
        self.dependencies
            .insert(target.to_owned(), self.computations.len() - 1);
        Ok(())
    }

    /// Insert a computation defining multiple columns at once (e.g. permutations).
    pub fn insert_many(&mut self, targets: &[ColumnRef], computation: Computation) -> Result<()> {
        self.computations.push(computation);
        for target in targets.iter() {
            self.dependencies
                .insert(target.to_owned(), self.computations.len() - 1);
        }
        Ok(())
    }

    /// Insert a computation defining multiple columns at once (e.g. permutations).
    pub fn add_dependency(&mut self, target: ColumnRef, computation_id: usize) -> Result<()> {
        if computation_id > self.computations.len() {
            bail!("non-existing computation")
        } else {
            self.dependencies.insert(target, computation_id);
            Ok(())
        }
    }

    /// Given a handle, returns, if there is one, the computation defining this column.
    pub fn computation_for(&self, target: &ColumnRef) -> Option<&Computation> {
        self.dependencies
            .iter()
            .find(|(k, _)| *k == target)
            .map(|x| &self.computations[*x.1])
    }

    /// Given a handle, returns, if there is one, the ID of computation defining this column.
    pub fn computation_idx_for(&self, target: &ColumnRef) -> Option<usize> {
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

#[derive(Default)]
pub struct GlobalData {
    computations: ComputationTable,
    pub perspectives: HashMap<String, HashMap<String, Option<Node>>>, // module -> {Perspectives}
}
impl GlobalData {
    pub fn set_perspective_trigger(
        &mut self,
        module: &str,
        perspective: &str,
        _trigger: Node,
    ) -> Result<()> {
        let trigger = self
            .perspectives
            .get_mut(module)
            .with_context(|| anyhow!("module {} has no perspectives", module))?
            .get_mut(perspective)
            .with_context(|| {
                anyhow!("perspective {} not found in module {}", perspective, module)
            })?;
        if trigger.is_some() {
            bail!("trigger already set for {}%{}", module, perspective)
        } else {
            *trigger = Some(_trigger);
            Ok(())
        }
    }

    pub fn get_perspective_trigger(&self, module: &str, perspective: &str) -> Result<Node> {
        self.perspectives
            .get(module)
            .with_context(|| anyhow!("module {} has no perspectives", module))?
            .get(perspective)
            .with_context(|| anyhow!("perspective {} not found in module {}", perspective, module))?
            .clone()
            .ok_or_else(|| anyhow!("perspective {}%{} has no trigger yet", module, perspective))
    }
}

/// All the symbols are stored in a tree.
/// The nodes are the nested symbol tables, one per context.
/// The tree also contains a data repository, that is used to store global data
/// (notably perspectives) as the parsing goes.
type SymbolTableTree = Tree<SymbolTable, GlobalData>;

/// A Scope is conceptually a pointer to a node in the [`SymbolTableTree`], that
/// contains most of the methods used to interact with the tree.
pub struct Scope {
    /// a shared reference to the global tree
    pub tree: Rc<RefCell<SymbolTableTree>>,
    /// the ID of the tree node this [`Scope`] points to
    id: NodeID,
}

/// A helper macro to access to the [`SymbolTable`] pointed by [`Scope`]
/// parameter
macro_rules! data {
    ($i:ident) => {
        $i.tree.clone().borrow()[$i.id].unwrap_data()
    };
}

/// A helper macro to mutably access to the [`SymbolTable`] pointed by [`Scope`]
/// parameter
macro_rules! data_mut {
    ($i:ident) => {
        $i.tree.clone().borrow_mut()[$i.id].unwrap_data_mut()
    };
}

impl Scope {
    pub fn new() -> Scope {
        let mut tree = Tree::new();
        let root = tree.add_node(
            None,
            Some(SymbolTable {
                name: super::MAIN_MODULE.to_owned(),
                module: super::MAIN_MODULE.to_owned(),
                perspective: None,
                closed: true,
                public: true,
                global: false,
                constraints: Default::default(),
                funcs: BUILTINS
                    .iter()
                    .map(|(k, f)| (k.to_string(), f.clone()))
                    .collect(),
                symbols: Default::default(),
            }),
        );
        tree.set_root(root);

        Scope {
            tree: Rc::new(RefCell::new(tree)),
            id: root,
        }
    }

    pub fn derive(&mut self, name: &str) -> Result<Scope> {
        let maybe_child = self.tree.borrow().find_child(self.id, |n| n.name == name);
        match maybe_child {
            Some(_) => {
                bail!("symbol table {} already exists in {}", name, self.name());
            }
            None => {
                let module = self.module();
                let current_global = data!(self).global;
                let new_node = self.tree.borrow_mut().add_node(
                    Some(self.id),
                    Some(SymbolTable {
                        name: name.to_owned(),
                        module,
                        closed: false,
                        public: false,
                        global: current_global,
                        constraints: Default::default(),
                        funcs: Default::default(),
                        symbols: Default::default(),
                        perspective: None,
                    }),
                );
                Ok(Scope {
                    tree: self.tree.clone(),
                    id: new_node,
                })
            }
        }
    }

    pub fn switch_to_module(&mut self, name: &str) -> Result<Scope> {
        let root = self.tree.borrow().root();
        let maybe_child = self.tree.borrow().find_child(root, |n| n.name == name);
        match maybe_child {
            Some(n) => Ok(self.at(n)),
            None => {
                let current_global = data!(self).global;
                let new_node = self.tree.borrow_mut().add_node(
                    Some(root),
                    Some(SymbolTable {
                        name: name.to_owned(),
                        module: name.to_owned(),
                        closed: false,
                        public: false,
                        global: current_global,
                        constraints: Default::default(),
                        funcs: Default::default(),
                        symbols: Default::default(),
                        perspective: None,
                    }),
                );
                Ok(Scope {
                    tree: self.tree.clone(),
                    id: new_node,
                })
            }
        }
    }

    pub fn jump_in(&mut self, name: &str) -> Result<Scope> {
        let maybe_child = self.tree.borrow().find_child(self.id, |n| n.name == name);
        match maybe_child {
            Some(n) => Ok(self.at(n)),
            None => {
                bail!(
                    "{} not found in {}",
                    name.bold().red(),
                    self.name().bold().yellow(),
                )
            }
        }
    }

    pub fn closed(self, closed: bool) -> Self {
        data_mut!(self).closed = closed;
        self
    }

    pub fn global(self, global: bool) -> Self {
        data_mut!(self).global = global;
        self
    }

    pub fn public(self, public: bool) -> Self {
        data_mut!(self).public = public;
        self
    }

    pub fn with_perspective(self, perspective: &str) -> Result<Self> {
        let module = self.module();
        let perspective_already_exists = self
            .tree
            .borrow_mut()
            .metadata_mut()
            .perspectives
            .entry(module)
            .or_default()
            .insert(perspective.to_owned(), None)
            .is_some();

        if perspective_already_exists {
            bail!(
                "perspective {} already exist in module {}",
                perspective,
                self.module()
            ) // TODO: better error message
        } else {
            data_mut!(self).perspective = Some(perspective.to_owned());
            Ok(self)
        }
    }

    pub fn module(&self) -> String {
        data!(self).module.to_owned()
    }

    pub fn name(&self) -> String {
        data!(self).name.to_owned()
    }

    pub fn perspective(&self) -> Option<String> {
        data!(self).perspective.clone()
    }

    pub fn computations(&self) -> ComputationTable {
        self.tree.borrow().metadata().computations.clone()
    }

    pub fn insert_many_computations(
        &self,
        targets: &[ColumnRef],
        computation: Computation,
    ) -> Result<()> {
        self.tree
            .borrow_mut()
            .metadata_mut()
            .computations
            .insert_many(targets, computation)
    }

    fn at(&self, id: usize) -> Scope {
        Scope {
            tree: self.tree.clone(),
            id,
        }
    }

    fn parent(&self) -> Option<Scope> {
        let parent = self.tree.borrow().parent(self.id);
        parent.map(|p| self.at(p))
    }

    fn children(&self) -> Vec<Scope> {
        self.tree
            .borrow()
            .children(self.id)
            .iter()
            .map(|c| self.at(*c))
            .collect()
    }

    pub fn visit_mut<T>(
        &mut self,
        f: &mut dyn FnMut(Handle, &mut Symbol) -> Result<()>,
    ) -> Result<()> {
        if !data!(self).public {
            return Ok(());
        }

        let module = data!(self).name.clone();
        for (handle, symbol) in data_mut!(self)
            .symbols
            .iter_mut()
            .map(|(k, v)| (Handle::new(&module, k), v))
        {
            f(handle, symbol)?;
        }
        for c in self.children().iter_mut() {
            c.visit_mut::<T>(f)?;
        }
        Ok(())
    }

    pub fn resolve_symbol(&mut self, name: &str) -> Result<Node, symbols::Error> {
        let module = self.module();
        let global = data!(self).global;

        if name.contains('.') {
            if global {
                self.resolve_symbol_with_path(name)
            } else {
                Err(symbols::Error::NotAGlobalScope)
            }
        } else if name.contains(PERSPECTIVE_SEPARATOR) {
            let mut s = name.split(PERSPECTIVE_SEPARATOR);
            let perspective = s
                .next()
                .ok_or_else(|| symbols::Error::MissingColumn(name.into()))?;
            let name = s
                .next()
                .ok_or_else(|| symbols::Error::MissingPerspective(name.into()))?;
            Self::_resolve_symbol_in_perspective(
                self.id,
                &mut self.tree.borrow_mut(),
                name,
                perspective,
            )
            .map_err(|e| match e {
                symbols::Error::SymbolNotFound(s, _, _) => {
                    symbols::Error::SymbolNotFound(s, module, Some(perspective.into()))
                }
                symbols::Error::PerspectiveNotFound(p, _) => {
                    symbols::Error::PerspectiveNotFound(p, module)
                }
                _ => unreachable!(),
            })
        } else {
            Self::_resolve_symbol(
                self.id,
                &mut self.tree.borrow_mut(),
                name,
                &mut HashSet::new(),
                false,
                false,
            )
            .map_err(|_| symbols::Error::SymbolNotFound(name.to_owned(), module, None))
        }
    }

    fn resolve_symbol_with_path(&mut self, name: &str) -> Result<Node, symbols::Error> {
        let components = name.split('.').collect::<Vec<_>>();
        self.root()._resolve_symbol_with_path(&components)
    }

    pub fn resolve_handle(&mut self, h: &Handle) -> Result<Node, symbols::Error> {
        let global = data!(self).global;
        if global {
            self.resolve_symbol(&h.to_string())
        } else {
            self.resolve_symbol(&h.name)
        }
    }

    fn _resolve_symbol(
        n: usize,
        tree: &mut SymbolTableTree,
        name: &str,
        ax: &mut HashSet<String>,
        absolute_path: bool,
        pure: bool,
    ) -> Result<Node, symbols::Error> {
        if ax.contains(name) {
            Err(symbols::Error::CircularDefinition(name.to_string()))
        } else {
            ax.insert(name.to_owned());
            match tree[n].unwrap_data_mut().symbols.get_mut(name) {
                Some(Symbol::Alias(target)) => {
                    let target = target.to_owned();
                    Self::_resolve_symbol(n, tree, &target, ax, absolute_path, pure)
                }
                Some(Symbol::Final(exp, ref mut visited)) => {
                    if pure && !matches!(exp.e(), Expression::Const(..)) {
                        Err(symbols::Error::UnavailableInPureContext(exp.to_string()))
                    } else {
                        *visited = true;
                        Result::Ok(exp.clone())
                    }
                }
                None => {
                    if absolute_path {
                        Err(symbols::Error::SymbolNotFound(name.into(), "".into(), None))
                    } else {
                        tree.parent(n).map_or(
                            Err(symbols::Error::SymbolNotFound(name.into(), "".into(), None)),
                            |parent| {
                                Self::_resolve_symbol(
                                    parent,
                                    tree,
                                    name,
                                    &mut HashSet::new(),
                                    false,
                                    tree[n].unwrap_data().closed || pure,
                                )
                            },
                        )
                    }
                }
            }
        }
    }

    fn _resolve_symbol_in_perspective(
        n: usize,
        tree: &mut SymbolTableTree,
        name: &str,
        perspective: &str,
    ) -> Result<Node, symbols::Error> {
        match tree.find_child(n, |o| {
            o.perspective
                .as_ref()
                .map(|p| p == perspective)
                .unwrap_or(false)
        }) {
            Some(o) => Self::_resolve_symbol(o, tree, name, &mut HashSet::new(), true, false),
            None => tree.parent(n).map_or(
                Err(symbols::Error::PerspectiveNotFound(
                    perspective.into(),
                    tree[n].data().unwrap().module.clone(),
                )),
                |parent| Self::_resolve_symbol_in_perspective(parent, tree, name, perspective),
            ),
        }
    }

    fn _resolve_symbol_with_path(&mut self, path: &[&str]) -> Result<Node, symbols::Error> {
        if path.len() == 1 {
            self.resolve_symbol(path[0])
        } else {
            for c in self.children() {
                if data!(c).name == path[0] {
                    return self.at(c.id)._resolve_symbol_with_path(&path[1..]);
                }
            }
            return Err(symbols::Error::ModuleNotFound(
                path.join("."),
                self.tree.borrow()[self.id].unwrap_data().name.to_string(),
            ));
        }
    }

    fn _edit_symbol(
        n: usize,
        tree: &mut SymbolTableTree,
        name: &str,
        f: &dyn Fn(&mut Expression),
        ax: &mut HashSet<String>,
    ) -> Result<()> {
        if ax.contains(name) {
            Err(anyhow!(symbols::Error::CircularDefinition(name.to_owned())))
        } else {
            ax.insert(name.to_owned());
            match tree[n].unwrap_data_mut().symbols.get_mut(name) {
                Some(Symbol::Alias(to)) => {
                    let to = to.to_owned();
                    Self::_edit_symbol(n, tree, &to, f, ax)
                }
                Some(Symbol::Final(ref mut constraint, _)) => {
                    f(constraint.e_mut());
                    Ok(())
                }
                None => tree.parent(n).map_or(
                    Err(anyhow!(symbols::Error::SymbolNotFound(
                        name.to_owned(),
                        tree[n].unwrap_data().name.to_owned(),
                        None,
                    ))),
                    |parent| Self::_edit_symbol(parent, tree, name, f, ax),
                ),
            }
        }
    }

    fn _resolve_function(&self, name: &str, ax: &mut HashSet<String>) -> Result<Function> {
        if ax.contains(name) {
            bail!(symbols::Error::CircularDefinition(name.to_owned()))
        } else {
            ax.insert(name.to_owned());
            match data!(self).funcs.get(name) {
                Some(Function {
                    class: FunctionClass::Alias(ref to),
                    ..
                }) => self.resolve_function(to),
                Some(f) => Ok(f.to_owned()),
                None => self
                    .parent()
                    .map_or(Err(anyhow!("function {} unknown", name.red())), |parent| {
                        parent.resolve_function(name)
                    }),
            }
        }
    }

    pub fn insert_constraint(&mut self, name: &str) -> Result<()> {
        if data!(self).constraints.contains(name) {
            warn!("redefining constraint `{}`", name.yellow());
        }
        if data_mut!(self).constraints.insert(name.to_owned()) {
            Ok(())
        } else {
            bail!("Constraint `{}` already defined", name)
        }
    }

    pub fn insert_symbol(&mut self, name: &str, e: Node) -> Result<()> {
        if data!(self).symbols.contains_key(name) {
            bail!(symbols::Error::SymbolAlreadyExists(
                name.to_owned(),
                data!(self).name.to_owned()
            ))
        } else {
            data_mut!(self)
                .symbols
                .insert(name.to_owned(), Symbol::Final(e, false));
            Ok(())
        }
    }

    pub fn insert_used_symbol(&mut self, name: &str, e: Node) -> Result<()> {
        self.insert_symbol(name, e)?;
        let _ = self.resolve_symbol(name).unwrap();
        Ok(())
    }

    pub fn insert_function(&mut self, name: &str, f: Function) -> Result<()> {
        let my_name = data!(self).name.to_owned();
        // User-defined function can be polymorphic on their input arguments and
        // thus can be declared multiple times.
        // Polymorphism is handled in the implementation for other classes of
        // functions, thus they can only be defined once.
        match &f.class {
            FunctionClass::UserDefined(new_specialization) => {
                if let Some(Function { ref mut class, .. }) = data_mut!(self).funcs.get_mut(name) {
                    return match class {
                        FunctionClass::UserDefined(ref mut defined) => defined
                            .add_specialization(new_specialization)
                            .with_context(|| anyhow!("while defining {}", name.yellow())),
                        _ => {
                            bail!(symbols::Error::FunctionAlreadyExists(
                                name.to_owned(),
                                my_name,
                            ))
                        }
                    };
                }

                // silly if/return instead of if/else due to data_mut! usage
                data_mut!(self).funcs.insert(name.to_owned(), f);
                Ok(())
            }
            _ => {
                if data!(self).funcs.contains_key(name) {
                    bail!(symbols::Error::FunctionAlreadyExists(
                        name.to_owned(),
                        data!(self).name.to_owned()
                    ))
                } else {
                    data_mut!(self).funcs.insert(name.to_owned(), f);
                    Ok(())
                }
            }
        }
    }

    pub fn insert_alias(&mut self, from: &str, to: &str) -> Result<()> {
        if data!(self).symbols.contains_key(from) {
            bail!(symbols::Error::SymbolAlreadyExists(
                from.to_owned(),
                data!(self).name.to_owned()
            ))
        } else {
            data_mut!(self)
                .symbols
                .insert(from.to_owned(), Symbol::Alias(to.to_owned()));
            Ok(())
        }
    }

    pub fn insert_funalias(&mut self, from: &str, to: &str) -> Result<()> {
        if data!(self).funcs.contains_key(from) {
            bail!(symbols::Error::AliasAlreadyExists(
                from.to_owned(),
                to.to_owned()
            ))
        } else {
            let module = data!(self).name.clone();
            data_mut!(self).funcs.insert(
                from.to_owned(),
                Function {
                    handle: Handle::new(module, to),
                    class: FunctionClass::Alias(to.to_string()),
                },
            );
            Ok(())
        }
    }

    pub fn edit_symbol(&mut self, name: &str, f: &dyn Fn(&mut Expression)) -> Result<()> {
        Self::_edit_symbol(
            self.id,
            &mut self.tree.borrow_mut(),
            name,
            f,
            &mut HashSet::new(),
        )
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
        if data!(self).symbols.contains_key(name) && !replace {
            bail!(symbols::Error::SymbolAlreadyExists(
                name.to_owned(),
                data!(self).name.to_owned()
            ))
        } else if let Some(fr) = pairing_ce::bn256::Fr::from_str(&value.to_string()) {
            data_mut!(self).symbols.insert(
                name.to_owned(),
                Symbol::Final(
                    Node::from_expr(Expression::Const(value, Some(fr))).with_type(t),
                    false,
                ),
            );
            Ok(())
        } else {
            bail!("{} is not an Fr element", value.to_string().red().bold())
        }
    }

    fn root(&self) -> Scope {
        self.at(self.tree.borrow().root())
    }

    #[allow(dead_code)]
    pub fn print(&self) {
        self.tree.borrow().print(|s| {
            format!(
                "{} - Sym = {{{}}} Fn = {{{}}}",
                s.name,
                s.symbols.keys().join(" "),
                s.funcs.keys().join(" ")
            )
        });
    }
}

impl std::clone::Clone for Scope {
    fn clone(&self) -> Self {
        Scope {
            tree: self.tree.clone(),
            id: self.id,
        }
    }
}

#[derive(Debug)]
pub struct SymbolTable {
    // The name of this table
    pub name: String,
    pub module: String,
    pub perspective: Option<String>,
    // The parent relationship is only used for contextual
    // semantics (i.e. for & functions), not modules
    closed: bool,
    // If true, then those are module definitions,
    // otherwise, this table is a private table, e.g. function arguments.
    // This is used when browsing the tables to avoid inner contexts.
    public: bool,
    // If true, fully qualified handles can be looked up; otherwise
    // it will result in a failure.
    // This setting in forcefully inherited by children scopes.
    global: bool,
    constraints: HashSet<String>,
    funcs: HashMap<String, Function>,
    symbols: HashMap<String, Symbol>,
}
