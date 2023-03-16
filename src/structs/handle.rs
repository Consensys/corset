use serde::{Deserialize, Serialize};

use super::{ARRAY_SEPARATOR, MODULE_SEPARATOR};

/// A handle uniquely and absolutely defines a symbol
#[derive(Clone, Serialize, Deserialize)]
pub struct Handle {
    /// the module to which the symbol belongs
    /// NOTE multi-level paths are not yet implemented
    pub module: String,
    /// the name of the symbol within its module
    pub name: String,
    /// a wart for optimization when evaluating constraints, where
    /// addressing symbold by ID is faster than string comparison.
    pub id: Option<usize>,
}
// The equality relation is only used in a semantic way, not computational
impl std::cmp::PartialEq for Handle {
    fn eq(&self, other: &Self) -> bool {
        (self.module == other.module) && (self.name == other.name)
    }
}
impl std::cmp::Eq for Handle {}
impl std::hash::Hash for Handle {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.module.hash(state);
        self.name.hash(state);
    }
}
impl Handle {
    pub fn new<S1: AsRef<str>, S2: AsRef<str>>(module: S1, name: S2) -> Self {
        Handle {
            module: module.as_ref().to_owned(),
            name: name.as_ref().to_owned(),
            id: None,
        }
    }

    pub fn set_id(&mut self, i: usize) {
        self.id = Some(i)
    }

    /// Generate a symbol corresponding to the ith column of an ArrayColumn
    pub fn ith(&self, i: usize) -> Handle {
        Handle {
            module: self.module.clone(),
            name: format!("{}{}{}", self.name, ARRAY_SEPARATOR, i),
            id: self.id,
        }
    }

    /// Remove all symbols in a symbol which are invalid in Go identifiers
    fn purify(s: &str) -> String {
        s.replace(['(', ')', '{', '}', '[', ']', '<', '>', ':', '%', '.'], "_")
            .replace('-', "sub")
            .replace('*', "mul")
            .replace('+', "add")
            .replace('/', "div")
            .replace(|c: char| !c.is_ascii(), "_")
    }

    /// Uniquely mangle a symbol into something usable in Go
    pub fn mangle(&self) -> String {
        let r = format!(
            "{}{}{}",
            Self::purify(&self.module),
            if self.module.is_empty() {
                ""
            } else {
                MODULE_SEPARATOR
            },
            Self::purify(&self.name)
        );
        r
    }

    /// Uniquely mangle the name of a symbol into something usable in Go
    pub fn mangled_name(&self) -> String {
        Self::purify(&self.name)
    }

    /// Uniquely mangle the module of a symbol into something usable in Go
    pub fn mangled_module(&self) -> String {
        Self::purify(&self.module)
    }
}
impl std::fmt::Debug for Handle {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}.{}", self.module, self.name)
    }
}
impl std::fmt::Display for Handle {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}.{}", self.module, self.name)
    }
}
