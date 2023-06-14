use serde::{Deserialize, Serialize};

use crate::{compiler::MAIN_MODULE, utils::purify};

use super::{ARRAY_SEPARATOR, MODULE_SEPARATOR};

/// A handle uniquely and absolutely defines a symbol
#[derive(Clone, Serialize, Deserialize)]
pub struct Handle {
    /// the module to which the symbol belongs
    /// NOTE multi-level paths are not yet implemented
    pub module: String,
    /// the name of the symbol within its module
    pub name: String,
    /// the perspective this symbol belongs to, if applicable
    pub perspective: Option<String>,
}
// The equality relation is only used in a semantic way, not computational
impl std::cmp::PartialEq for Handle {
    fn eq(&self, other: &Self) -> bool {
        (self.module == other.module)
            && (self.name == other.name)
            && (self.perspective == other.perspective)
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
            perspective: None,
        }
    }

    pub fn maybe_with_perspective<S1: AsRef<str>, S2: AsRef<str>>(
        module: S1,
        name: S2,
        perspective: Option<String>,
    ) -> Self {
        Handle {
            module: module.as_ref().to_owned(),
            name: name.as_ref().to_owned(),
            perspective,
        }
    }

    pub fn and_with_perspective(mut self, perspective: Option<String>) -> Self {
        self.perspective = perspective;
        self
    }

    /// Generate a symbol corresponding to the ith column of an ArrayColumn
    pub fn ith(&self, i: usize) -> Handle {
        Handle {
            module: self.module.clone(),
            name: format!("{}{}{}", self.name, ARRAY_SEPARATOR, i),
            perspective: self.perspective.clone(),
        }
    }

    /// Uniquely mangle a symbol into something usable in Go
    pub fn mangle(&self) -> String {
        let r = format!(
            "{}{}{}",
            purify(&self.module),
            if self.module.is_empty() {
                ""
            } else {
                MODULE_SEPARATOR
            },
            purify(&self.name)
        );
        r
    }

    /// Uniquely mangle the name of a symbol into something usable in Go
    pub fn mangled_name(&self) -> String {
        purify(&self.name)
    }

    /// Uniquely mangle the module of a symbol into something usable in Go
    pub fn mangled_module(&self) -> String {
        purify(&self.module)
    }
}
impl std::fmt::Debug for Handle {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}.{}%{:?}", self.module, self.name, self.perspective)
    }
}
impl std::fmt::Display for Handle {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        if self.module == MAIN_MODULE {
            write!(
                f,
                "{}{}",
                if let Some(p) = self.perspective.as_ref() {
                    format!("{}/", p)
                } else {
                    "".to_owned()
                },
                self.name,
            )
        } else {
            write!(
                f,
                "{}.{}{}",
                self.module,
                if let Some(p) = self.perspective.as_ref() {
                    format!("{}/", p)
                } else {
                    "".to_owned()
                },
                self.name,
            )
        }
    }
}
