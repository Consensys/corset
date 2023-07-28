use serde::{Deserialize, Serialize};

use crate::{compiler::MAIN_MODULE, utils::purify};

use super::{ARRAY_SEPARATOR, MODULE_SEPARATOR, OVERFLOW_SEPARATOR, SUM_SEPARATOR, BYTE_SEPARATOR, ACC_SEPARATOR};

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
impl std::cmp::Ord for Handle {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match self.perspective.cmp(&other.perspective) {
            std::cmp::Ordering::Equal => match self.module.cmp(&other.module) {
                std::cmp::Ordering::Equal => self.name.cmp(&other.name),
                other => other,
            },
            other => other,
        }
    }
}
impl std::cmp::PartialOrd for Handle {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
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
    ) -> Handle {
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

    /// Generate a symbol corresponding to the sum of two Handle (used to sum field agnostic columns)
    pub fn sum(&self, other: &Handle) -> Handle {
        Handle {
            module: self.module.clone(),
            name: format!(
                "{}{}{}.{}",
                self.name, SUM_SEPARATOR, other.module, other.name
            ), // TODO reprendre
            perspective: self.perspective.clone(), // TODO handle other.perspective
        }
    }

    pub fn as_overflow_handle(&self, index: usize) -> Self {
        Handle {
            module: self.module.clone(),
            name: format!("{}{}{}", self.name, OVERFLOW_SEPARATOR, index), // TODO reprendre
            perspective: self.perspective.clone(), // TODO handle other.perspective
        }
    }

    pub fn as_byte_handle(&self, index: usize) -> Self {
        Handle {
            module: self.module.clone(),
            name: format!("{}{}{}", self.name, BYTE_SEPARATOR, index), // TODO reprendre
            perspective: self.perspective.clone(), // TODO handle other.perspective
        }
    }

    pub fn as_acc_handle(&self, index: usize) -> Self {
        Handle {
            module: self.module.clone(),
            name: format!("{}{}{}", self.name, ACC_SEPARATOR, index), // TODO reprendre
            perspective: self.perspective.clone(), // TODO handle other.perspective
        }
    }

    pub fn as_counter_handle(&self) -> Self {
        Handle {
            module: self.module.clone(),
            name: format!("{}{}", self.name, ACC_SEPARATOR), // TODO reprendre
            perspective: self.perspective.clone(), // TODO handle other.perspective
        }
    }

    pub fn to_sum_constraint(&self, index: usize) -> Handle {
        Handle {
            module: self.module.clone(),
            name: format!("{}_sub_constraint_{}", self.name, index),
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
        purify(&format!(
            "{}{}",
            self.perspective
                .clone()
                .map(|s| format!("{s}__"))
                .unwrap_or_default(),
            &self.name
        ))
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
