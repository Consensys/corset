use serde::de::{Error, Visitor};
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use std::fmt;

use crate::{compiler::MAIN_MODULE, utils::purify};

use super::{ARRAY_SEPARATOR, MODULE_SEPARATOR};

/// A handle uniquely and absolutely defines a symbol
#[derive(Clone)]
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

    pub fn to_string(&self) -> String {
        // NOTE: its unclear why a distinction is needed for the
        // prelude.
        if self.module == "<prelude>" {
            match &self.perspective {
                // Generate cases
                None => format!("{}", self.name),
                Some(p) => format!("{}/{}", p, self.name),
            }
        } else {
            match &self.perspective {
                // Generate cases
                None => format!("{}.{}", self.module, self.name),
                Some(p) => format!("{}.{}/{}", self.module, p, self.name),
            }
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

    /// Generate a symbol corresponding to the ith column of an ArrayColumn
    pub fn iota(&self, i: usize) -> Handle {
        Handle {
            module: self.module.clone(),
            name: format!("{}ɩ{}", self.name, i),
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

    pub fn mangle_ith(&self, i: usize) -> String {
        let r = format!(
            "{}{}{}ɩ{}",
            purify(&self.module),
            if self.module.is_empty() {
                ""
            } else {
                MODULE_SEPARATOR
            },
            purify(&self.name),
            i,
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

impl Handle {
    pub fn to_serialized_string(&self) -> String {
        match &self.perspective {
            None => format!("{}.{}", self.module, self.name),
            Some(p) => format!("{}.{}:{}", self.module, self.name, p),
        }
    }
    pub fn from_serialized_string(input: &str) -> Result<Handle, String> {
        let p1: Vec<&str> = input.split(":").collect();
        // Split up module / name
        let p2: Vec<&str> = p1[0].split(".").collect();
        // Error check
        if p1.len() > 2 || p2.len() > 2 {
            Err(format!("invalid serialized Handle: {}", input))
        } else {
            // Attempt to extract perspective (if present)
            let perspective = if p1.len() == 1 {
                // No perspective provided
                None
            } else {
                // Extract perspective name
                Some(p1[1].to_string())
            };
            // Done
            Ok(Handle::maybe_with_perspective(p2[0], p2[1], perspective))
        }
    }
}

impl<'a> Deserialize<'a> for Handle {
    fn deserialize<S: Deserializer<'a>>(deserializer: S) -> Result<Self, S::Error> {
        let st = String::deserialize(deserializer)?;
        // Decode it
        Self::from_serialized_string(&st).map_err(S::Error::custom)
    }
}

impl Serialize for Handle {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        serializer.serialize_str(&self.to_serialized_string())
    }
}
