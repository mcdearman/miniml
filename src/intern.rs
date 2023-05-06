use lasso::{Spur, ThreadedRodeo};
use once_cell::sync::Lazy;
use std::fmt::{Debug, Display};

pub static mut INTERNER: Lazy<ThreadedRodeo> = Lazy::new(|| ThreadedRodeo::default());

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct InternedString {
    pub key: Spur,
}

impl From<Spur> for InternedString {
    fn from(key: Spur) -> Self {
        Self { key }
    }
}

impl From<&str> for InternedString {
    fn from(name: &str) -> Self {
        Self {
            key: unsafe { INTERNER.get_or_intern(name) },
        }
    }
}

impl From<String> for InternedString {
    fn from(name: String) -> Self {
        Self {
            key: unsafe { INTERNER.get_or_intern(name) },
        }
    }
}

impl Debug for InternedString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "InternedString({})", unsafe {
            INTERNER.resolve(&self.key)
        })
    }
}

impl Display for InternedString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", unsafe { INTERNER.resolve(&self.key) })
    }
}
