use lasso::{Spur, ThreadedRodeo};
use once_cell::sync::Lazy;
use std::{
    borrow::Borrow,
    fmt::{Debug, Display},
    ops::Deref,
};

pub static mut INTERNER: Lazy<ThreadedRodeo> = Lazy::new(|| ThreadedRodeo::default());

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Symbol {
    key: Spur,
}

impl From<Spur> for Symbol {
    fn from(key: Spur) -> Self {
        Self { key }
    }
}

impl From<&str> for Symbol {
    fn from(name: &str) -> Self {
        Self {
            key: unsafe { INTERNER.get_or_intern(name) },
        }
    }
}

impl From<String> for Symbol {
    fn from(name: String) -> Self {
        Self {
            key: unsafe { INTERNER.get_or_intern(name) },
        }
    }
}

impl Debug for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "InternedString({})", unsafe {
            INTERNER.resolve(&self.key)
        })
    }
}

impl Display for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "\"{}\"", unsafe { INTERNER.resolve(&self.key) })
    }
}

impl Borrow<str> for Symbol {
    fn borrow(&self) -> &str {
        unsafe { INTERNER.resolve(&self.key) }
    }
}

impl Deref for Symbol {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        unsafe { INTERNER.resolve(&self.key) }
    }
}
