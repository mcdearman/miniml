use dbg_pls::DebugPls;
use lasso::{Spur, ThreadedRodeo};
use once_cell::sync::Lazy;
use std::{
    borrow::Borrow,
    fmt::{Debug, Display},
    ops::Deref,
};

static mut INTERNER: Lazy<ThreadedRodeo> = Lazy::new(|| ThreadedRodeo::default());

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
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

impl DebugPls for InternedString {
    fn fmt(&self, f: dbg_pls::Formatter<'_>) {
        f.debug_ident(&unsafe { INTERNER.resolve(&self.key) });
    }
}

impl Display for InternedString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", unsafe { INTERNER.resolve(&self.key) })
    }
}

impl Borrow<str> for InternedString {
    fn borrow(&self) -> &str {
        unsafe { INTERNER.resolve(&self.key) }
    }
}

impl Deref for InternedString {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        unsafe { INTERNER.resolve(&self.key) }
    }
}
