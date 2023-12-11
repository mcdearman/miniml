use compiler::string_interner::StringInterner;
use lasso::{Spur, ThreadedRodeo};
use once_cell::sync::Lazy;
use std::{
    fmt::{Debug, Display},
    ops::Deref,
};

// #[derive(Debug)]
// pub struct ThreadedRodeoInterner(ThreadedRodeo);

// impl StringInterner for ThreadedRodeoInterner {
//     fn get_or_intern<T: InternedString>(&self, s: &str) -> T {
//         T::from_str(self.0.get_or_intern(s))
//     }

//     fn resolve<T: InternedString>(&self, key: &T) -> &str {
//         self.0.resolve(key.key)
//     }
// }

pub static mut INTERNER: Lazy<ThreadedRodeo> = Lazy::new(|| ThreadedRodeo::default());

#[repr(C)]
#[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct InternedString {
    pub key: Spur,
}

impl From<Spur> for InternedString {
    fn from(key: Spur) -> Self {
        Self { key }
    }
}

impl From<&str> for InternedString {
    fn from(s: &str) -> Self {
        Self {
            key: unsafe { INTERNER.get_or_intern(s) },
        }
    }
}

impl From<String> for InternedString {
    fn from(s: String) -> Self {
        Self {
            key: unsafe { INTERNER.get_or_intern(s) },
        }
    }
}

impl Deref for InternedString {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        unsafe { INTERNER.resolve(&self.key) }
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
