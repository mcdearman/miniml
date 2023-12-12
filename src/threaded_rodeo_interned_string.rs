use std::{fmt::Debug, ops::Deref};

use crate::{
    compiler::interned_string::InternedString,
    threaded_rodeo_string_interner::ThreadedRodeoInterner,
};
use lasso::Spur;

#[derive(Copy, Clone, PartialEq, Hash, PartialOrd)]
pub struct ThreadedRodeoInternedString<'a> {
    interner: &'a ThreadedRodeoInterner,
    key: Spur,
}

// impl<'a> ThreadedRodeoInternedString<'a> {}

impl<'a> InternedString for ThreadedRodeoInternedString<'a> {
    fn new(interner: &'a ThreadedRodeoInterner, key: Spur) -> Self {
        Self { interner, key }
    }

    fn get_interner<'b, I: crate::compiler::string_interner::StringInterner>(&self) -> &'b I {
        self.interner
    }
}

impl<'a> From<&'a str> for ThreadedRodeoInternedString<'a> {
    fn from(s: &'a str) -> Self {
        Self {
            interner: &ThreadedRodeoInterner::new(),
            key: ThreadedRodeoInterner::new().get_or_intern(s),
        }
    }
}

impl<'a> Deref for ThreadedRodeoInternedString<'a> {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        unsafe { self.interner.resolve(&self.key) }
    }
}

impl<'a> Debug for ThreadedRodeoInternedString<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "InternedString({})", unsafe {
            self.interner.resolve(&self.key)
        })
    }
}

impl<'a> Eq for ThreadedRodeoInternedString<'a> {}
