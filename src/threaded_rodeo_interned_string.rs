use std::{fmt::Debug, ops::Deref, str::FromStr};

use crate::{
    compiler::interned_string::InternedString,
    threaded_rodeo_string_interner::ThreadedRodeoInterner,
};
use lasso::Spur;

#[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ThreadedRodeoInternedString<'a> {
    interner: &'a ThreadedRodeoInterner,
    key: Spur,
}

impl<'a> InternedString for ThreadedRodeoInternedString<'a> {
    fn get_interner<'a, I: crate::compiler::string_interner::StringInterner>() -> &'a I {
        todo!()
    }
}

impl<'a> FromStr for ThreadedRodeoInternedString<'a> {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        todo!()
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
