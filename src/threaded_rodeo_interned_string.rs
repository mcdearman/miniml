use std::{fmt::Debug, ops::Deref};

use crate::{
    compiler::interned_string::InternedString,
    threaded_rodeo_string_interner::ThreadedRodeoInterner,
};
use lasso::Spur;


#[derive(Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct ThreadedRodeoInternedString<'a> {
    interner: &'a ThreadedRodeoInterner,
    key: Spur,
}

impl InternedString for ThreadedRodeoInternedString {
    type Interner = ThreadedRodeoInterner;

    fn new(interner: Box<Self::Interner>, key: Spur) -> Self {
        Self { interner, key }
    }

    fn get_interner(&self) -> Box<Self::Interner> {
        self.interner
    }
}

impl Deref for ThreadedRodeoInternedString {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        unsafe { self.interner.resolve(&self.key) }
    }
}

impl Debug for ThreadedRodeoInternedString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "InternedString({})", unsafe {
            self.interner.resolve(&self.key)
        })
    }
}
