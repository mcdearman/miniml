use crate::{
    compiler::string_interner::StringInterner,
    threaded_rodeo_interned_string::ThreadedRodeoInternedString,
};
use lasso::ThreadedRodeo;
use std::fmt::Debug;

#[derive(Debug, PartialEq, Eq)]
pub struct ThreadedRodeoInterner(ThreadedRodeo);

impl<'a> StringInterner<ThreadedRodeoInternedString<'a>> for ThreadedRodeoInterner {
    fn get_or_intern(&self, s: &str) -> ThreadedRodeoInternedString<'a> {
        self.0.get_or_intern(s)
    }

    fn resolve(&self, key: &ThreadedRodeoInternedString<'a>) -> &str {
        self.0.resolve(key)
    }
}
