use crate::{
    compiler::{interned_string::InternedString, string_interner::StringInterner},
    threaded_rodeo_interned_string::ThreadedRodeoInternedString,
};
use lasso::ThreadedRodeo;
use once_cell::sync::Lazy;
use std::fmt::Debug;

pub static mut INTERNER: Lazy<ThreadedRodeo> = Lazy::new(|| ThreadedRodeo::default());

#[derive(Debug, PartialEq, Eq)]
pub struct ThreadedRodeoInterner(ThreadedRodeo);

impl StringInterner for ThreadedRodeoInterner {
    type Key = ThreadedRodeoInternedString;

    fn get_or_intern(&self, s: &str) -> Self::Key {
        ThreadedRodeoInternedString::new(self, self.0.get_or_intern(s))
    }

    fn resolve<K: InternedString>(&self, key: Self::Key) -> &str {
        self.0.resolve(key)
    }
}
