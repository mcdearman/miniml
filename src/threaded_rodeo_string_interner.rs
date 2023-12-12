use crate::compiler::{interned_string::InternedString, string_interner::StringInterner};
use lasso::ThreadedRodeo;
use std::fmt::Debug;

#[derive(Debug)]
pub struct ThreadedRodeoInterner(ThreadedRodeo);

impl StringInterner for ThreadedRodeoInterner {
    fn get_or_intern<T: InternedString>(&self, s: &str) -> T {
        T::from_str(self.0.get_or_intern(s))
    }

    fn resolve<T: InternedString>(&self, key: &T) -> &str {
        self.0.resolve(key.key)
    }
}
