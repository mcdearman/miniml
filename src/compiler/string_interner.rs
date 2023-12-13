use super::interned_string::InternedString;
use std::fmt::Debug;

pub trait StringInterner: Debug {
    type Key: InternedString;

    fn get_or_intern(&self, s: &str) -> Self::Key;
    fn resolve(&self, key: Self::Key) -> &str;
}
