use super::interned_string::InternedString;
use std::fmt::Debug;

pub trait StringInterner<K: InternedString>: Debug {
    fn get_or_intern(&self, s: &str) -> K;
    fn resolve(&self, key: &K) -> &str;
}
