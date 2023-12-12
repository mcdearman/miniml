use super::string_interner::StringInterner;
use std::{fmt::Debug, hash::Hash};

pub trait InternedString: Debug + Copy + Clone + PartialEq + Eq + Hash {
    fn new<K: InternedString, I: StringInterner<K>>(interner: I, s: &str) -> Self;
    fn get_interner<'b, K: InternedString, I: StringInterner<K>>(&self) -> &'b I;
}
