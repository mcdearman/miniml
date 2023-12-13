use super::string_interner::StringInterner;
use std::{fmt::Debug, hash::Hash};

pub trait InternedString: Debug + Clone + PartialEq + Eq + Hash {
    type Interner: StringInterner;

    fn new(interner: Box<Self::Interner>, s: &str) -> Self;
    fn get_interner(&self) -> Box<Self::Interner>;
}
