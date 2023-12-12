use super::string_interner::StringInterner;
use std::{fmt::Debug, hash::Hash};

pub trait InternedString<'a>: Debug + Copy + Clone + PartialEq + Eq + Hash + From<&'a str> {
    fn get_interner<'b, I: StringInterner>() -> &'b I;
}
