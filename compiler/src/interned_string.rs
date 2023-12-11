use crate::string_interner::StringInterner;
use std::{fmt::Debug, hash::Hash, str::FromStr};

pub trait InternedString: Debug + Copy + Clone + PartialEq + Eq + Hash + FromStr {
    fn get_interner<'a, I: StringInterner>() -> &'a I;
}
