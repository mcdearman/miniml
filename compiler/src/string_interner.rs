use std::{fmt::Debug, hash::Hash};

pub trait StringInterner: Debug + Clone {
    fn get_or_intern<T: InternedString>(&self, s: &str) -> T;
    fn resolve<T: InternedString>(&self, key: &T) -> &str;
}

pub trait InternedString: Debug + Copy + Clone + PartialEq + Eq + Hash {
    fn from_str(s: &str) -> Self;
    fn from_string(s: String) -> Self;
    fn as_str(&self) -> &str;
}
