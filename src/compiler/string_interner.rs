use super::interned_string::InternedString;
use std::fmt::Debug;

pub trait StringInterner: Debug + Clone {
    fn get_or_intern<T: InternedString>(&self, s: &str) -> T;
    fn resolve<T: InternedString>(&self, key: &T) -> &str;
}
