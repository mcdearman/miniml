use std::fmt::Debug;

pub trait Object: Debug + Clone + PartialEq + Eq + PartialOrd {
    // fn get_class(&self) -> InternedString;
}
