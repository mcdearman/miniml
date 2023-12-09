use common::interner::InternedString;
use std::fmt::Debug;

pub trait Object: Debug + Clone + PartialEq + Eq + PartialOrd {
    fn new() -> Self;
    fn name(&self) -> InternedString;
    fn define_method(&mut self, name: InternedString, args: Vec<Self>, body: Self);
    fn invoke_method(&self, name: InternedString, args: Vec<Self>) -> Self;
    fn get_property(&self, name: InternedString) -> Self;
    fn set_property(&mut self, name: InternedString, value: Self);
    fn add_parent(&mut self, parent: Self);
    fn get_parents(&self) -> Vec<Self>;
}
