use std::{cell::RefCell, collections::HashMap, rc::Rc};

use num_bigint::BigInt;

use crate::{intern::InternedString, list::List, parser::Pattern};

#[derive(Debug, Clone, PartialEq)]
pub struct Env {
    pub bindings: HashMap<InternedString, Value>,
    pub parent: Option<Rc<RefCell<Env>>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Int(BigInt),
    Float(f64),
    String(InternedString),
    Char(char),
    Bool(bool),
    List(List<Self>),
    Tuple(Vec<Self>),
    Map(HashMap<InternedString, Self>),
    Record(HashMap<InternedString, Self>),
    Lambda { param: Pattern, body: Box<Self> },
    Unit,
}
