use crate::{
    intern::InternedString,
    list::List,
    parser::{Expr, Pattern},
};
use num_bigint::BigInt;
use std::{
    cell::{Ref, RefCell},
    collections::HashMap,
    fmt::Display,
    rc::Rc,
};

#[derive(Debug, Clone, PartialEq)]
pub struct Env {
    pub bindings: HashMap<InternedString, Value>,
    pub parent: Option<Rc<RefCell<Env>>>,
}

impl Env {
    pub fn new() -> Self {
        Self {
            bindings: HashMap::new(),
            parent: None,
        }
    }

    pub fn create_child(parent: Rc<RefCell<Self>>) -> Self {
        Self {
            bindings: HashMap::new(),
            parent: Some(parent),
        }
    }

    pub fn define(&mut self, name: InternedString, val: Value) {
        self.bindings.insert(name, val);
    }

    pub fn lookup(&self, name: &InternedString) -> Option<Value> {
        if let Some(v) = self.bindings.get(name) {
            Some(v.clone())
        } else if let Some(parent) = &self.parent {
            parent.as_ref().borrow().lookup(name)
        } else {
            None
        }
    }
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
    Lambda {
        env: Rc<RefCell<Env>>,
        param: Pattern,
        body: Box<Self>,
    },
    Unit,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct RuntimeError(pub String);

impl RuntimeError {
    pub fn new(msg: &str) -> Self {
        Self(msg.to_string())
    }
}

impl Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

pub type Result<T> = std::result::Result<T, RuntimeError>;

pub fn eval(env: &mut Env, expr: &Value) -> Result<Value> {
    todo!()
}
