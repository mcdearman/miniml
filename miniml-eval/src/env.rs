use crate::value::Value;
use miniml_util::intern::InternedString;
use std::{cell::RefCell, collections::HashMap, rc::Rc};

#[derive(Debug, Clone)]
pub struct Env {
    parent: Option<Rc<RefCell<Env>>>,
    bindings: HashMap<InternedString, Value>,
}

impl Env {
    pub fn new() -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(Self {
            parent: None,
            bindings: HashMap::new(),
        }))
    }

    pub fn with_parent(parent: Rc<RefCell<Env>>) -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(Self {
            parent: Some(parent),
            bindings: HashMap::new(),
        }))
    }

    pub fn find(&self, name: &InternedString) -> Option<Value> {
        match self.bindings.get(name) {
            Some(value) => Some(value.clone()),
            None => match &self.parent {
                Some(parent) => parent.borrow().find(name),
                None => None,
            },
        }
    }

    pub fn define(&mut self, name: InternedString, value: Value) {
        self.bindings.insert(name, value);
    }
}
