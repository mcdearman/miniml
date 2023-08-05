use std::{cell::RefCell, collections::HashMap, rc::Rc};

use miniml_util::intern::InternedString;

use crate::value::Value;

#[derive(Debug, Clone)]
pub struct Env {
    parent: Option<Rc<RefCell<Env>>>,
    bindings: HashMap<InternedString, Value>,
}

impl Env {
    pub fn new() -> Self {
        Self {
            parent: None,
            bindings: HashMap::new(),
        }
    }

    pub fn with_parent(parent: Rc<RefCell<Env>>) -> Self {
        Self {
            parent: Some(parent),
            bindings: HashMap::new(),
        }
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