use super::value::Value;
use crate::utils::intern::InternedString;
use std::{cell::RefCell, collections::HashMap, fmt::Debug, rc::Rc};

#[derive(Clone, PartialEq)]
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

    pub fn new_with_parent(parent: Rc<RefCell<Env>>) -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(Self {
            parent: Some(parent),
            bindings: HashMap::new(),
        }))
    }

    pub fn insert(&mut self, name: InternedString, value: Value) {
        self.bindings.insert(name, value);
    }

    pub fn get(&self, name: &InternedString) -> Option<Value> {
        self.bindings.get(name).cloned().or(self
            .parent
            .as_ref()
            .and_then(|parent| parent.borrow().get(name).clone()))
    }
}

impl Debug for Env {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut builder = f.debug_struct("Env");
        builder.field(
            "parent",
            if let Some(_) = &self.parent {
                &"Some"
            } else {
                &"None"
            },
        );
        builder.field("bindings", &self.bindings);
        builder.finish()
    }
}
