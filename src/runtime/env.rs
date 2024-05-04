use super::value::Value;
use crate::utils::unique_id::UniqueId;
use std::{cell::RefCell, collections::HashMap, fmt::Debug, rc::Rc};

#[derive(Clone, PartialEq)]
pub struct Env {
    parent: Option<Rc<RefCell<Env>>>,
    bindings: HashMap<UniqueId, Value>,
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

    pub fn insert(&mut self, id: UniqueId, value: Value) {
        self.bindings.insert(id, value);
    }

    pub fn get(&self, ident: &UniqueId) -> Option<Value> {
        self.bindings.get(ident).cloned().or(self
            .parent
            .as_ref()
            .and_then(|parent| parent.borrow().get(ident).clone()))
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
