use super::value::Value;
use crate::utils::{intern::InternedString, unique_id::UniqueId};
use std::{cell::RefCell, collections::HashMap, fmt::Debug, rc::Rc};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct EnvIdent {
    pub id: UniqueId,
    pub name: InternedString,
}

#[derive(Clone, PartialEq)]
pub struct Env {
    parent: Option<Rc<RefCell<Env>>>,
    bindings: HashMap<EnvIdent, Value>,
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

    pub fn insert(&mut self, id: EnvIdent, value: Value) {
        self.bindings.insert(id, value);
    }

    pub fn get(&self, ident: &EnvIdent) -> Option<Value> {
        self.bindings.get(ident).cloned().or(self
            .parent
            .as_ref()
            .and_then(|parent| parent.borrow().get(ident).clone()))
    }

    pub fn dump(&self) -> HashMap<EnvIdent, Value> {
        let mut map = self.bindings.clone();
        if let Some(parent) = &self.parent {
            map.extend(parent.borrow().dump());
        }
        map
    }

    pub fn dump_frame(&self) -> HashMap<EnvIdent, Value> {
        self.bindings.clone()
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
