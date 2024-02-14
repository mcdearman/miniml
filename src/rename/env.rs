use crate::utils::{intern::InternedString, unique_id::UniqueId};
use std::{cell::RefCell, collections::HashMap, rc::Rc};

#[derive(Debug, Clone, PartialEq)]
pub struct Env {
    parent: Option<Rc<RefCell<Env>>>,
    data: HashMap<InternedString, UniqueId>,
}

impl Env {
    pub fn new() -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(Self {
            parent: None,
            data: HashMap::new(),
        }))
    }

    pub fn new_with_builtins(builtins: HashMap<UniqueId, InternedString>) -> Rc<RefCell<Self>> {
        let mut data = HashMap::new();
        for (id, name) in builtins {
            data.insert(name, id);
        }
        Rc::new(RefCell::new(Self { parent: None, data }))
    }

    pub(super) fn new_with_parent(parent: Rc<RefCell<Self>>) -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(Self {
            parent: Some(parent),
            data: HashMap::new(),
        }))
    }

    pub(super) fn find(&self, name: &InternedString) -> Option<UniqueId> {
        if let Some(id) = self.data.get(name) {
            Some(*id)
        } else if let Some(parent) = &self.parent {
            parent.borrow().find(name)
        } else {
            None
        }
    }

    pub(super) fn find_in_scope(&self, name: &InternedString) -> Option<UniqueId> {
        self.data.get(name).copied()
    }

    pub(super) fn find_in_parent(&self, name: &InternedString) -> Option<UniqueId> {
        if let Some(parent) = &self.parent {
            parent.borrow().find(name)
        } else {
            None
        }
    }

    pub(super) fn define(&mut self, name: InternedString) -> UniqueId {
        let id = UniqueId::gen();
        self.data.insert(name, id);
        id
    }

    pub(super) fn insert(&mut self, name: InternedString, id: UniqueId) {
        self.data.insert(name, id);
    }

    pub(super) fn define_if_absent_in_scope(&mut self, name: InternedString) -> Option<UniqueId> {
        if let Some(id) = self.find_in_scope(&name) {
            Some(id)
        } else {
            let id = UniqueId::gen();
            self.data.insert(name, id);
            Some(id)
        }
    }

    pub(super) fn define_if_absent(&mut self, name: InternedString) -> UniqueId {
        if let Some(id) = self.find(&name) {
            id
        } else {
            let id = UniqueId::gen();
            self.data.insert(name, id);
            id
        }
    }
}
