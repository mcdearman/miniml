use super::scheme::Scheme;
use crate::utils::unique_id::UniqueId;
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub struct Registry {
    vars: HashMap<UniqueId, Scheme>,
}

impl Registry {
    pub fn new() -> Self {
        Self {
            vars: HashMap::new(),
        }
    }

    pub fn insert(&mut self, id: UniqueId, scheme: Scheme) {
        self.vars.insert(id, scheme);
    }

    pub fn get(&self, id: &UniqueId) -> Option<&Scheme> {
        self.vars.get(id)
    }

    pub fn iter(&self) -> impl Iterator<Item = (&UniqueId, &Scheme)> {
        self.vars.iter()
    }
}
