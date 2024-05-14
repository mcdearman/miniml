use super::scheme::PolyType;
use crate::utils::unique_id::UniqueId;
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub struct Registry {
    vars: HashMap<UniqueId, PolyType>,
}

impl Registry {
    pub fn new() -> Self {
        Self {
            vars: HashMap::new(),
        }
    }

    pub fn insert(&mut self, id: UniqueId, scheme: PolyType) {
        self.vars.insert(id, scheme);
    }

    pub fn get(&self, id: &UniqueId) -> Option<&PolyType> {
        self.vars.get(id)
    }

    pub fn iter(&self) -> impl Iterator<Item = (&UniqueId, &PolyType)> {
        self.vars.iter()
    }
}
