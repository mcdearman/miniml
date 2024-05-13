use super::meta::Meta;
use crate::utils::unique_id::UniqueId;
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub struct MetaContext {
    bindings: HashMap<UniqueId, Meta>,
}

impl MetaContext {
    pub fn new() -> Self {
        Self {
            bindings: HashMap::new(),
        }
    }

    pub fn insert(&mut self, meta: Meta) -> UniqueId {
        let id = UniqueId::gen();
        self.bindings.insert(id, meta);
        id
    }

    pub fn get(&self, id: UniqueId) -> Option<&Meta> {
        self.bindings.get(&id)
    }

    pub fn get_mut(&mut self, id: UniqueId) -> Option<&mut Meta> {
        self.bindings.get_mut(&id)
    }
}
