use super::value::Value;
use crate::utils::unique_id::UniqueId;
use std::collections::HashMap;

pub struct Env {
    bindings: HashMap<UniqueId, Value>,
}

impl Env {
    pub fn new() -> Self {
        Self {
            bindings: HashMap::new(),
        }
    }

    pub fn def(&mut self, id: UniqueId, value: Value) {
        self.bindings.insert(id, value);
    }

    pub fn get(&self, id: &UniqueId) -> Option<Value> {
        self.bindings.get(id).cloned()
    }

    pub fn del(&mut self, id: &UniqueId) -> Option<Value> {
        self.bindings.remove(id)
    }
}
