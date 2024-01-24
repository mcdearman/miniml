use crate::utils::{InternedString, UniqueId};
use std::{collections::HashMap, fmt::Debug};

#[derive(Debug, Clone)]
pub struct Database {
    map: HashMap<UniqueId, InternedString>,
}

impl Database {
    pub fn new() -> Self {
        Self {
            map: HashMap::new(),
        }
    }
}

impl Database {
    pub fn insert(&mut self, key: UniqueId, value: InternedString) {
        self.map.insert(key, value);
    }

    pub fn insert_or_get(&mut self, key: UniqueId, value: InternedString) -> InternedString {
        if let Some(value) = self.get(key) {
            value.clone()
        } else {
            self.insert(key, value);
            value
        }
    }

    pub fn get(&self, key: UniqueId) -> Option<&InternedString> {
        self.map.get(&key)
    }

    pub fn remove(&mut self, key: UniqueId) -> Option<InternedString> {
        self.map.remove(&key)
    }
}
