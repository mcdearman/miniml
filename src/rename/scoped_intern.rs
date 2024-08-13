use super::res_name::ResName;
use crate::utils::intern::InternedString;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct ScopedInterner {
    entries: HashMap<ResName, InternedString>,
}

impl ScopedInterner {
    pub fn new() -> Self {
        Self {
            entries: HashMap::new(),
        }
    }

    pub fn intern(&mut self, key: InternedString) -> ResName {
        let id = ResName::gen();
        self.entries.insert(id, key);
        id
    }

    pub fn union(&mut self, other: ScopedInterner) {
        self.entries.extend(other.entries);
    }

    pub fn get(&self, id: ResName) -> Option<&InternedString> {
        self.entries.get(&id)
    }
}
