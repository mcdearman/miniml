use super::{intern::InternedString, unique_id::UniqueId};
use std::collections::HashMap;

#[derive(Debug)]
pub struct ScopedInterner {
    entries: HashMap<UniqueId, InternedString>,
}

impl ScopedInterner {
    pub fn new() -> Self {
        Self {
            entries: HashMap::new(),
        }
    }

    pub fn intern(&mut self, key: InternedString) -> UniqueId {
        let id = UniqueId::gen();
        self.entries.insert(id, key);
        id
    }

    pub fn union(&mut self, other: ScopedInterner) {
        self.entries.extend(other.entries);
    }

    pub fn get(&self, id: UniqueId) -> Option<&InternedString> {
        self.entries.get(&id)
    }
}

impl FromIterator<(UniqueId, InternedString)> for ScopedInterner {
    fn from_iter<T: IntoIterator<Item = (UniqueId, InternedString)>>(iter: T) -> Self {
        Self {
            entries: iter.into_iter().collect(),
        }
    }
}
