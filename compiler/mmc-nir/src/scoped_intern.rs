use super::res_id::ResId;
use miniml_utils::intern::InternedString;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct ScopedInterner {
    entries: HashMap<ResId, InternedString>,
}

impl ScopedInterner {
    pub fn new() -> Self {
        Self {
            entries: HashMap::new(),
        }
    }

    pub fn intern(&mut self, key: InternedString) -> ResId {
        let id = ResId::gen();
        self.entries.insert(id, key);
        id
    }

    pub fn union(&mut self, other: ScopedInterner) {
        self.entries.extend(other.entries);
    }

    pub fn get(&self, id: ResId) -> Option<&InternedString> {
        self.entries.get(&id)
    }
}

impl FromIterator<(ResId, InternedString)> for ScopedInterner {
    fn from_iter<T: IntoIterator<Item = (ResId, InternedString)>>(iter: T) -> Self {
        Self {
            entries: iter.into_iter().collect(),
        }
    }
}
