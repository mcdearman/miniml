use crate::utils::{InternedString, UniqueId};
use std::fmt::Debug;

pub trait Database: Debug {
    fn insert(&mut self, key: UniqueId, value: InternedString);
    fn insert_or_get(&mut self, key: UniqueId, value: InternedString) -> InternedString {
        if let Some(value) = self.get(key) {
            value.clone()
        } else {
            self.insert(key, value);
            value
        }
    }
    fn get(&self, key: UniqueId) -> Option<&InternedString>;
    fn remove(&mut self, key: UniqueId) -> Option<InternedString>;
}
