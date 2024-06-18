use crate::utils::{intern::InternedString, unique_id::UniqueId};
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub struct Env {
    bindings: HashMap<InternedString, usize>,
}

impl Env {
    pub fn new() -> Self {
        Self {
            bindings: HashMap::new(),
        }
    }

    pub fn push(&mut self, name: InternedString) -> InternedString {
        if let Some(level) = self.bindings.get_mut(&name) {
            *level += 1;
            format!("{}{}", name, level).into()
        } else {
            self.bindings.insert(name.clone(), 0);
            format!("{}0", name).into()
        }
    }

    pub fn pop(&mut self, name: InternedString) {
        if let Some(level) = self.bindings.get_mut(&name) {
            if *level == 0 {
                self.bindings.remove(&name);
            } else {
                *level -= 1;
            }
        }
    }

    pub fn find(&self, name: &InternedString) -> Option<InternedString> {
        self.bindings
            .get(name)
            .map(|level| format!("{}{}", name, level).into())
    }
}
