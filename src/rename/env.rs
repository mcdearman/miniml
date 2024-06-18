use crate::utils::intern::InternedString;
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

    pub fn push(&mut self, name: InternedString) -> usize {
        if let Some(level) = self.bindings.get_mut(&name) {
            *level += 1;
            *level
        } else {
            self.bindings.insert(name.clone(), 0);
            0
        }
    }

    pub fn overwrite(&mut self, name: InternedString) {
        self.bindings.insert(name, 0);
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

    pub fn find(&self, name: &InternedString) -> Option<usize> {
        self.bindings.get(name).map(|level| *level)
    }
}
