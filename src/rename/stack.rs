use crate::utils::{intern::InternedString, unique_id::UniqueId};
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub struct Stack {
    frames: Vec<Frame>,
}

impl Stack {
    pub fn new() -> Self {
        Self {
            frames: vec![Frame::new()],
        }
    }

    pub fn new_with_builtins(builtins: HashMap<UniqueId, InternedString>) -> Self {
        let mut bindings = HashMap::new();

        for (id, name) in builtins {
            bindings.insert(name, id);
        }

        Self {
            frames: vec![Frame { bindings }],
        }
    }

    pub fn push(&mut self) -> Frame {
        let frame = Frame::new();
        self.frames.push(frame);
        frame
    }

    pub fn pop(&mut self) {
        self.frames.pop();
    }

    pub fn define(&mut self, name: InternedString) -> Option<UniqueId> {
        self.frames.last_mut().map(|frame| frame.define(name))
    }

    pub fn push_and_define(&mut self, name: InternedString) -> UniqueId {
        let mut frame = self.push();
        frame.define(name)
    }

    pub fn find(&self, name: &InternedString) -> Option<UniqueId> {
        for frame in self.frames.iter().rev() {
            if let Some(id) = frame.get(name) {
                return Some(id);
            }
        }
        None
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Frame {
    bindings: HashMap<InternedString, UniqueId>,
}

impl Frame {
    pub fn new() -> Self {
        Self {
            bindings: HashMap::new(),
        }
    }

    pub fn define(&mut self, name: InternedString) -> UniqueId {
        let id = UniqueId::gen();
        self.bindings.insert(name, id);
        id
    }

    pub fn insert(&mut self, name: InternedString, id: UniqueId) {
        self.bindings.insert(name, id);
    }

    pub fn get(&self, name: &InternedString) -> Option<UniqueId> {
        self.bindings.get(name).copied()
    }
}
