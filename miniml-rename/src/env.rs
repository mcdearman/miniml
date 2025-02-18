use miniml_nir::{res_id::ResId, scoped_intern::ScopedInterner};
use miniml_utils::intern::InternedString;
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub struct Env {
    frames: Vec<Frame>,
}

impl Env {
    pub fn new() -> Self {
        Self {
            frames: vec![Frame::new()],
        }
    }

    pub fn new_with_builtins(builtins: HashMap<ResId, InternedString>) -> Self {
        let mut bindings = HashMap::new();

        for (id, name) in builtins {
            bindings.insert(name, id);
        }

        Self {
            frames: vec![Frame { bindings }],
        }
    }

    pub fn dump_to_interner(&self) -> ScopedInterner {
        ScopedInterner::from_iter(self.flatten().into_iter().map(|(k, v)| (v, k)))
    }

    fn flatten(&self) -> HashMap<InternedString, ResId> {
        self.frames
            .iter()
            .rev()
            .flat_map(|frame| frame.bindings.iter())
            .map(|(name, id)| (name.clone(), *id))
            .collect()
    }

    pub fn push(&mut self) {
        self.frames.push(Frame::new());
    }

    pub fn pop(&mut self) {
        self.frames.pop();
    }

    pub fn define(&mut self, name: InternedString) -> ResId {
        if let Some(frame) = self.frames.last_mut() {
            frame.define(name)
        } else {
            let mut frame = Frame::new();
            let id = frame.define(name);
            self.frames.push(frame);
            id
        }
    }

    pub fn push_and_define(&mut self, name: InternedString) -> ResId {
        let mut frame = Frame::new();
        let id = frame.define(name);
        self.frames.push(frame);
        id
    }

    pub fn find(&self, name: &InternedString) -> Option<ResId> {
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
    bindings: HashMap<InternedString, ResId>,
}

impl Frame {
    pub fn new() -> Self {
        Self {
            bindings: HashMap::new(),
        }
    }

    pub fn define(&mut self, name: InternedString) -> ResId {
        let id = ResId::gen();
        self.bindings.insert(name, id);
        id
    }

    pub fn insert(&mut self, name: InternedString, id: ResId) {
        self.bindings.insert(name, id);
    }

    pub fn get(&self, name: &InternedString) -> Option<ResId> {
        self.bindings.get(name).copied()
    }
}
