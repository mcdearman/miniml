use miniml_tir::scheme::Scheme;
use miniml_utils::intern::InternedString;
use std::collections::{HashMap, HashSet};

#[derive(Debug, Clone, PartialEq)]
pub struct Context {
    // meta_ctx: MetaContext,
    frames: Vec<Frame>,
}

impl Context {
    pub fn new() -> Self {
        Self {
            frames: vec![Frame::new()],
        }
    }

    pub fn push(&mut self) {
        self.frames.push(Frame::new());
    }

    pub fn pop(&mut self) {
        self.frames.pop();
    }

    pub fn get(&self, id: &InternedString) -> Option<Scheme> {
        self.frames.iter().rev().find_map(|frame| frame.get(id))
    }

    pub fn insert(&mut self, id: InternedString, scheme: Scheme) {
        if let Some(frame) = self.frames.last_mut() {
            frame.insert(id, scheme);
        } else {
            let mut frame = Frame::new();
            frame.insert(id, scheme);
            self.frames.push(frame);
        }
    }

    pub(super) fn free_vars(&self) -> HashSet<u32> {
        self.frames
            .iter()
            .map(|frame| frame.free_vars())
            .fold(HashSet::new(), |acc, set| {
                acc.union(&set).cloned().collect()
            })
    }

    pub fn zonk(&self) -> Self {
        Self {
            frames: self.frames.iter().map(|frame| frame.zonk()).collect(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Frame {
    bindings: HashMap<InternedString, Scheme>,
}

impl Frame {
    pub fn new() -> Self {
        Self {
            bindings: HashMap::new(),
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = (&InternedString, &Scheme)> {
        self.bindings.iter()
    }

    pub fn get(&self, name: &InternedString) -> Option<Scheme> {
        self.bindings.get(name).cloned()
    }

    pub fn insert(&mut self, name: InternedString, scheme: Scheme) {
        self.bindings.insert(name, scheme);
    }

    pub(super) fn free_vars(&self) -> HashSet<u32> {
        // self.clone()
        //     .bindings
        //     .into_iter()
        //     .map(|(_, scheme)| scheme.free_vars(meta_ctx))
        //     .fold(HashSet::new(), |acc, set| {
        //         acc.union(&set).cloned().collect()
        //     })
        todo!()
    }

    pub fn zonk(&self) -> Self {
        let mut frame = Frame::new();
        for (name, scheme) in self.iter() {
            frame.insert(*name, scheme.zonk());
        }
        frame
    }
}
