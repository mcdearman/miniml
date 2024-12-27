use miniml_tir::{poly_type::PolyType, var_context::VarContext};
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

    pub fn get(&self, id: &InternedString) -> Option<PolyType> {
        self.frames.iter().rev().find_map(|frame| frame.get(id))
    }

    pub fn insert(&mut self, id: InternedString, scheme: PolyType) {
        if let Some(frame) = self.frames.last_mut() {
            frame.insert(id, scheme);
        } else {
            let mut frame = Frame::new();
            frame.insert(id, scheme);
            self.frames.push(frame);
        }
    }

    pub(super) fn free_vars(&self, meta_ctx: &VarContext) -> HashSet<MetaId> {
        self.frames
            .iter()
            .map(|frame| frame.free_vars(meta_ctx))
            .fold(HashSet::new(), |acc, set| {
                acc.union(&set).cloned().collect()
            })
    }

    pub fn zonk(&self, meta_ctx: &mut VarContext) -> Self {
        Self {
            frames: self
                .frames
                .iter()
                .map(|frame| frame.zonk(meta_ctx))
                .collect(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Frame {
    bindings: HashMap<InternedString, PolyType>,
}

impl Frame {
    pub fn new() -> Self {
        Self {
            bindings: HashMap::new(),
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = (&InternedString, &PolyType)> {
        self.bindings.iter()
    }

    pub fn get(&self, name: &InternedString) -> Option<PolyType> {
        self.bindings.get(name).cloned()
    }

    pub fn insert(&mut self, name: InternedString, scheme: PolyType) {
        self.bindings.insert(name, scheme);
    }

    pub(super) fn free_vars(&self, meta_ctx: &VarContext) -> HashSet<MetaId> {
        self.clone()
            .bindings
            .into_iter()
            .map(|(_, scheme)| scheme.free_vars(meta_ctx))
            .fold(HashSet::new(), |acc, set| {
                acc.union(&set).cloned().collect()
            })
    }

    pub fn zonk(&self, meta_ctx: &mut VarContext) -> Self {
        let mut frame = Frame::new();
        for (name, scheme) in self.iter() {
            frame.insert(*name, scheme.zonk(meta_ctx));
        }
        frame
    }
}
