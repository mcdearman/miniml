use super::{meta::Meta, meta_context::MetaContext, r#type::Type, scheme::PolyType};
use crate::utils::{intern::InternedString, unique_id::UniqueId};
use std::collections::{HashMap, HashSet};

#[derive(Debug, Clone, PartialEq)]
pub struct Context {
    frames: Vec<Frame>,
}

impl Context {
    pub fn new() -> Self {
        Self {
            frames: vec![Frame::new()],
        }
    }

    pub fn from_builtins(
        builtins: &HashMap<UniqueId, InternedString>,
        meta_ctx: &mut MetaContext,
    ) -> Self {
        let mut frame = Frame::new();
        for (id, name) in builtins {
            match name.as_ref() {
                "neg" => {
                    let meta = Meta::fresh();
                    frame.insert(
                        *id,
                        PolyType::new(
                            vec![meta.clone()],
                            Type::Lambda(
                                vec![Type::Meta(meta.clone())],
                                Box::new(Type::Meta(meta.clone())),
                            ),
                        ),
                    );
                }
                "not" => {
                    frame.insert(
                        *id,
                        PolyType::new(vec![], Type::Lambda(vec![Type::Bool], Box::new(Type::Bool))),
                    );
                }
                "add" => {
                    frame.insert(
                        *id,
                        PolyType::new(
                            vec![],
                            Type::Lambda(vec![Type::Int, Type::Int], Box::new(Type::Int)),
                        ),
                    );
                }
                "sub" => {
                    frame.insert(
                        *id,
                        PolyType::new(
                            vec![],
                            Type::Lambda(vec![Type::Int, Type::Int], Box::new(Type::Int)),
                        ),
                    );
                }
                "mul" => {
                    frame.insert(
                        *id,
                        PolyType::new(
                            vec![],
                            Type::Lambda(vec![Type::Int, Type::Int], Box::new(Type::Int)),
                        ),
                    );
                }
                "div" => {
                    frame.insert(
                        *id,
                        PolyType::new(
                            vec![],
                            Type::Lambda(vec![Type::Int, Type::Int], Box::new(Type::Int)),
                        ),
                    );
                }
                "rem" => {
                    frame.insert(
                        *id,
                        PolyType::new(
                            vec![],
                            Type::Lambda(vec![Type::Int, Type::Int], Box::new(Type::Int)),
                        ),
                    );
                }
                "pow" => {
                    let meta = Meta::fresh();
                    frame.insert(
                        *id,
                        PolyType::new(
                            vec![meta.clone()],
                            Type::Lambda(
                                vec![Type::Meta(meta.clone()), Type::Meta(meta.clone())],
                                Box::new(Type::Meta(meta.clone())),
                            ),
                        ),
                    );
                }
                "eq" => {
                    let meta = Meta::fresh();
                    frame.insert(
                        *id,
                        PolyType::new(
                            vec![meta.clone()],
                            Type::Lambda(
                                vec![Type::Meta(meta.clone()), Type::Meta(meta.clone())],
                                Box::new(Type::Bool),
                            ),
                        ),
                    );
                }
                "neq" => {
                    let meta = Meta::fresh();
                    frame.insert(
                        *id,
                        PolyType::new(
                            vec![meta.clone()],
                            Type::Lambda(
                                vec![Type::Meta(meta.clone()), Type::Meta(meta.clone())],
                                Box::new(Type::Bool),
                            ),
                        ),
                    );
                }
                "lt" => {
                    let meta = Meta::fresh();
                    frame.insert(
                        *id,
                        PolyType::new(
                            vec![meta.clone()],
                            Type::Lambda(
                                vec![Type::Meta(meta.clone()), Type::Meta(meta.clone())],
                                Box::new(Type::Bool),
                            ),
                        ),
                    );
                }
                "lte" => {
                    let meta = Meta::fresh();
                    frame.insert(
                        *id,
                        PolyType::new(
                            vec![meta.clone()],
                            Type::Lambda(
                                vec![Type::Meta(meta.clone()), Type::Meta(meta.clone())],
                                Box::new(Type::Bool),
                            ),
                        ),
                    );
                }
                "gt" => {
                    let meta = Meta::fresh();
                    frame.insert(
                        *id,
                        PolyType::new(
                            vec![meta.clone()],
                            Type::Lambda(
                                vec![Type::Meta(meta.clone()), Type::Meta(meta.clone())],
                                Box::new(Type::Bool),
                            ),
                        ),
                    );
                }
                "gte" => {
                    let meta = Meta::fresh();
                    frame.insert(
                        *id,
                        PolyType::new(
                            vec![meta.clone()],
                            Type::Lambda(
                                vec![Type::Meta(meta.clone()), Type::Meta(meta.clone())],
                                Box::new(Type::Bool),
                            ),
                        ),
                    );
                }
                "println" => {
                    let meta = Meta::fresh();
                    frame.insert(
                        *id,
                        PolyType::new(
                            vec![meta.clone()],
                            Type::Lambda(vec![Type::Meta(meta.clone())], Box::new(Type::Unit)),
                        ),
                    );
                }
                "pair" => {
                    let meta = Meta::fresh();
                    let ty = Type::Meta(meta.clone());
                    frame.insert(
                        *id,
                        PolyType::new(
                            vec![meta.clone()],
                            Type::Lambda(
                                vec![ty.clone(), Type::List(Box::new(ty.clone()))],
                                Box::new(Type::List(Box::new(ty))),
                            ),
                        ),
                    );
                }
                _ => unreachable!("unknown builtin: {}", name),
            }
        }

        Self {
            frames: vec![frame],
        }
    }

    pub fn push(&mut self) {
        self.frames.push(Frame::new());
    }

    pub fn pop(&mut self) {
        self.frames.pop();
    }

    pub fn get(&self, id: &UniqueId) -> Option<PolyType> {
        self.frames.iter().rev().find_map(|frame| frame.get(id))
    }

    pub fn insert(&mut self, id: UniqueId, scheme: PolyType) {
        if let Some(frame) = self.frames.last_mut() {
            frame.insert(id, scheme);
        } else {
            let mut frame = Frame::new();
            frame.insert(id, scheme);
            self.frames.push(frame);
        }
    }

    pub(super) fn free_vars(&self) -> HashSet<Meta> {
        self.frames
            .iter()
            .map(|frame| frame.free_vars())
            .fold(HashSet::new(), |acc, set| {
                acc.union(&set).cloned().collect()
            })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Frame {
    bindings: HashMap<UniqueId, PolyType>,
}

impl Frame {
    pub fn new() -> Self {
        Self {
            bindings: HashMap::new(),
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = (&UniqueId, &PolyType)> {
        self.bindings.iter()
    }

    pub fn get(&self, id: &UniqueId) -> Option<PolyType> {
        self.bindings.get(id).cloned()
    }

    pub fn insert(&mut self, id: UniqueId, scheme: PolyType) {
        self.bindings.insert(id, scheme);
    }

    pub(super) fn free_vars(&self) -> HashSet<Meta> {
        self.clone()
            .bindings
            .into_iter()
            .map(|(_, scheme)| scheme.free_vars())
            .fold(HashSet::new(), |acc, set| {
                acc.union(&set).cloned().collect()
            })
    }
}
