use super::{
    meta::Meta,
    meta_context::{MetaContext, MetaId},
    poly_type::PolyType,
    r#type::Type,
};
use crate::utils::intern::InternedString;
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
        builtins: &HashMap<InternedString, InternedString>,
        meta_ctx: &mut MetaContext,
    ) -> Self {
        let mut frame = Frame::new();
        for (id, name) in builtins {
            match name.as_ref() {
                "neg" => {
                    let vid = meta_ctx.fresh();
                    frame.insert(
                        *id,
                        PolyType::new(
                            vec![vid],
                            Type::Lambda(vec![Type::Meta(vid)], Box::new(Type::Meta(vid))),
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
                    let vid = meta_ctx.fresh();
                    frame.insert(
                        *id,
                        PolyType::new(
                            vec![vid],
                            Type::Lambda(
                                vec![Type::Meta(vid), Type::Meta(vid)],
                                Box::new(Type::Meta(vid)),
                            ),
                        ),
                    );
                }
                "eq" => {
                    let vid = meta_ctx.fresh();
                    frame.insert(
                        *id,
                        PolyType::new(
                            vec![vid],
                            Type::Lambda(
                                vec![Type::Meta(vid), Type::Meta(vid)],
                                Box::new(Type::Bool),
                            ),
                        ),
                    );
                }
                "neq" => {
                    let vid = meta_ctx.fresh();
                    frame.insert(
                        *id,
                        PolyType::new(
                            vec![vid],
                            Type::Lambda(
                                vec![Type::Meta(vid), Type::Meta(vid)],
                                Box::new(Type::Bool),
                            ),
                        ),
                    );
                }
                "lt" => {
                    let vid = meta_ctx.fresh();
                    frame.insert(
                        *id,
                        PolyType::new(
                            vec![vid],
                            Type::Lambda(
                                vec![Type::Meta(vid), Type::Meta(vid)],
                                Box::new(Type::Bool),
                            ),
                        ),
                    );
                }
                "lte" => {
                    let vid = meta_ctx.fresh();
                    frame.insert(
                        *id,
                        PolyType::new(
                            vec![vid],
                            Type::Lambda(
                                vec![Type::Meta(vid), Type::Meta(vid)],
                                Box::new(Type::Bool),
                            ),
                        ),
                    );
                }
                "gt" => {
                    let vid = meta_ctx.fresh();
                    frame.insert(
                        *id,
                        PolyType::new(
                            vec![vid],
                            Type::Lambda(
                                vec![Type::Meta(vid), Type::Meta(vid)],
                                Box::new(Type::Bool),
                            ),
                        ),
                    );
                }
                "gte" => {
                    let vid = meta_ctx.fresh();
                    frame.insert(
                        *id,
                        PolyType::new(
                            vec![vid],
                            Type::Lambda(
                                vec![Type::Meta(vid), Type::Meta(vid)],
                                Box::new(Type::Bool),
                            ),
                        ),
                    );
                }
                "println" => {
                    let vid = meta_ctx.fresh();
                    frame.insert(
                        *id,
                        PolyType::new(
                            vec![vid],
                            Type::Lambda(vec![Type::Meta(vid)], Box::new(Type::Unit)),
                        ),
                    );
                }
                "pair" => {
                    let vid = meta_ctx.fresh();
                    let ty = Type::Meta(vid);
                    frame.insert(
                        *id,
                        PolyType::new(
                            vec![vid],
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

    pub(super) fn free_vars(&self) -> HashSet<MetaId> {
        self.frames
            .iter()
            .map(|frame| frame.free_vars())
            .fold(HashSet::new(), |acc, set| {
                acc.union(&set).cloned().collect()
            })
    }

    pub fn zonk(&self, meta_ctx: &mut MetaContext) -> Self {
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

    pub fn get(&self, id: &InternedString) -> Option<PolyType> {
        self.bindings.get(id).cloned()
    }

    pub fn insert(&mut self, id: InternedString, scheme: PolyType) {
        self.bindings.insert(id, scheme);
    }

    pub(super) fn free_vars(&self) -> HashSet<MetaId> {
        self.clone()
            .bindings
            .into_iter()
            .map(|(_, scheme)| scheme.free_vars())
            .fold(HashSet::new(), |acc, set| {
                acc.union(&set).cloned().collect()
            })
    }

    pub fn zonk(&self, meta_ctx: &mut MetaContext) -> Self {
        let mut frame = Frame::new();
        for (id, scheme) in self.iter() {
            frame.insert(*id, scheme.zonk(meta_ctx));
        }
        frame
    }
}
