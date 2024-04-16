use super::{r#type::Type, scheme::Scheme, substitution::Substitution, ty_var::TyVar};
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

    pub fn from_builtins(builtins: &HashMap<UniqueId, InternedString>) -> Self {
        let mut frame = Frame::new();
        for (id, name) in builtins {
            match name.as_ref() {
                "neg" => {
                    let var = TyVar::fresh();
                    frame.insert(
                        *id,
                        Scheme::new(
                            vec![var],
                            Type::Lambda(vec![Type::Var(var)], Box::new(Type::Var(var))),
                        ),
                    );
                }
                "not" => {
                    frame.insert(
                        *id,
                        Scheme::new(vec![], Type::Lambda(vec![Type::Bool], Box::new(Type::Bool))),
                    );
                }
                "add" => {
                    frame.insert(
                        *id,
                        Scheme::new(
                            vec![],
                            Type::Lambda(vec![Type::Int, Type::Int], Box::new(Type::Int)),
                        ),
                    );
                }
                "sub" => {
                    frame.insert(
                        *id,
                        Scheme::new(
                            vec![],
                            Type::Lambda(vec![Type::Int, Type::Int], Box::new(Type::Int)),
                        ),
                    );
                }
                "mul" => {
                    frame.insert(
                        *id,
                        Scheme::new(
                            vec![],
                            Type::Lambda(
                                vec![Type::Int, Type::Int],
                                Box::new(Type::Int),
                            ),
                        ),
                    );
                }
                "div" => {
                    frame.insert(
                        *id,
                        Scheme::new(
                            vec![],
                            Type::Lambda(
                                vec![Type::Int, Type::Int],
                                Box::new(Type::Int),
                            ),
                        ),
                    );
                }
                "rem" => {
                    frame.insert(
                        *id,
                        Scheme::new(
                            vec![],
                            Type::Lambda(vec![Type::Int, Type::Int], Box::new(Type::Int)),
                        ),
                    );
                }
                "pow" => {
                    let var = TyVar::fresh();
                    frame.insert(
                        *id,
                        Scheme::new(
                            vec![var],
                            Type::Lambda(
                                vec![Type::Var(var), Type::Var(var)],
                                Box::new(Type::Var(var)),
                            ),
                        ),
                    );
                }
                "eq" => {
                    let var = TyVar::fresh();
                    frame.insert(
                        *id,
                        Scheme::new(
                            vec![var],
                            Type::Lambda(
                                vec![Type::Var(var), Type::Var(var)],
                                Box::new(Type::Bool),
                            ),
                        ),
                    );
                }
                "neq" => {
                    let var = TyVar::fresh();
                    frame.insert(
                        *id,
                        Scheme::new(
                            vec![var],
                            Type::Lambda(
                                vec![Type::Var(var), Type::Var(var)],
                                Box::new(Type::Bool),
                            ),
                        ),
                    );
                }
                "lt" => {
                    let var = TyVar::fresh();
                    frame.insert(
                        *id,
                        Scheme::new(
                            vec![var],
                            Type::Lambda(
                                vec![Type::Var(var), Type::Var(var)],
                                Box::new(Type::Bool),
                            ),
                        ),
                    );
                }
                "lte" => {
                    let var = TyVar::fresh();
                    frame.insert(
                        *id,
                        Scheme::new(
                            vec![var],
                            Type::Lambda(
                                vec![Type::Var(var), Type::Var(var)],
                                Box::new(Type::Bool),
                            ),
                        ),
                    );
                }
                "gt" => {
                    let var = TyVar::fresh();
                    frame.insert(
                        *id,
                        Scheme::new(
                            vec![var],
                            Type::Lambda(
                                vec![Type::Var(var), Type::Var(var)],
                                Box::new(Type::Bool),
                            ),
                        ),
                    );
                }
                "gte" => {
                    let var = TyVar::fresh();
                    frame.insert(
                        *id,
                        Scheme::new(
                            vec![var],
                            Type::Lambda(
                                vec![Type::Var(var), Type::Var(var)],
                                Box::new(Type::Bool),
                            ),
                        ),
                    );
                }
                "println" => {
                    let var = TyVar::fresh();
                    frame.insert(
                        *id,
                        Scheme::new(
                            vec![var],
                            Type::Lambda(vec![Type::Var(var)], Box::new(Type::Unit)),
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

    pub fn get(&self, id: &UniqueId) -> Option<Scheme> {
        self.frames.iter().rev().find_map(|frame| frame.get(id))
    }

    pub fn insert(&mut self, id: UniqueId, scheme: Scheme) {
        if let Some(frame) = self.frames.last_mut() {
            frame.insert(id, scheme);
        } else {
            let mut frame = Frame::new();
            frame.insert(id, scheme);
            self.frames.push(frame);
        }
    }

    pub(super) fn apply_subst(&self, subst: &Substitution) -> Self {
        Self {
            frames: self
                .frames
                .iter()
                .map(|frame| frame.apply_subst(subst))
                .collect(),
        }
    }

    pub(super) fn free_vars(&self) -> HashSet<TyVar> {
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
    bindings: HashMap<UniqueId, Scheme>,
}

impl Frame {
    pub fn new() -> Self {
        Self {
            bindings: HashMap::new(),
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = (&UniqueId, &Scheme)> {
        self.bindings.iter()
    }

    pub fn get(&self, id: &UniqueId) -> Option<Scheme> {
        self.bindings.get(id).cloned()
    }

    pub fn insert(&mut self, id: UniqueId, scheme: Scheme) {
        self.bindings.insert(id, scheme);
    }

    pub(super) fn apply_subst(&self, subst: &Substitution) -> Self {
        Self {
            bindings: self
                .bindings
                .clone()
                .into_iter()
                .map(|(id, scheme)| (id, scheme.apply_subst(subst)))
                .collect(),
        }
    }

    pub(super) fn free_vars(&self) -> HashSet<TyVar> {
        self.clone()
            .bindings
            .into_iter()
            .map(|(_, scheme)| scheme.free_vars())
            .fold(HashSet::new(), |acc, set| {
                acc.union(&set).cloned().collect()
            })
    }
}
