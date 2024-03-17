use super::{r#type::Type, scheme::Scheme, substitution::Substitution, ty_var::TyVar};
use crate::utils::{intern::InternedString, unique_id::UniqueId};
use std::collections::{HashMap, HashSet};

#[derive(Debug, Clone, PartialEq)]
pub struct Context {
    bindings: HashMap<UniqueId, Scheme>,
}

impl Context {
    pub fn new() -> Self {
        Self {
            bindings: HashMap::new(),
        }
    }

    pub fn from_builtins(builtins: &HashMap<UniqueId, InternedString>) -> Self {
        let mut bindings = HashMap::new();
        for (id, name) in builtins {
            match name.as_ref() {
                "neg" => {
                    let var = TyVar::fresh();
                    bindings.insert(
                        *id,
                        Scheme::new(
                            vec![var],
                            Type::Lambda(Box::new(Type::Var(var)), Box::new(Type::Var(var))),
                        ),
                    );
                }
                "not" => {
                    let var = TyVar::fresh();
                    bindings.insert(
                        *id,
                        Scheme::new(
                            vec![var],
                            Type::Lambda(Box::new(Type::Var(var)), Box::new(Type::Var(var))),
                        ),
                    );
                }
                "add" => {
                    bindings.insert(
                        *id,
                        Scheme::new(
                            vec![],
                            Type::Lambda(
                                Box::new(Type::Int),
                                Box::new(Type::Lambda(Box::new(Type::Int), Box::new(Type::Int))),
                            ),
                        ),
                    );
                }
                "sub" => {
                    bindings.insert(
                        *id,
                        Scheme::new(
                            vec![],
                            Type::Lambda(
                                Box::new(Type::Int),
                                Box::new(Type::Lambda(Box::new(Type::Int), Box::new(Type::Int))),
                            ),
                        ),
                    );
                }
                "mul" => {
                    bindings.insert(
                        *id,
                        Scheme::new(
                            vec![],
                            Type::Lambda(
                                Box::new(Type::Int),
                                Box::new(Type::Lambda(Box::new(Type::Int), Box::new(Type::Int))),
                            ),
                        ),
                    );
                }
                "div" => {
                    bindings.insert(
                        *id,
                        Scheme::new(
                            vec![],
                            Type::Lambda(
                                Box::new(Type::Int),
                                Box::new(Type::Lambda(Box::new(Type::Int), Box::new(Type::Int))),
                            ),
                        ),
                    );
                }
                "rem" => {
                    bindings.insert(
                        *id,
                        Scheme::new(
                            vec![],
                            Type::Lambda(
                                Box::new(Type::Int),
                                Box::new(Type::Lambda(Box::new(Type::Int), Box::new(Type::Int))),
                            ),
                        ),
                    );
                }
                "pow" => {
                    bindings.insert(
                        *id,
                        Scheme::new(
                            vec![],
                            Type::Lambda(
                                Box::new(Type::Int),
                                Box::new(Type::Lambda(Box::new(Type::Int), Box::new(Type::Int))),
                            ),
                        ),
                    );
                }
                "eq" => {
                    let var = TyVar::fresh();
                    bindings.insert(
                        *id,
                        Scheme::new(
                            vec![var],
                            Type::Lambda(
                                Box::new(Type::Var(var)),
                                Box::new(Type::Lambda(
                                    Box::new(Type::Var(var)),
                                    Box::new(Type::Bool),
                                )),
                            ),
                        ),
                    );
                }
                "neq" => {
                    let var = TyVar::fresh();
                    bindings.insert(
                        *id,
                        Scheme::new(
                            vec![var],
                            Type::Lambda(
                                Box::new(Type::Var(var)),
                                Box::new(Type::Lambda(
                                    Box::new(Type::Var(var)),
                                    Box::new(Type::Bool),
                                )),
                            ),
                        ),
                    );
                }
                "lt" => {
                    let var = TyVar::fresh();
                    bindings.insert(
                        *id,
                        Scheme::new(
                            vec![var],
                            Type::Lambda(
                                Box::new(Type::Var(var)),
                                Box::new(Type::Lambda(
                                    Box::new(Type::Var(var)),
                                    Box::new(Type::Bool),
                                )),
                            ),
                        ),
                    );
                }
                "lte" => {
                    let var = TyVar::fresh();
                    bindings.insert(
                        *id,
                        Scheme::new(
                            vec![var],
                            Type::Lambda(
                                Box::new(Type::Var(var)),
                                Box::new(Type::Lambda(
                                    Box::new(Type::Var(var)),
                                    Box::new(Type::Bool),
                                )),
                            ),
                        ),
                    );
                }
                "gt" => {
                    let var = TyVar::fresh();
                    bindings.insert(
                        *id,
                        Scheme::new(
                            vec![var],
                            Type::Lambda(
                                Box::new(Type::Var(var)),
                                Box::new(Type::Lambda(
                                    Box::new(Type::Var(var)),
                                    Box::new(Type::Bool),
                                )),
                            ),
                        ),
                    );
                }
                "gte" => {
                    let var = TyVar::fresh();
                    bindings.insert(
                        *id,
                        Scheme::new(
                            vec![var],
                            Type::Lambda(
                                Box::new(Type::Var(var)),
                                Box::new(Type::Lambda(
                                    Box::new(Type::Var(var)),
                                    Box::new(Type::Bool),
                                )),
                            ),
                        ),
                    );
                }
                "println" => {
                    let var = TyVar::fresh();
                    bindings.insert(
                        *id,
                        Scheme::new(
                            vec![var],
                            Type::Lambda(Box::new(Type::Var(var)), Box::new(Type::Unit)),
                        ),
                    );
                }
                _ => unreachable!("unknown builtin: {}", name),
            }
        }

        Self { bindings }
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
