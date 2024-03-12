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
                            Type::Lambda(vec![Type::Var(var)], Box::new(Type::Var(var))),
                        ),
                    );
                }
                "not" => {
                    let var = TyVar::fresh();
                    println!("not: {:?}", var);
                    bindings.insert(
                        *id,
                        Scheme::new(
                            vec![var],
                            Type::Lambda(vec![Type::Var(var)], Box::new(Type::Var(var))),
                        ),
                    );
                }
                "add" => {
                    let var = TyVar::fresh();
                    bindings.insert(
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
                "sub" => {
                    let var = TyVar::fresh();
                    bindings.insert(
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
                "mul" => {
                    let var = TyVar::fresh();
                    bindings.insert(
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
                "div" => {
                    let var = TyVar::fresh();
                    bindings.insert(
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
                "rem" => {
                    let var = TyVar::fresh();
                    bindings.insert(
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
                "pow" => {
                    let var = TyVar::fresh();
                    bindings.insert(
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
                    bindings.insert(
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
                    bindings.insert(
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
                    bindings.insert(
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
                    bindings.insert(
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
                    bindings.insert(
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
                    bindings.insert(
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
                "and" => {
                    let var = TyVar::fresh();
                    bindings.insert(
                        *id,
                        Scheme::new(
                            vec![var],
                            Type::Lambda(vec![Type::Bool, Type::Bool], Box::new(Type::Bool)),
                        ),
                    );
                }
                "or" => {
                    let var = TyVar::fresh();
                    bindings.insert(
                        *id,
                        Scheme::new(
                            vec![var],
                            Type::Lambda(vec![Type::Bool, Type::Bool], Box::new(Type::Bool)),
                        ),
                    );
                }
                "println" => {
                    let var = TyVar::fresh();
                    bindings.insert(
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
