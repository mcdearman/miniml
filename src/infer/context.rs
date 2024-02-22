use super::{r#type::Type, scheme::Scheme, substitution::Substitution, ty_var::TyVar};
use crate::utils::{intern::InternedString, unique_id::UniqueId};
use std::collections::{BTreeSet, HashMap};

#[derive(Debug, Clone, PartialEq)]
pub struct Context {
    vars: HashMap<UniqueId, Scheme>,
}

impl Context {
    pub fn new() -> Self {
        Self {
            vars: HashMap::new(),
        }
    }

    pub fn from_builtins(builtins: &HashMap<UniqueId, InternedString>) -> Self {
        let mut vars = HashMap::new();
        for (id, name) in builtins {
            match name.as_ref() {
                "neg" => {
                    let var = TyVar::fresh();
                    vars.insert(
                        *id,
                        Scheme::new(
                            vec![var],
                            Type::Lambda(vec![Type::Var(var)], Box::new(Type::Var(var))),
                        ),
                    );
                }
                "not" => {
                    let var = TyVar::fresh();
                    vars.insert(
                        *id,
                        Scheme::new(
                            vec![var],
                            Type::Lambda(vec![Type::Var(var)], Box::new(Type::Var(var))),
                        ),
                    );
                }
                "add" => {
                    let var = TyVar::fresh();
                    vars.insert(
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
                    vars.insert(
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
                    vars.insert(
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
                    vars.insert(
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
                    vars.insert(
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
                    vars.insert(
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
                    vars.insert(
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
                    vars.insert(
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
                    vars.insert(
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
                    vars.insert(
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
                    vars.insert(
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
                    vars.insert(
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
                    vars.insert(
                        *id,
                        Scheme::new(
                            vec![var],
                            Type::Lambda(vec![Type::Bool, Type::Bool], Box::new(Type::Bool)),
                        ),
                    );
                }
                "or" => {
                    let var = TyVar::fresh();
                    vars.insert(
                        *id,
                        Scheme::new(
                            vec![var],
                            Type::Lambda(vec![Type::Bool, Type::Bool], Box::new(Type::Bool)),
                        ),
                    );
                }
                "println" => {
                    let var = TyVar::fresh();
                    vars.insert(
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
        Self { vars }
    }

    pub fn get(&self, id: &UniqueId) -> Option<&Scheme> {
        self.vars.get(id)
    }

    pub(crate) fn extend(&mut self, id: UniqueId, scheme: Scheme) {
        self.vars.insert(id, scheme);
    }

    pub fn union(&self, other: Self) -> Self {
        Self {
            vars: self
                .vars
                .clone()
                .into_iter()
                .chain(other.vars.clone().into_iter())
                .collect(),
        }
    }

    pub(super) fn apply_subst(&self, subst: &Substitution) -> Self {
        Self {
            vars: self
                .vars
                .clone()
                .into_iter()
                .map(|(id, scheme)| (id, scheme.apply_subst(subst)))
                .collect(),
        }
    }

    pub(super) fn free_vars(&self) -> BTreeSet<TyVar> {
        self.clone()
            .vars
            .into_iter()
            .map(|(_, scheme)| scheme.free_vars())
            .fold(BTreeSet::new(), |acc, set| {
                acc.union(&set).cloned().collect()
            })
    }
}
