use super::{scheme::Scheme, substitution::Substitution, ty_var::TyVar};
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

    pub fn from_builtins(builtins: HashMap<UniqueId, InternedString>) -> Self {
        let mut vars = HashMap::new();
        for (id, name) in builtins {
            match name.as_str() {
                "neg" => vars.insert(id, Scheme::new(vec![], TyVar::new())),
                "add"
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

    pub(super) fn union(&self, other: Self) -> Self {
        Self {
            vars: self
                .vars
                .clone()
                .into_iter()
                .chain(other.vars.clone().into_iter())
                .collect(),
        }
    }

    pub(super) fn apply_subst(&self, subst: Substitution) -> Self {
        Self {
            vars: self
                .vars
                .clone()
                .into_iter()
                .map(|(id, scheme)| (id, scheme.apply_subst(subst.clone())))
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
