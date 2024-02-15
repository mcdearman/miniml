use super::{scheme::Scheme, ty_var::TyVar};
use crate::utils::unique_id::UniqueId;
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

    pub fn extend(&mut self, id: UniqueId, scheme: Scheme) {
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
                .map(|(id, scheme)| (id, apply_subst_scheme(subst.clone(), scheme)))
                .collect(),
        }
    }

    pub(super) fn free_vars(&self) -> BTreeSet<TyVar> {
        self.vars
            .into_iter()
            .map(|(_, scheme)| free_vars_scheme(scheme))
            .fold(BTreeSet::new(), |acc, set| {
                acc.union(&set).cloned().collect()
            })
    }
}
