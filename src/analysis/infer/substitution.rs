use super::{r#type::Type, ty_var::TyVar};
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub struct Substitution {
    map: HashMap<TyVar, Type>,
}

impl Substitution {
    pub fn new() -> Self {
        Self {
            map: HashMap::new(),
        }
    }

    pub fn get(&self, var: &TyVar) -> Option<&Type> {
        self.map.get(var)
    }

    pub fn insert(&mut self, var: TyVar, ty: Type) {
        self.map.insert(var, ty);
    }

    pub fn remove(&mut self, var: &TyVar) -> Option<Type> {
        self.map.remove(var)
    }

    // left-biased union
    fn union(&self, other: Self) -> Self {
        other
            .clone()
            .into_iter()
            .chain(self.clone().into_iter())
            .collect()
    }

    // composeSubst s1 s2 = Map.union (Map.map (applySubst s1) s2) s1
    pub fn compose(&self, other: &Self) -> Self {
        self.union(
            other
                .clone()
                .into_iter()
                .map(|(var, ty)| (var, ty.apply_subst(self)))
                .collect(),
        )
    }
}

impl FromIterator<(TyVar, Type)> for Substitution {
    fn from_iter<T: IntoIterator<Item = (TyVar, Type)>>(iter: T) -> Self {
        Self {
            map: iter.into_iter().collect(),
        }
    }
}

impl IntoIterator for Substitution {
    type Item = (TyVar, Type);
    type IntoIter = std::collections::hash_map::IntoIter<TyVar, Type>;

    fn into_iter(self) -> Self::IntoIter {
        self.map.into_iter()
    }
}
