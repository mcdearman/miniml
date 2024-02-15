use super::{r#type::Type, ty_var::TyVar};
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
struct Substitution {
    map: HashMap<TyVar, Type>,
}

impl Substitution {
    fn new() -> Self {
        Self {
            map: HashMap::new(),
        }
    }

    fn get(&self, var: &TyVar) -> Option<&Type> {
        self.map.get(var)
    }

    fn insert(&mut self, var: TyVar, ty: Type) {
        self.map.insert(var, ty);
    }

    fn remove(&mut self, var: &TyVar) -> Option<Type> {
        self.map.remove(var)
    }

    // left-biased union
    fn union(&self, other: Self) -> Self {
        self.clone()
            .into_iter()
            .chain(other.clone().into_iter())
            .collect()
    }

    fn compose(&self, other: Substitution) -> Self {
        self.union(
            other
                .into_iter()
                .map(|(var, ty)| (var, ty.apply_subst(self.clone())))
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
