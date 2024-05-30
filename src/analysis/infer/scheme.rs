use super::{meta::Meta, meta_context::MetaContext, r#type::Type};
use crate::utils::unique_id::UniqueId;
use itertools::join;
use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct PolyType {
    metas: Vec<UniqueId>,
    ty: Box<Type>,
}

impl PolyType {
    pub fn new(metas: Vec<UniqueId>, ty: Type) -> Self {
        Self {
            metas,
            ty: Box::new(ty),
        }
    }

    pub fn free_vars(&self) -> HashSet<UniqueId> {
        self.ty
            .free_vars()
            .difference(&self.metas.iter().cloned().collect())
            .cloned()
            .collect()
    }

    pub fn instantiate(&self, meta_ctx: &mut MetaContext) -> Type {
        fn substitute(ty: &Type, subst: &HashMap<UniqueId, UniqueId>) -> Type {
            match ty {
                Type::Meta(tv) => subst
                    .get(tv)
                    .cloned()
                    .map_or(ty.clone(), |tv| Type::Meta(tv)),
                Type::Lambda(params, body) => Type::Lambda(
                    params.iter().map(|ty| substitute(ty, subst)).collect(),
                    Box::new(substitute(body, subst)),
                ),
                Type::List(ty) => Type::List(Box::new(substitute(ty, subst))),
                Type::Record(id, fields) => Type::Record(
                    *id,
                    fields
                        .iter()
                        .map(|(name, ty)| (name.clone(), substitute(ty, subst)))
                        .collect(),
                ),
                _ => ty.clone(),
            }
        }

        let mut subst = HashMap::new();
        for m in self.metas.iter() {
            let id = meta_ctx.fresh();
            subst.insert(*m, id);
        }

        substitute(&self.ty, &subst)
    }
}

impl Display for PolyType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.metas.is_empty() {
            write!(f, "{}", self.ty)
        } else {
            write!(f, "{}. {}", join(&self.metas, " "), self.ty)
        }
    }
}
