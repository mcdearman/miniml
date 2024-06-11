use super::{
    meta_context::{MetaContext, MetaId},
    r#type::Type,
};
use crate::utils::unique_id::UniqueId;
use itertools::join;
use std::{
    collections::{HashMap, HashSet},
    fmt::{Debug, Display},
};

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct PolyType {
    metas: Vec<MetaId>,
    ty: Box<Type>,
}

impl PolyType {
    pub fn new(metas: Vec<MetaId>, ty: Type) -> Self {
        Self {
            metas,
            ty: Box::new(ty),
        }
    }

    pub fn zonk(&self, meta_ctx: &mut MetaContext) -> PolyType {
        Self::new(self.metas.clone(), self.ty.zonk(meta_ctx))
    }

    pub fn free_vars(&self) -> HashSet<MetaId> {
        self.ty
            .free_vars()
            .difference(&self.metas.iter().cloned().collect())
            .cloned()
            .collect()
    }

    pub fn instantiate(&self, meta_ctx: &mut MetaContext) -> Type {
        fn substitute(ty: &Type, subst: &HashMap<MetaId, MetaId>) -> Type {
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

        substitute(&meta_ctx.force(&self.ty), &subst)
    }
}

impl Debug for PolyType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.metas.is_empty() {
            write!(f, "{:?}", self.ty)
        } else {
            write!(f, "{:?}. {:?}", self.metas, self.ty)
        }
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
