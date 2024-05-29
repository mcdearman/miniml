use super::{meta::Meta, meta_context::MetaContext, r#type::Type};
use crate::utils::unique_id::UniqueId;
use itertools::join;
use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct PolyType {
    vars: Vec<Meta>,
    ty: Box<Type>,
}

impl PolyType {
    pub fn new(vars: Vec<Meta>, ty: Type) -> Self {
        Self {
            vars,
            ty: Box::new(ty),
        }
    }

    pub fn free_vars(&self) -> HashSet<Meta> {
        self.ty
            .free_vars()
            .difference(&self.vars.iter().cloned().collect())
            .cloned()
            .collect()
    }

    pub fn instantiate(&self, meta_ctx: &mut MetaContext) -> Type {
        fn substitute(ty: &Type, subst: &HashMap<Meta, Meta>) -> Type {
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
        for tv in self.vars.iter() {
            let new_tv = Meta::fresh();
            // meta_ctx.insert(new_tv.id(), Type::Meta(tv.clone()));
            subst.insert(tv.clone(), new_tv);
        }

        substitute(&self.ty, &subst)
    }
}

impl Display for PolyType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.vars.is_empty() {
            write!(f, "{}", self.ty)
        } else {
            write!(f, "{}. {}", join(&self.vars, " "), self.ty)
        }
    }
}
