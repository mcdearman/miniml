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
        fn replace_tvs(ty: &Type, from: &UniqueId, to: &Type) -> Type {
            match ty {
                Type::Meta(tv) if tv.id() == *from => to.clone(),
                Type::Lambda(params, body) => {
                    let new_params = params.iter().map(|ty| replace_tvs(ty, from, to)).collect();
                    let new_body = Box::new(replace_tvs(body, from, to));
                    Type::Lambda(new_params, new_body)
                }
                Type::List(ty) => Type::List(Box::new(replace_tvs(ty, from, to))),
                Type::Record(id, fields) => {
                    let new_fields = fields
                        .iter()
                        .map(|(name, ty)| (name.clone(), replace_tvs(ty, from, to)))
                        .collect();
                    Type::Record(*id, new_fields)
                }
                _ => ty.clone(),
            }
        }

        // let mut ty = self.ty.as_ref().clone();
        // for tv in self.vars.iter() {
        //     let meta = Meta::fresh();
        //     log::debug!("instantiate: replacing {:?} with {:?}", tv, meta);
        //     ty = replace_tvs(&ty, &tv.id(), &Type::Meta(meta.clone()));
        //     log::debug!("type after replacement: {:?}", ty);
        //     meta_ctx.insert(meta.id(), Type::Meta(tv.clone()));
        // }

        // ty
        replace_tvs(&self.ty, &UniqueId::new(0), &Type::Meta(Meta::fresh()))
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
