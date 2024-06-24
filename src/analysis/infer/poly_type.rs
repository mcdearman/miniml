use super::{meta::MetaId, meta_context::MetaContext, r#type::Type};
use crate::analysis::infer::meta::Meta;
use itertools::join;
use std::{
    collections::{HashMap, HashSet},
    fmt::{Debug, Display},
};

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct PolyType {
    pub vars: Vec<MetaId>,
    pub ty: Box<Type>,
}

impl PolyType {
    pub fn new(vars: Vec<MetaId>, ty: Type) -> Self {
        Self {
            vars,
            ty: Box::new(ty),
        }
    }

    pub fn zonk(&self, meta_ctx: &mut MetaContext) -> PolyType {
        Self::new(self.vars.clone(), self.ty.zonk(meta_ctx))
    }

    pub fn free_vars(&self, meta_ctx: &MetaContext) -> HashSet<MetaId> {
        self.ty
            .free_vars(meta_ctx)
            .difference(&self.vars.iter().cloned().collect())
            .cloned()
            .collect()
    }

    pub fn instantiate(&self, meta_ctx: &mut MetaContext) -> Type {
        fn substitute(ty: &Type, subst: &HashMap<u32, Type>, meta_ctx: &mut MetaContext) -> Type {
            match ty {
                Type::MetaRef(id) => match meta_ctx.get(id) {
                    Meta::Bound(ty) => substitute(&ty, subst, meta_ctx),
                    Meta::Unbound(tv) => match subst.get(&tv) {
                        Some(t) => t.clone(),
                        None => Type::MetaRef(*id),
                    },
                },
                Type::Lambda(params, body) => Type::Lambda(
                    params
                        .iter()
                        .map(|ty| substitute(ty, subst, meta_ctx))
                        .collect(),
                    Box::new(substitute(body, subst, meta_ctx)),
                ),
                // Type::List(ty) => Type::List(Box::new(substitute(ty, subst, meta_ctx))),
                // Type::Record(id, fields) => Type::Record(
                //     *id,
                //     fields
                //         .iter()
                //         .map(|(name, ty)| (name.clone(), substitute(ty, subst, meta_ctx)))
                //         .collect(),
                // ),
                _ => ty.clone(),
            }
        }

        let mut subst = HashMap::new();
        for m in self.vars.iter() {
            let meta_ref = meta_ctx.fresh();
            match meta_ctx.get(&meta_ref) {
                Meta::Unbound(id) => {
                    subst.insert(id, Type::MetaRef(meta_ref));
                }
                _ => continue,
            }
        }

        substitute(&meta_ctx.force(&self.ty), &subst, meta_ctx)
    }
}

impl Debug for PolyType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.vars.is_empty() {
            write!(f, "{:?}", self.ty)
        } else {
            write!(f, "{:?}. {:?}", self.vars, self.ty)
        }
    }
}

// impl Display for PolyType {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         if self.vars.is_empty() {
//             write!(f, "{}", self.ty)
//         } else {
//             write!(f, "{}. {}", join(&self.vars, " "), self.ty)
//         }
//     }
// }
