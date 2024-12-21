use super::{meta::MetaId, meta_context::MetaContext, ty::Ty};
use crate::meta::Meta;
use itertools::join;
use std::{
    collections::{HashMap, HashSet},
    fmt::{Debug, Display},
};

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct PolyType {
    pub vars: Vec<MetaId>,
    pub ty: Box<Ty>,
}

impl PolyType {
    pub fn new(vars: Vec<MetaId>, ty: Ty) -> Self {
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

    pub fn instantiate(&self, meta_ctx: &mut MetaContext) -> Ty {
        fn substitute(ty: &Ty, subst: &HashMap<u32, Ty>, meta_ctx: &mut MetaContext) -> Ty {
            match ty {
                Ty::MetaRef(id) => match meta_ctx.get(id) {
                    Some(Meta::Bound(ty)) => substitute(&ty, subst, meta_ctx),
                    Some(Meta::Unbound(tv)) => match subst.get(&tv) {
                        Some(t) => t.clone(),
                        None => ty.clone(),
                    },
                    None => ty.clone(),
                },
                Ty::Lambda(param, body) => Ty::Lambda(
                    Box::new(substitute(param, subst, meta_ctx)),
                    Box::new(substitute(body, subst, meta_ctx)),
                ),
                Ty::List(ty) => Ty::List(Box::new(substitute(ty, subst, meta_ctx))),
                _ => ty.clone(),
            }
        }

        let mut subst = HashMap::new();
        for m in self.vars.iter() {
            subst.insert(*m, Ty::MetaRef(meta_ctx.fresh()));
        }

        let ty = meta_ctx.force(&self.ty);
        let inst = substitute(&ty, &subst, meta_ctx);
        log::debug!("instantiate: {:?} -- {:?}", self, inst);
        inst
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

impl Display for PolyType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.vars.is_empty() {
            write!(f, "{}", self.ty)
        } else {
            write!(f, "{}. {}", join(&self.vars, " "), self.ty)
        }
    }
}
