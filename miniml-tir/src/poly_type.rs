use super::{ty::Ty, var_context::VarContext};
use crate::ty_var::TyVar;
use itertools::join;
use std::{
    collections::{HashMap, HashSet},
    fmt::{Debug, Display},
};

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct PolyType {
    pub vars: Vec<u16>,
    pub ty: Box<Ty>,
}

impl PolyType {
    pub fn new(vars: Vec<u16>, ty: Ty) -> Self {
        Self {
            vars,
            ty: Box::new(ty),
        }
    }

    pub fn zonk(&self, meta_ctx: &mut VarContext) -> PolyType {
        Self::new(self.vars.clone(), self.ty.zonk(meta_ctx))
    }

    pub fn free_vars(&self, meta_ctx: &VarContext) -> HashSet<u16> {
        // self.ty
        //     .free_vars(meta_ctx)
        //     .difference(&self.vars.iter().cloned().collect())
        //     .cloned()
        //     .collect()
        todo!()
    }

    pub fn instantiate(&self, meta_ctx: &mut VarContext) -> Ty {
        //     fn substitute(ty: &Ty, subst: &HashMap<u32, Ty>, meta_ctx: &mut VarContext) -> Ty {
        //         match ty {
        //             Ty::Var(id) => match meta_ctx.get(id) {
        //                 Some(TyVar::Bound(ty)) => substitute(&ty, subst, meta_ctx),
        //                 Some(TyVar::Unbound(tv)) => match subst.get(&tv) {
        //                     Some(t) => t.clone(),
        //                     None => ty.clone(),
        //                 },
        //                 None => ty.clone(),
        //             },
        //             Ty::Arrow(param, body) => Ty::Arrow(
        //                 Box::new(substitute(param, subst, meta_ctx)),
        //                 Box::new(substitute(body, subst, meta_ctx)),
        //             ),
        //             Ty::List(ty) => Ty::List(Box::new(substitute(ty, subst, meta_ctx))),
        //             _ => ty.clone(),
        //         }
        //     }

        //     let mut subst = HashMap::new();
        //     for m in self.vars.iter() {
        //         subst.insert(*m, Ty::Var(meta_ctx.fresh()));
        //     }

        //     let ty = meta_ctx.force(&self.ty);
        //     let inst = substitute(&ty, &subst, meta_ctx);
        //     log::debug!("instantiate: {:?} -- {:?}", self, inst);
        //     inst
        todo!()
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
