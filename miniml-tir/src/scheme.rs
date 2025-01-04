use super::{ty::Ty, var_context::VarContext};
use crate::{ty_var::TyVar, var_context::VarId};
use itertools::join;
use std::{
    collections::{HashMap, HashSet},
    fmt::{Debug, Display},
};

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Scheme {
    pub vars: Vec<u16>,
    pub ty: Box<Ty>,
}

impl Scheme {
    pub fn new(vars: Vec<u16>, ty: Ty) -> Self {
        Self {
            vars,
            ty: Box::new(ty),
        }
    }

    pub fn zonk(&self, var_ctx: &mut VarContext) -> Scheme {
        Self::new(self.vars.clone(), self.ty.zonk(var_ctx))
    }

    pub fn free_vars(&self, var_ctx: &VarContext) -> HashSet<u16> {
        // self.ty
        //     .free_vars(var_ctx)
        //     .difference(&self.vars.iter().cloned().collect())
        //     .cloned()
        //     .collect()
        todo!()
    }

    pub fn instantiate(&self, var_ctx: &mut VarContext) -> Ty {
        fn substitute(ty: &Ty, subst: &HashMap<u16, VarId>, var_ctx: &mut VarContext) -> Ty {
            match ty {
                Ty::Var(id) => match var_ctx.get(id) {
                    TyVar::Bound(ty) => substitute(&ty, subst, var_ctx),
                    TyVar::Unbound(tv) => todo!(),
                },
                Ty::Arrow(param, body) => Ty::Arrow(
                    Box::new(substitute(param, subst, var_ctx)),
                    Box::new(substitute(body, subst, var_ctx)),
                ),
                Ty::List(ty) => Ty::List(Box::new(substitute(ty, subst, var_ctx))),
                _ => ty.clone(),
            }
        }

        let mut subst = HashMap::new();
        for m in self.vars.iter() {
            subst.insert(*m, var_ctx.fresh());
        }

        let ty = var_ctx.force(&self.ty);
        let inst = substitute(&ty, &subst, var_ctx);
        log::debug!("instantiate: {:?} -- {:?}", self, inst);
        inst
    }
}

impl Debug for Scheme {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.vars.is_empty() {
            write!(f, "{:?}", self.ty)
        } else {
            write!(f, "{:?}. {:?}", self.vars, self.ty)
        }
    }
}

impl Display for Scheme {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.vars.is_empty() {
            write!(f, "{}", self.ty)
        } else {
            write!(f, "{}. {}", join(&self.vars, " "), self.ty)
        }
    }
}
