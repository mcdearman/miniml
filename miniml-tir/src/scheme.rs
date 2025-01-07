use super::ty::Ty;
use crate::ty_var::{TyVar, VarId};
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

    pub fn zonk(&self) -> Scheme {
        Self::new(self.vars.clone(), self.ty.zonk())
    }

    pub fn free_vars(&self) -> HashSet<u16> {
        // self.ty
        //     .free_vars(var_ctx)
        //     .difference(&self.vars.iter().cloned().collect())
        //     .cloned()
        //     .collect()
        todo!()
    }

    pub fn instantiate(&self) -> Ty {
        fn substitute(ty: &Ty, subst: &HashMap<u16, VarId>) -> Ty {
            match ty {
                Ty::Var(id) => match id.get() {
                    TyVar::Bound(ty) => substitute(&ty, subst),
                    TyVar::Unbound(tv) => todo!(),
                },
                Ty::Arrow(param, body) => Ty::Arrow(
                    Box::new(substitute(param, subst)),
                    Box::new(substitute(body, subst)),
                ),
                Ty::List(ty) => Ty::List(Box::new(substitute(ty, subst))),
                _ => ty.clone(),
            }
        }

        let mut subst = HashMap::new();
        for m in self.vars.iter() {
            subst.insert(*m, VarId::fresh());
        }

        let ty = self.ty.force();
        let inst = substitute(&ty, &subst);
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
