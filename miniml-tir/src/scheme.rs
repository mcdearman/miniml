use super::ty::Ty;
use crate::meta::{Meta, MetaId};
use std::{
    collections::{BTreeSet, HashMap},
    fmt::{Debug, Display},
};

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Scheme {
    pub ty: Box<Ty>,
}

impl Scheme {
    pub fn new(ty: Ty) -> Self {
        Self { ty: Box::new(ty) }
    }

    pub fn zonk(&self) -> Scheme {
        Self::new(self.ty.zonk())
    }

    pub fn free_vars(&self) -> BTreeSet<u32> {
        self.ty.free_vars()
    }

    pub fn instantiate(&self) -> Ty {
        fn substitute(ty: &Ty, subst: &HashMap<u16, MetaId>) -> Ty {
            match ty {
                Ty::Meta(id) => match id.get() {
                    Meta::Bound(ty) => substitute(&ty, subst),
                    Meta::Unbound(tv) => todo!(),
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
            subst.insert(*m, MetaId::fresh());
        }

        let ty = self.ty.force();
        let inst = substitute(&ty, &subst);
        log::debug!("instantiate: {:?} -- {:?}", self, inst);
        inst
    }
}

impl Debug for Scheme {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.ty)
    }
}

impl Display for Scheme {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.ty)
    }
}
