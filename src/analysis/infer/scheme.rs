use super::{r#type::Type, ty_var::TyVar};
use std::collections::HashSet;

#[derive(Debug, Clone, PartialEq)]
pub struct Scheme {
    vars: Vec<TyVar>,
    ty: Type,
}

impl Scheme {
    pub fn new(vars: Vec<TyVar>, ty: Type) -> Self {
        Self { vars, ty }
    }

    pub fn free_vars(&self) -> HashSet<TyVar> {
        self.ty
            .free_vars()
            .difference(&self.vars.iter().cloned().collect())
            .cloned()
            .collect()
    }

    pub fn instantiate(&self) -> Type {
        for var in self.clone().vars {
            subst.insert(var, Type::Var(TyVar::fresh()));
        }
        self.ty.apply_subst(&subst)
    }
}
