use super::{context::Context, r#type::Type, substitution::Substitution, ty_var::TyVar};
use std::collections::BTreeSet;

#[derive(Debug, Clone, PartialEq)]
pub struct Scheme {
    vars: Vec<TyVar>,
    ty: Type,
}

impl Scheme {
    pub fn new(vars: Vec<TyVar>, ty: Type) -> Self {
        Self { vars, ty }
    }

    pub fn apply_subst(&self, subst: &Substitution) -> Self {
        let mut new_sub = subst.clone();
        for var in &self.vars {
            new_sub.remove(var);
        }
        Scheme {
            vars: self.vars.clone(),
            ty: self.ty.apply_subst(&new_sub),
        }
    }

    pub fn free_vars(&self) -> BTreeSet<TyVar> {
        self.ty
            .free_vars()
            .difference(&self.vars.iter().cloned().collect())
            .cloned()
            .collect()
    }

    pub fn instantiate(&self) -> Type {
        let mut subst = Substitution::new();
        for var in self.clone().vars {
            subst.insert(var, Type::Var(TyVar::fresh()));
        }
        self.ty.apply_subst(&subst)
    }
}
