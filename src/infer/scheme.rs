use super::{
    context::Context,
    r#type::{Type, TypeKind},
    substitution::Substitution,
    ty_var::TyVar,
};
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

    pub fn apply_subst(&self, mut subst: Substitution) -> Self {
        for var in self.vars {
            subst.remove(&var);
        }
        Scheme {
            vars: self.vars.clone(),
            ty: self.ty.apply_subst(subst),
        }
    }
    pub fn free_vars(&self) -> BTreeSet<TyVar> {
        self.ty
            .free_vars()
            .difference(self.vars.iter().cloned().collect())
            .cloned()
            .collect()
    }

    pub fn generalize(ctx: Context, ty: Type) -> Self {
        Self {
            vars: ty
                .free_vars()
                .difference(&ctx.free_vars())
                .cloned()
                .collect(),
            ty,
        }
    }

    pub fn instantiate(&self) -> Type {
        let mut subst = Substitution::new();
        for var in self.vars {
            subst.insert(var, Type::new(TypeKind::Var(TyVar::fresh())));
        }
        self.ty.apply_subst(subst)
    }
}
