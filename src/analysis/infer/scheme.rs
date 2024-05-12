use crate::utils::unique_id::UniqueId;

use super::{meta::Meta, r#type::Type};
use std::collections::{HashMap, HashSet};

#[derive(Debug, Clone, PartialEq)]
pub struct Scheme {
    vars: Vec<Meta>,
    ty: Type,
}

impl Scheme {
    pub fn new(vars: Vec<Meta>, ty: Type) -> Self {
        Self { vars, ty }
    }

    pub fn free_vars(&self) -> HashSet<Meta> {
        self.ty
            .free_vars()
            .difference(&self.vars.iter().cloned().collect())
            .cloned()
            .collect()
    }

    pub fn instantiate(&self) -> Type {
        fn replace_tvs(ty: &Type, from: &Meta, to: &Type) -> Type {
            match ty {
                Type::Var(tv) if tv == from => to.clone(),
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

        let mut tvs_to_replace = HashMap::new();
        for tv in self.vars.iter() {
            tvs_to_replace.insert(tv.clone(), Type::Var(Meta::fresh()));
        }

        replace_tvs(
            &self.ty,
            &Meta::Unbound(UniqueId::new(0)),
            &Type::Var(Meta::fresh()),
        )
    }
}
