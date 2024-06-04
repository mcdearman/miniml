use super::{
    error::{InferResult, TypeError},
    meta::Meta,
    r#type::Type,
};
use crate::utils::unique_id::UniqueId;
use std::{collections::HashMap, fmt::Debug};

#[derive(Debug, Clone, PartialEq)]
pub struct MetaContext {
    bindings: HashMap<UniqueId, Meta>,
}

impl MetaContext {
    pub fn new() -> Self {
        Self {
            bindings: HashMap::new(),
        }
    }

    pub fn fresh(&mut self) -> UniqueId {
        let id = UniqueId::gen();
        self.bindings.insert(id, Meta::fresh());
        id
    }

    pub fn get(&self, id: &UniqueId) -> Option<Type> {
        match self.bindings.get(id) {
            Some(Meta::Bound(ty)) => Some(ty.clone()),
            _ => None,
        }
    }

    pub fn bind(&mut self, id: &UniqueId, ty: &Type) -> InferResult<()> {
        if *ty == Type::Meta(*id) {
            Ok(())
        } else if ty.free_vars().contains(&id) {
            Err(TypeError::from(format!(
                "occurs check failed: {} occurs in {:?}",
                id, ty
            )))
        } else {
            self.bindings.insert(*id, Meta::Bound(ty.clone()));
            Ok(())
        }
    }

    pub fn force(&mut self, ty: &Type) -> Type {
        match ty {
            Type::Meta(id) => match self.clone().bindings.get(id) {
                Some(Meta::Bound(ty)) => {
                    let ty = self.force(ty);
                    self.bindings.insert(*id, Meta::Bound(ty.clone()));
                    ty
                }
                _ => ty.clone(),
            },
            Type::Lambda(params, body) => Type::Lambda(
                params.into_iter().map(|ty| self.force(ty)).collect(),
                Box::new(self.force(body.as_ref())),
            ),
            Type::List(ty) => Type::List(Box::new(self.force(ty.as_ref()))),
            Type::Record(id, fields) => Type::Record(
                *id,
                fields
                    .into_iter()
                    .map(|(name, ty)| (*name, self.force(ty)))
                    .collect(),
            ),
            _ => ty.clone(),
        }
    }

    pub fn unify(&mut self, t1: &Type, t2: &Type) -> InferResult<()> {
        log::debug!("unify: {:?} and {:?}", t1, t2);
        let t1 = self.force(t1);
        let t2 = self.force(t2);

        match (&t1, &t2) {
            (Type::Byte, Type::Byte)
            | (Type::Int, Type::Int)
            | (Type::Rational, Type::Rational)
            | (Type::Real, Type::Real)
            | (Type::Bool, Type::Bool)
            | (Type::String, Type::String)
            | (Type::Char, Type::Char)
            | (Type::Unit, Type::Unit) => Ok(()),
            (Type::Lambda(p1, b1), Type::Lambda(p2, b2)) => {
                if p1.len() != p2.len() {
                    return Err(TypeError::from(format!(
                        "functions have different arity: {:?} and {:?}",
                        t1, t2,
                    )));
                }
                for (t1, t2) in p1.iter().zip(p2.iter()) {
                    self.unify(t1, t2)?;
                }
                self.unify(b1, b2)
            }
            (Type::List(l1), Type::List(l2)) => self.unify(l1, l2),
            (_, Type::Meta(key)) => {
                self.bind(key, &t1)?;
                log::debug!("bind: {:?} to {:?}", key, t1);
                // log::debug!("meta_ctx: {:#?}", self);
                Ok(())
            }
            (Type::Meta(key), _) => {
                self.bind(key, &t2)?;
                log::debug!("bind: {:?} to {:?}", key, t2);
                // log::debug!("meta_ctx: {:#?}", self);
                Ok(())
            }
            _ => Err(TypeError::from(format!(
                "cannot unify {:?} and {:?}",
                t1, t2,
            ))),
        }
    }
}

// impl Debug for MetaContext {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {}
// }
