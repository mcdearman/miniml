use super::{
    context::Context,
    meta::{Meta, MetaId},
    meta_context::{MetaContext, MetaRef},
    poly_type::PolyType,
};
use crate::utils::{intern::InternedString, unique_id::UniqueId};
use itertools::Itertools;
use std::{
    collections::{HashMap, HashSet},
    fmt::{Debug, Display},
};

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Type {
    Byte,
    Int,
    Rational,
    Real,
    Bool,
    String,
    Char,
    MetaRef(MetaRef),
    Meta(Box<Meta>),
    // Poly(PolyType),
    Lambda(Box<Self>, Box<Self>),
    List(Box<Self>),
    Record(UniqueId, Vec<(InternedString, Self)>),
    Unit,
}

impl Type {
    pub(super) fn is_numeric(&self) -> bool {
        match self {
            Self::Byte | Self::Int | Self::Rational | Self::Real => true,
            _ => false,
        }
    }

    pub fn generalize(&self, ctx: &Context, meta_ctx: &MetaContext) -> PolyType {
        log::debug!("generalize: {:?}", self);
        log::debug!("free vars: {:?}", self.free_vars(meta_ctx));
        log::debug!("ctx free vars: {:?}", ctx.free_vars(meta_ctx));
        PolyType::new(
            self.free_vars(meta_ctx)
                .difference(&ctx.free_vars(meta_ctx))
                .cloned()
                .collect(),
            self.clone(),
        )
    }

    pub(super) fn free_vars(&self, meta_ctx: &MetaContext) -> HashSet<MetaId> {
        match self {
            Self::MetaRef(n) => match meta_ctx.get(n) {
                Meta::Bound(ty) => ty.free_vars(meta_ctx),
                Meta::Unbound(tv) => vec![tv].into_iter().collect(),
            },
            Self::Lambda(param, body) => param
                .free_vars(meta_ctx)
                .union(&body.free_vars(meta_ctx))
                .cloned()
                .collect(),
            _ => HashSet::new(),
        }
    }

    pub fn zonk(&self, meta_ctx: &mut MetaContext) -> Type {
        // log::debug!("zonk: {:?}", self);
        match meta_ctx.force(self) {
            Type::Byte => Type::Byte,
            Type::Int => Type::Int,
            Type::Rational => Type::Rational,
            Type::Real => Type::Real,
            Type::Bool => Type::Bool,
            Type::String => Type::String,
            Type::Char => Type::Char,
            Type::Unit => Type::Unit,
            Type::MetaRef(n) => match meta_ctx.get(&n) {
                Meta::Bound(ty) => ty.zonk(meta_ctx),
                Meta::Unbound(tv) => Type::Meta(Box::new(Meta::Unbound(tv))),
            },
            m @ Type::Meta(_) => m,
            // Type::Poly(poly) => Type::Poly(poly.clone()),
            Type::Lambda(param, body) => Type::Lambda(
                Box::new(param.zonk(meta_ctx)),
                Box::new(body.zonk(meta_ctx)),
            ),
            Type::List(ty) => Type::List(Box::new(ty.zonk(meta_ctx))),
            Type::Record(id, fields) => Type::Record(
                id,
                fields
                    .iter()
                    .map(|(name, ty)| (name.clone(), ty.zonk(meta_ctx)))
                    .collect(),
            ),
        }
    }
}

impl Debug for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Byte => write!(f, "Byte"),
            Self::Int => write!(f, "Int"),
            Self::Rational => write!(f, "Rational"),
            Self::Real => write!(f, "Real"),
            Self::Bool => write!(f, "Bool"),
            Self::String => write!(f, "String"),
            Self::Char => write!(f, "Char"),
            Self::MetaRef(n) => write!(f, "{}", n),
            Self::Meta(m) => write!(f, "{:?}", m),
            // Self::Poly(poly) => write!(f, "{:?}", poly),
            Self::Lambda(param, body) => {
                if matches!(**param, Self::Lambda(_, _)) {
                    write!(f, "({:?}) -> {:?}", param, body)
                } else {
                    write!(f, "{:?} -> {:?}", param, body)
                }
            }
            Self::List(ty) => write!(f, "[{:?}]", ty),
            Self::Record(id, fields) => write!(f, "{:?} = {:?}", id, fields),
            Self::Unit => write!(f, "()"),
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fn lower(ty: &Type, metas: &mut HashMap<u32, u32>) -> Type {
            match ty {
                Type::Meta(m) => match m.as_ref() {
                    Meta::Bound(ty) => lower(ty, metas),
                    Meta::Unbound(tv) => {
                        if let Some(lowered) = metas.get(tv) {
                            Type::Meta(Box::new(Meta::Unbound(*lowered)))
                        } else {
                            let id = metas.len() as u32;
                            metas.insert(*tv, id);
                            Type::Meta(Box::new(Meta::Unbound(id)))
                        }
                    }
                },
                Type::Lambda(param, body) => {
                    Type::Lambda(Box::new(lower(param, metas)), Box::new(lower(body, metas)))
                }
                Type::List(list_ty) => Type::List(Box::new(lower(list_ty, metas))),
                // Type::Record(name, fields) => {
                //     let mut lowered_fields = vec![];
                //     for (k, v) in fields {
                //         lowered_fields.push((k.clone(), lower(v, metas)));
                //     }
                //     Type::Record(name, lowered_fields)
                // }
                _ => ty.clone(),
            }
        }

        let mut vars = HashMap::new();
        // log::debug!("lowered: {:?}", lower(self, &mut vars));
        match lower(self, &mut vars) {
            Self::Byte => write!(f, "Byte"),
            Self::Int => write!(f, "Int"),
            Self::Rational => write!(f, "Rational"),
            Self::Real => write!(f, "Real"),
            Self::Bool => write!(f, "Bool"),
            Self::String => write!(f, "String"),
            Self::Char => write!(f, "Char"),
            Self::MetaRef(n) => write!(f, "{}", n),
            Self::Meta(m) => write!(f, "{}", m),
            // Self::Poly(poly) => write!(f, "{}", poly),
            Self::Lambda(param, body) => {
                if matches!(*param, Self::Lambda(_, _)) {
                    write!(f, "({:?}) -> {:?}", param, body)
                } else {
                    write!(f, "{:?} -> {:?}", param, body)
                }
            }
            Self::List(ty) => write!(f, "[{}]", ty),
            Self::Record(name, fields) => write!(f, "{:?} = {:?}", name, fields),
            Self::Unit => write!(f, "()"),
        }
    }
}
