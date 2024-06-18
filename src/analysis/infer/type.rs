use super::{
    context::Context,
    meta_context::{MetaContext, MetaId},
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
    Meta(MetaId),
    Poly(PolyType),
    Lambda(Vec<Self>, Box<Self>),
    // Qual(Box<Constraint>, Box<Self>),
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

    pub fn generalize(&self, ctx: &Context) -> PolyType {
        log::debug!("generalize: {:?}", self);
        log::debug!("free vars: {:?}", self.free_vars());
        log::debug!("ctx free vars: {:?}", ctx.free_vars());
        PolyType::new(
            self.free_vars()
                .difference(&ctx.free_vars())
                .cloned()
                .collect(),
            self.clone(),
        )
    }

    pub(super) fn free_vars(&self) -> HashSet<MetaId> {
        match self {
            Self::Meta(n) => vec![n.clone()].into_iter().collect(),
            Self::Lambda(params, body) => params
                .iter()
                .fold(HashSet::new(), |acc, ty| {
                    acc.union(&ty.free_vars()).cloned().collect()
                })
                .union(&body.free_vars())
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
            m @ Type::Meta(_) => m,
            Type::Poly(poly) => Type::Poly(poly.clone()),
            Type::Lambda(params, body) => Type::Lambda(
                params.iter().map(|ty| ty.zonk(meta_ctx)).collect_vec(),
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
        write!(f, "{}", self)
    }
    // fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    //     match self.clone() {
    //         Self::Byte => write!(f, "Byte"),
    //         Self::Int => write!(f, "Int"),
    //         Self::Rational => write!(f, "Rational"),
    //         Self::Real => write!(f, "Real"),
    //         Self::Bool => write!(f, "Bool"),
    //         Self::String => write!(f, "String"),
    //         Self::Char => write!(f, "Char"),
    //         Self::Meta(n) => write!(f, "{:?}", n),
    //         Self::Poly(poly) => write!(f, "{:?}", poly),
    //         Self::Lambda(params, body) => {
    //             if params.len() == 1 {
    //                 write!(f, "{:?} -> {:?}", params[0], body)
    //             } else {
    //                 write!(f, "({:?}) -> {:?}", params.iter().format(", "), body)
    //             }
    //         }
    //         Self::List(ty) => write!(f, "[{:?}]", ty),
    //         Self::Record(name, fields) => write!(f, "{:?} = {:?}", name, fields),
    //         Self::Unit => write!(f, "()"),
    //     }
    // }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fn lower(ty: Type, metas: &mut HashMap<MetaId, MetaId>) -> Type {
            match ty {
                Type::Byte
                | Type::Int
                | Type::Rational
                | Type::Real
                | Type::Bool
                | Type::String
                | Type::Char
                | Type::Unit => ty,
                Type::Meta(id) => {
                    if let Some(lowered) = metas.get(&id) {
                        Type::Meta(lowered.clone())
                    } else {
                        let new_id = MetaId::gen();
                        metas.insert(id, new_id);
                        Type::Meta(id)
                    }
                }
                Type::Poly(poly) => Type::Poly(poly),
                Type::Lambda(params, body) => Type::Lambda(
                    params.iter().map(|p| lower(p.clone(), metas)).collect_vec(),
                    Box::new(lower(*body, metas)),
                ),
                Type::List(list_ty) => Type::List(Box::new(lower(*list_ty, metas))),
                Type::Record(name, fields) => {
                    let mut lowered_fields = vec![];
                    for (k, v) in fields {
                        lowered_fields.push((k.clone(), lower(v, metas)));
                    }
                    Type::Record(name, lowered_fields)
                }
            }
        }

        let mut vars = HashMap::new();
        match lower(self.clone(), &mut vars) {
            Self::Byte => write!(f, "Byte"),
            Self::Int => write!(f, "Int"),
            Self::Rational => write!(f, "Rational"),
            Self::Real => write!(f, "Real"),
            Self::Bool => write!(f, "Bool"),
            Self::String => write!(f, "String"),
            Self::Char => write!(f, "Char"),
            Self::Meta(n) => write!(f, "{}", n),
            Self::Poly(poly) => write!(f, "{}", poly),
            Self::Lambda(params, body) => {
                if params.len() == 1 {
                    write!(f, "{} -> {}", params[0], body)
                } else {
                    write!(f, "({}) -> {}", params.iter().format(", "), body)
                }
            }
            Self::List(ty) => write!(f, "[{}]", ty),
            Self::Record(name, fields) => write!(f, "{:?} = {:?}", name, fields),
            Self::Unit => write!(f, "()"),
        }
    }
}
