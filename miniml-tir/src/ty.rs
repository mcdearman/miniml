use crate::{
    meta::{Meta, MetaId},
    scheme::Scheme,
};
use miniml_utils::{intern::InternedString, unique_id::UniqueId};
use std::{
    collections::{BTreeMap, BTreeSet, HashMap, HashSet},
    fmt::{Debug, Display},
};

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Ty {
    Byte,
    Int,
    Rational,
    Real,
    Bool,
    String,
    Char,
    Meta(MetaId),
    PolyType(Scheme),
    Var(u16),
    Arrow(Box<Self>, Box<Self>),
    List(Box<Self>),
    Array(Box<Self>),
    Record(UniqueId, Vec<(InternedString, Self)>),
    Unit,
}

impl Ty {
    pub fn generalize(&self, ctx_free_vars: BTreeSet<u32>) -> Scheme {
        // log::debug!("generalize: {:?}", self);
        // log::debug!("free vars: {:?}", self.free_vars(meta_ctx));
        // log::debug!("ctx free vars: {:?}", ctx.free_vars(meta_ctx));
        let subst: BTreeMap<u32, Ty> = self
            .free_vars()
            .difference(&ctx_free_vars)
            .cloned()
            .zip((0u16..).map(Ty::Var))
            .collect();

        fn substitute(ty: &Ty, subst: &BTreeMap<u32, Ty>) -> Ty {
            match ty {
                Ty::Meta(id) => match id.get() {
                    Meta::Bound(ty) => substitute(&ty, subst),
                    Meta::Unbound(tv) => subst.get(&tv).unwrap_or(ty).clone(),
                },
                Ty::Arrow(param, body) => Ty::Arrow(
                    Box::new(substitute(param, subst)),
                    Box::new(substitute(body, subst)),
                ),
                Ty::List(ty) => Ty::List(Box::new(substitute(ty, subst))),
                Ty::Array(ty) => Ty::Array(Box::new(substitute(ty, subst))),
                Ty::Record(id, fields) => Ty::Record(
                    *id,
                    fields
                        .iter()
                        .map(|(name, ty)| (name.clone(), substitute(ty, subst)))
                        .collect(),
                ),
                _ => ty.clone(),
            }
        }

        Scheme::new(subst.len() as u16, substitute(self, &subst))
    }

    pub fn free_vars(&self) -> BTreeSet<u32> {
        match self {
            Self::Meta(n) => match n.get() {
                Meta::Bound(ty) => ty.free_vars(),
                Meta::Unbound(tv) => BTreeSet::from([tv]),
            },
            Self::Arrow(param, body) => param
                .free_vars()
                .union(&body.free_vars())
                .cloned()
                .collect(),
            _ => BTreeSet::new(),
        }
    }

    pub fn zonk(&self) -> Ty {
        // log::debug!("zonk: {:?}", self);
        match self.force() {
            Ty::Byte => Ty::Byte,
            Ty::Int => Ty::Int,
            Ty::Rational => Ty::Rational,
            Ty::Real => Ty::Real,
            Ty::Bool => Ty::Bool,
            Ty::String => Ty::String,
            Ty::Char => Ty::Char,
            Ty::Unit => Ty::Unit,
            Ty::Meta(v) => match v.get() {
                Meta::Bound(ty) => ty.zonk(),
                Meta::Unbound(tv) => panic!("unbound type variable: {:?}", tv),
            },
            Ty::PolyType(scheme) => Ty::PolyType(scheme.zonk()),
            Ty::Var(id) => Ty::Var(id),
            Ty::Arrow(param, body) => Ty::Arrow(Box::new(param.zonk()), Box::new(body.zonk())),
            Ty::List(ty) => Ty::List(Box::new(ty.zonk())),
            Ty::Array(ty) => Ty::Array(Box::new(ty.zonk())),
            Ty::Record(id, fields) => Ty::Record(
                id,
                fields
                    .iter()
                    .map(|(name, ty)| (name.clone(), ty.zonk()))
                    .collect(),
            ),
        }
    }

    pub fn force(&self) -> Ty {
        match self {
            Ty::Meta(id) => match id.get() {
                Meta::Bound(ty) => {
                    let ty = ty.force();
                    id.insert(Meta::Bound(ty.clone()));
                    ty
                }
                _ => self.clone(),
            },
            _ => self.clone(),
        }
    }
}

impl Debug for Ty {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Byte => write!(f, "Byte"),
            Self::Int => write!(f, "Int"),
            Self::Rational => write!(f, "Rational"),
            Self::Real => write!(f, "Real"),
            Self::Bool => write!(f, "Bool"),
            Self::String => write!(f, "String"),
            Self::Char => write!(f, "Char"),
            Self::Meta(n) => write!(f, "{}", n),
            Self::PolyType(scheme) => write!(f, "{:?}", scheme),
            Self::Var(id) => write!(f, "Var({})", id),
            Self::Arrow(param, body) => {
                if matches!(**param, Self::Arrow(_, _)) {
                    write!(f, "({:?}) -> {:?}", param, body)
                } else {
                    write!(f, "{:?} -> {:?}", param, body)
                }
            }
            Self::List(ty) => write!(f, "[{:?}]", ty),
            Self::Array(ty) => write!(f, "#[{:?}]", ty),
            Self::Record(id, fields) => write!(f, "{:?} = {:?}", id, fields),
            Self::Unit => write!(f, "()"),
        }
    }
}

impl Display for Ty {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fn lower(ty: &Ty, metas: &mut HashMap<u32, u32>) -> Ty {
            match ty {
                // Ty::Meta(m) => match m.as_ref() {
                //     TyVar::Bound(ty) => lower(ty, metas),
                //     TyVar::Unbound(tv) => {
                //         if let Some(lowered) = metas.get(tv) {
                //             Ty::Meta(Box::new(TyVar::Unbound(*lowered)))
                //         } else {
                //             let id = metas.len() as u32;
                //             metas.insert(*tv, id);
                //             Ty::Meta(Box::new(TyVar::Unbound(id)))
                //         }
                //     }
                // },
                Ty::Arrow(param, body) => {
                    Ty::Arrow(Box::new(lower(param, metas)), Box::new(lower(body, metas)))
                }
                Ty::List(list_ty) => Ty::List(Box::new(lower(list_ty, metas))),
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
            Self::Meta(n) => write!(f, "{}", n),
            Self::PolyType(scheme) => write!(f, "{:?}", scheme),
            Self::Var(id) => write!(f, "Var({})", id),
            Self::Arrow(param, body) => {
                if matches!(*param, Self::Arrow(_, _)) {
                    write!(f, "({:?}) -> {:?}", param, body)
                } else {
                    write!(f, "{:?} -> {:?}", param, body)
                }
            }
            Self::List(ty) => write!(f, "[{}]", ty),
            Self::Array(ty) => write!(f, "[{}]", ty),
            Self::Record(name, fields) => write!(f, "{:?} = {:?}", name, fields),
            Self::Unit => write!(f, "()"),
        }
    }
}
