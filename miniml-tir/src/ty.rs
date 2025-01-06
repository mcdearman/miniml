use crate::{
    scheme::Scheme,
    ty_var::TyVar,
    var_context::{VarContext, VarId},
};
use miniml_utils::{intern::InternedString, unique_id::UniqueId};
use std::{
    collections::{HashMap, HashSet},
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
    Var(VarId),
    Arrow(Box<Self>, Box<Self>),
    Gen(u16),
    List(Box<Self>),
    Array(Box<Self>),
    Record(UniqueId, Vec<(InternedString, Self)>),
    Unit,
}

impl Ty {
    pub fn generalize(&self, ctx_free_vars: HashSet<u32>, meta_ctx: &VarContext) -> Scheme {
        // log::debug!("generalize: {:?}", self);
        // log::debug!("free vars: {:?}", self.free_vars(meta_ctx));
        // log::debug!("ctx free vars: {:?}", ctx.free_vars(meta_ctx));
        // Scheme::new(
        //     self.free_vars(meta_ctx)
        //         .difference(&ctx_free_vars)
        //         .cloned()
        //         .for_each(|v| );
        //         .collect(),
        //     self.clone(),
        // )
        todo!()
    }

    pub fn free_vars(&self, var_ctx: &VarContext) -> HashSet<u32> {
        match self {
            Self::Var(n) => match var_ctx.get(n) {
                TyVar::Bound(ty) => ty.free_vars(var_ctx),
                TyVar::Unbound(tv) => vec![tv].into_iter().collect(),
            },
            Self::Arrow(param, body) => param
                .free_vars(var_ctx)
                .union(&body.free_vars(var_ctx))
                .cloned()
                .collect(),
            _ => HashSet::new(),
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
            Ty::Var(v) => match v.get() {
                TyVar::Bound(ty) => ty.zonk(),
                TyVar::Unbound(tv) => panic!("unbound type variable: {:?}", tv),
            },
            Ty::Arrow(param, body) => Ty::Arrow(Box::new(param.zonk()), Box::new(body.zonk())),
            Ty::Gen(id) => Ty::Gen(id),
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
            Ty::Var(id) => match id.get() {
                TyVar::Bound(ty) => {
                    let ty = ty.force();
                    id.insert(TyVar::Bound(ty.clone()));
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
            Self::Var(n) => write!(f, "{}", n),
            Self::Arrow(param, body) => {
                if matches!(**param, Self::Arrow(_, _)) {
                    write!(f, "({:?}) -> {:?}", param, body)
                } else {
                    write!(f, "{:?} -> {:?}", param, body)
                }
            }
            Self::Gen(id) => write!(f, "Gen({})", id),
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
            Self::Var(n) => write!(f, "{}", n),
            Self::Arrow(param, body) => {
                if matches!(*param, Self::Arrow(_, _)) {
                    write!(f, "({:?}) -> {:?}", param, body)
                } else {
                    write!(f, "{:?} -> {:?}", param, body)
                }
            }
            Self::Gen(id) => write!(f, "Gen({})", id),
            Self::List(ty) => write!(f, "[{}]", ty),
            Self::Array(ty) => write!(f, "[{}]", ty),
            Self::Record(name, fields) => write!(f, "{:?} = {:?}", name, fields),
            Self::Unit => write!(f, "()"),
        }
    }
}
