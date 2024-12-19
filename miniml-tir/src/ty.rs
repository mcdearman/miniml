use std::{
    collections::{HashMap, HashSet},
    fmt::{Debug, Display},
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Ty;

// #[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
// pub enum Ty {
//     Byte,
//     Int,
//     Rational,
//     Real,
//     Bool,
//     String,
//     Char,
//     MetaRef(MetaRef),
//     Meta(Box<Meta>),
//     // Poly(PolyType),
//     Lambda(Box<Self>, Box<Self>),
//     List(Box<Self>),
//     Array(Box<Self>),
//     Record(UniqueId, Vec<(InternedString, Self)>),
//     Unit,
// }

// impl Ty {
//     pub(super) fn is_numeric(&self) -> bool {
//         match self {
//             Self::Byte | Self::Int | Self::Rational | Self::Real => true,
//             _ => false,
//         }
//     }

//     pub(super) fn generalize(&self, ctx: &Context, meta_ctx: &MetaContext) -> PolyType {
//         // log::debug!("generalize: {:?}", self);
//         // log::debug!("free vars: {:?}", self.free_vars(meta_ctx));
//         // log::debug!("ctx free vars: {:?}", ctx.free_vars(meta_ctx));
//         PolyType::new(
//             self.free_vars(meta_ctx)
//                 .difference(&ctx.free_vars(meta_ctx))
//                 .cloned()
//                 .collect(),
//             self.clone(),
//         )
//     }

//     pub(super) fn free_vars(&self, meta_ctx: &MetaContext) -> HashSet<MetaId> {
//         match self {
//             Self::MetaRef(n) => match meta_ctx.get(n) {
//                 Some(Meta::Bound(ty)) => ty.free_vars(meta_ctx),
//                 Some(Meta::Unbound(tv)) => vec![tv].into_iter().collect(),
//                 None => HashSet::new(),
//             },
//             Self::Lambda(param, body) => param
//                 .free_vars(meta_ctx)
//                 .union(&body.free_vars(meta_ctx))
//                 .cloned()
//                 .collect(),
//             _ => HashSet::new(),
//         }
//     }

//     pub(super) fn zonk(&self, meta_ctx: &mut MetaContext) -> Ty {
//         // log::debug!("zonk: {:?}", self);
//         match meta_ctx.force(self) {
//             Ty::Byte => Ty::Byte,
//             Ty::Int => Ty::Int,
//             Ty::Rational => Ty::Rational,
//             Ty::Real => Ty::Real,
//             Ty::Bool => Ty::Bool,
//             Ty::String => Ty::String,
//             Ty::Char => Ty::Char,
//             Ty::Unit => Ty::Unit,
//             Ty::MetaRef(n) => match meta_ctx.get(&n) {
//                 Some(Meta::Bound(ty)) => ty.zonk(meta_ctx),
//                 Some(Meta::Unbound(tv)) => Ty::Meta(Box::new(Meta::Unbound(tv))),
//                 None => Ty::MetaRef(n),
//             },
//             m @ Ty::Meta(_) => m,
//             // Type::Poly(poly) => Type::Poly(poly.clone()),
//             Ty::Lambda(param, body) => Ty::Lambda(
//                 Box::new(param.zonk(meta_ctx)),
//                 Box::new(body.zonk(meta_ctx)),
//             ),
//             Ty::List(ty) => Ty::List(Box::new(ty.zonk(meta_ctx))),
//             Ty::Array(ty) => Ty::Array(Box::new(ty.zonk(meta_ctx))),
//             Ty::Record(id, fields) => Ty::Record(
//                 id,
//                 fields
//                     .iter()
//                     .map(|(name, ty)| (name.clone(), ty.zonk(meta_ctx)))
//                     .collect(),
//             ),
//         }
//     }
// }

// impl Debug for Ty {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         match self {
//             Self::Byte => write!(f, "Byte"),
//             Self::Int => write!(f, "Int"),
//             Self::Rational => write!(f, "Rational"),
//             Self::Real => write!(f, "Real"),
//             Self::Bool => write!(f, "Bool"),
//             Self::String => write!(f, "String"),
//             Self::Char => write!(f, "Char"),
//             Self::MetaRef(n) => write!(f, "{}", n),
//             Self::Meta(m) => write!(f, "{:?}", m),
//             // Self::Poly(poly) => write!(f, "{:?}", poly),
//             Self::Lambda(param, body) => {
//                 if matches!(**param, Self::Lambda(_, _)) {
//                     write!(f, "({:?}) -> {:?}", param, body)
//                 } else {
//                     write!(f, "{:?} -> {:?}", param, body)
//                 }
//             }
//             Self::List(ty) => write!(f, "[{:?}]", ty),
//             Self::Array(ty) => write!(f, "#[{:?}]", ty),
//             Self::Record(id, fields) => write!(f, "{:?} = {:?}", id, fields),
//             Self::Unit => write!(f, "()"),
//         }
//     }
// }

// impl Display for Ty {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         fn lower(ty: &Ty, metas: &mut HashMap<u32, u32>) -> Ty {
//             match ty {
//                 Ty::Meta(m) => match m.as_ref() {
//                     Meta::Bound(ty) => lower(ty, metas),
//                     Meta::Unbound(tv) => {
//                         if let Some(lowered) = metas.get(tv) {
//                             Ty::Meta(Box::new(Meta::Unbound(*lowered)))
//                         } else {
//                             let id = metas.len() as u32;
//                             metas.insert(*tv, id);
//                             Ty::Meta(Box::new(Meta::Unbound(id)))
//                         }
//                     }
//                 },
//                 Ty::Lambda(param, body) => {
//                     Ty::Lambda(Box::new(lower(param, metas)), Box::new(lower(body, metas)))
//                 }
//                 Ty::List(list_ty) => Ty::List(Box::new(lower(list_ty, metas))),
//                 // Type::Record(name, fields) => {
//                 //     let mut lowered_fields = vec![];
//                 //     for (k, v) in fields {
//                 //         lowered_fields.push((k.clone(), lower(v, metas)));
//                 //     }
//                 //     Type::Record(name, lowered_fields)
//                 // }
//                 _ => ty.clone(),
//             }
//         }

//         let mut vars = HashMap::new();
//         // log::debug!("lowered: {:?}", lower(self, &mut vars));
//         match lower(self, &mut vars) {
//             Self::Byte => write!(f, "Byte"),
//             Self::Int => write!(f, "Int"),
//             Self::Rational => write!(f, "Rational"),
//             Self::Real => write!(f, "Real"),
//             Self::Bool => write!(f, "Bool"),
//             Self::String => write!(f, "String"),
//             Self::Char => write!(f, "Char"),
//             Self::MetaRef(n) => write!(f, "{}", n),
//             Self::Meta(m) => write!(f, "{}", m),
//             // Self::Poly(poly) => write!(f, "{}", poly),
//             Self::Lambda(param, body) => {
//                 if matches!(*param, Self::Lambda(_, _)) {
//                     write!(f, "({:?}) -> {:?}", param, body)
//                 } else {
//                     write!(f, "{:?} -> {:?}", param, body)
//                 }
//             }
//             Self::List(ty) => write!(f, "[{}]", ty),
//             Self::Array(ty) => write!(f, "[{}]", ty),
//             Self::Record(name, fields) => write!(f, "{:?} = {:?}", name, fields),
//             Self::Unit => write!(f, "()"),
//         }
//     }
// }
