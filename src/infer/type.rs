use super::{substitution::Substitution, ty_var::TyVar};
use crate::utils::{intern::InternedString, unique_id::UniqueId};
use std::{
    collections::{BTreeSet, HashMap},
    fmt::Debug,
};

#[derive(Clone, PartialEq, Eq)]
pub enum Type {
    Int,
    Bool,
    String,
    Var(TyVar),
    Lambda(Vec<Self>, Box<Self>),
    List(Box<Self>),
    Record(UniqueId, HashMap<UniqueId, Self>),
    Unit,
}

impl Type {
    pub(super) fn apply_subst(&self, subst: Substitution) -> Type {
        match self {
            Self::Int | Self::Bool | Self::String | Self::Unit => self.clone(),
            Self::Var(n) => subst.get(&n).cloned().unwrap_or(self.clone()),
            Self::Lambda(params, body) => Self::Lambda(
                params
                    .into_iter()
                    .map(|p| p.apply_subst(subst.clone()))
                    .collect(),
                Box::new(body.apply_subst(subst)),
            ),
            Self::List(ty) => Self::List(Box::new(ty.apply_subst(subst))),
            Self::Record(name, fields) => Self::Record(
                *name,
                fields
                    .iter()
                    .map(|(k, v)| (k.clone(), v.apply_subst(subst.clone())))
                    .collect(),
            ),
        }
    }

    pub(super) fn lower(&self, vars: &mut HashMap<TyVar, TyVar>) -> Self {
        match self {
            Self::Int | Self::Bool | Self::String | Self::Unit => self.clone(),
            Self::Var(name) => {
                if let Some(n) = vars.get(&name) {
                    Self::Var(*n)
                } else {
                    let n = vars.len();
                    vars.insert(*name, TyVar::from(n));
                    Self::Var(TyVar::from(n))
                }
            }
            Self::Lambda(params, body) => {
                let mut lowered_params = vec![];
                for param in params {
                    lowered_params.push(param.lower(vars));
                }
                Self::Lambda(lowered_params, Box::new(body.lower(vars)))
            }
            Self::List(ty) => Self::List(Box::new(ty.lower(vars))),
            Self::Record(name, fields) => {
                let mut lowered_fields = HashMap::new();
                for (k, v) in fields {
                    lowered_fields.insert(k.clone(), v.lower(vars));
                }
                Self::Record(*name, lowered_fields)
            }
        }
    }

    pub(super) fn free_vars(&self) -> BTreeSet<TyVar> {
        match self {
            Self::Var(n) => vec![n.clone()].into_iter().collect(),
            Self::Lambda(params, body) => params
                .into_iter()
                .fold(BTreeSet::new(), |acc, p| {
                    p.free_vars().union(&acc).cloned().collect()
                })
                .union(&body.free_vars())
                .cloned()
                .collect(),
            _ => BTreeSet::new(),
        }
    }
}

impl Debug for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.clone() {
            Self::Int => write!(f, "Int"),
            Self::Bool => write!(f, "Bool"),
            Self::String => write!(f, "String"),
            Self::Var(n) => write!(f, "{:?}", n),
            Self::Lambda(params, body) => write!(f, "{:?} -> {:?}", params, body),
            Self::List(ty) => write!(f, "[{:?}]", ty),
            Self::Record(name, fields) => write!(f, "{:?} = {:?}", name, fields),
            Self::Unit => write!(f, "()"),
        }
    }
}
