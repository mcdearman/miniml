use super::{
    context::Context,
    error::{InferResult, TypeError},
    scheme::Scheme,
    substitution::Substitution,
    ty_var::TyVar,
};
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
    Record(UniqueId, Vec<(InternedString, Self)>),
    Unit,
}

impl Type {
    pub(super) fn apply_subst(&self, subst: &Substitution) -> Type {
        match self {
            Self::Int | Self::Bool | Self::String | Self::Unit => self.clone(),
            Self::Var(n) => subst.get(&n).cloned().unwrap_or(self.clone()),
            Self::Lambda(params, body) => Self::Lambda(
                params.into_iter().map(|p| p.apply_subst(subst)).collect(),
                Box::new(body.apply_subst(subst)),
            ),
            Self::List(ty) => Self::List(Box::new(ty.apply_subst(subst))),
            Self::Record(name, fields) => Self::Record(
                *name,
                fields
                    .iter()
                    .map(|(k, v)| (k.clone(), v.apply_subst(subst)))
                    .collect(),
            ),
        }
    }

    pub fn generalize(self, ctx: &Context) -> Scheme {
        Scheme::new(
            self.free_vars()
                .difference(&ctx.free_vars())
                .cloned()
                .collect(),
            self,
        )
    }

    pub fn unify(&self, other: &Self) -> InferResult<Substitution> {
        // println!("unify: {:?} and {:?}", t1, t2);
        match (self, other) {
            (Type::Int, Type::Int) | (Type::Bool, Type::Bool) | (Type::Unit, Type::Unit) => {
                Ok(Substitution::new())
            }
            (Type::Lambda(p1, b1), Type::Lambda(p2, b2)) => {
                let s1 = p1.iter().zip(p2.iter()).fold(
                    Ok(Substitution::new()),
                    |acc: InferResult<Substitution>, (t1, t2)| {
                        let s = acc?;
                        let t1 = t1.apply_subst(&s);
                        let t2 = t2.apply_subst(&s);
                        let s1 = self.unify(&t2)?;
                        Ok(s1.compose(&s))
                    },
                )?;
                let s2 = b1.apply_subst(&s1).unify(&b2.apply_subst(&s1))?;
                Ok(s1.compose(&s2))
            }
            (_, Type::Var(var)) => var.bind(self.clone()),
            (Type::Var(var), _) => var.bind(other.clone()),
            _ => Err(TypeError::from(format!(
                "cannot unify {:?} and {:?}",
                self.lower(&mut HashMap::new()),
                other.lower(&mut HashMap::new())
            ))),
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
                let mut lowered_fields = vec![];
                for (k, v) in fields {
                    lowered_fields.push((k.clone(), v.lower(vars)));
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
