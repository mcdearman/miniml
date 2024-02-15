use super::{substitution::Substitution, ty_var::TyVar};
use std::{
    collections::{BTreeSet, HashMap},
    fmt::Debug,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Type {
    kind: Box<TypeKind>,
}

impl Type {
    pub fn new(kind: TypeKind) -> Self {
        Self {
            kind: Box::new(kind),
        }
    }

    pub fn kind(&self) -> &TypeKind {
        &self.kind
    }

    pub(super) fn apply_subst(&self, subst: Substitution) -> Type {
        match self.kind() {
            TypeKind::Int | TypeKind::Bool | TypeKind::Unit => *self,
            TypeKind::Var(n) => subst.get(&n).cloned().unwrap_or(*self),
            TypeKind::Lambda(params, body) => Type::new(TypeKind::Lambda(
                params.into_iter().map(|p| p.apply_subst(subst)).collect(),
                body.apply_subst(subst),
            )),
        }
    }

    pub(super) fn lower(&self, vars: &mut HashMap<TyVar, TyVar>) -> Self {
        match *self.kind {
            TypeKind::Int | TypeKind::Bool | TypeKind::Unit => *self,
            TypeKind::Var(name) => {
                if let Some(n) = vars.get(&name) {
                    Type::new(TypeKind::Var(*n))
                } else {
                    let n = vars.len();
                    vars.insert(name, TyVar::from(n));
                    Type::new(TypeKind::Var(TyVar::from(n)))
                }
            }
            TypeKind::Lambda(params, body) => {
                let mut lowered_params = vec![];
                for param in params {
                    lowered_params.push(param.lower(vars));
                }
                Type::new(TypeKind::Lambda(lowered_params, body.lower(vars)))
            }
        }
    }

    pub(super) fn free_vars(&self) -> BTreeSet<TyVar> {
        match *self.kind {
            TypeKind::Var(n) => vec![n.clone()].into_iter().collect(),
            TypeKind::Lambda(params, body) => params
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

#[derive(Clone, PartialEq, Eq)]
pub enum TypeKind {
    Int,
    Bool,
    // String,
    Var(TyVar),
    Lambda(Vec<Type>, Type),
    Unit,
}

impl Debug for TypeKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.clone() {
            TypeKind::Int => write!(f, "Int"),
            TypeKind::Bool => write!(f, "Bool"),
            // TypeKind::String => write!(f, "String"),
            TypeKind::Var(n) => write!(f, "{:?}", n),
            TypeKind::Lambda(params, body) => write!(f, "{:?} -> {:?}", params, body),
            TypeKind::Unit => write!(f, "()"),
        }
    }
}
