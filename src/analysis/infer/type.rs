use itertools::Itertools;

use super::{
    constraint::Constraint,
    context::Context,
    error::{InferResult, TypeError},
    scheme::Scheme,
    ty_var::TyVar,
};
use crate::utils::{intern::InternedString, unique_id::UniqueId};
use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    fmt::{Debug, Display},
    rc::Rc,
};

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Type {
    Byte,
    Int,
    Rational,
    Real,
    Bool,
    String,
    Char,
    Var(Rc<RefCell<TyVar>>),
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

    pub fn generalize(&self, ctx: &Context) -> Scheme {
        log::debug!("generalize: {:?}", self);
        log::debug!("free vars: {:?}", self.free_vars());
        log::debug!("ctx free vars: {:?}", ctx.free_vars());
        Scheme::new(
            self.free_vars()
                .difference(&ctx.free_vars())
                .cloned()
                .collect(),
            self.clone(),
        )
    }

    pub fn unify(&mut self, other: &mut Self) -> InferResult<()> {
        log::debug!("unify: {:?} and {:?}", self, other);
        match (self, other) {
            (Type::Byte, Type::Byte)
            | (Type::Int, Type::Int)
            | (Type::Rational, Type::Rational)
            | (Type::Real, Type::Real)
            | (Type::Bool, Type::Bool)
            | (Type::String, Type::String)
            | (Type::Char, Type::Char)
            | (Type::Unit, Type::Unit) => Ok(()),
            (Type::Lambda(p1, b1), Type::Lambda(p2, b2)) => {
                // let s1 = p1.iter().zip(p2.iter()).try_fold(
                //     Substitution::new(),
                //     |s, (t1, t2)| match t1.apply_subst(&s).unify(&t2.apply_subst(&s)) {
                //         Ok(sub) => Ok(s.compose(&sub)),
                //         err => err,
                //     },
                // )?;
                // let s2 = b1.apply_subst(&s1).unify(&b2.apply_subst(&s1))?;
                // Ok(s1.compose(&s2))
                todo!()
            }
            (Type::List(t1), Type::List(t2)) => t1.unify(t2),
            (_, Type::Var(var)) => var.bind(self.clone()),
            (Type::Var(var), _) => var.bind(other.clone()),
            _ => Err(TypeError::from(format!(
                "cannot unify {:?} and {:?}",
                self, other,
            ))),
        }
    }

    pub(super) fn free_vars(&self) -> HashSet<TyVar> {
        match self {
            Self::Var(n) => vec![n.clone()].into_iter().collect(),
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
}

impl Debug for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.clone() {
            Self::Byte => write!(f, "Byte"),
            Self::Int => write!(f, "Int"),
            Self::Rational => write!(f, "Rational"),
            Self::Real => write!(f, "Real"),
            Self::Bool => write!(f, "Bool"),
            Self::String => write!(f, "String"),
            Self::Char => write!(f, "Char"),
            Self::Var(n) => write!(f, "{:?}", n),
            Self::Lambda(params, body) => write!(f, "{:?} -> {:?}", params, body),
            Self::List(ty) => write!(f, "[{:?}]", ty),
            Self::Record(name, fields) => write!(f, "{:?} = {:?}", name, fields),
            Self::Unit => write!(f, "()"),
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fn lower(ty: Type, vars: &mut HashMap<TyVar, TyVar>) -> Type {
            match ty {
                Type::Byte
                | Type::Int
                | Type::Rational
                | Type::Real
                | Type::Bool
                | Type::String
                | Type::Char
                | Type::Unit => ty,
                Type::Var(var) => {
                    if let Some(v) = vars.get(&var) {
                        Type::Var(v.clone())
                    } else {
                        let ty = TyVar::Unbound(UniqueId::new(vars.len()));
                        vars.insert(var, ty.clone());
                        Type::Var(ty)
                    }
                }
                Type::Lambda(params, body) => Type::Lambda(
                    params.iter().map(|p| lower(p.clone(), vars)).collect_vec(),
                    Box::new(lower(*body, vars)),
                ),
                Type::List(list_ty) => Type::List(Box::new(lower(*list_ty, vars))),
                Type::Record(name, fields) => {
                    let mut lowered_fields = vec![];
                    for (k, v) in fields {
                        lowered_fields.push((k.clone(), lower(v, vars)));
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
            Self::Var(n) => write!(f, "{}", n),
            Self::Lambda(params, body) => {
                if params.len() == 1 {
                    // println!("1");
                    write!(f, "{:?} -> {:?}", params[0], body)
                } else {
                    // println!("2");
                    write!(f, "({}) -> {:?}", params.iter().format(", "), body)
                }
            }
            Self::List(ty) => write!(f, "[{:?}]", ty),
            Self::Record(name, fields) => write!(f, "{:?} = {:?}", name, fields),
            Self::Unit => write!(f, "()"),
        }
    }
}
