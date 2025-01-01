use miniml_tir::{ty::Ty, ty_var::TyVar};
use miniml_utils::intern::InternedString;
use std::fmt::Display;

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum TypeError {
    UnificationMismatch(Ty, Ty),
    UnboundVariable(TyVar),
    OccursCheck,
}

impl Display for TypeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeError::UnificationMismatch(t1, t2) => write!(f, "cannot unify {} with {}", t1, t2),
            TypeError::UnboundVariable => write!(f, "unbound variable"),
            TypeError::OccursCheck => write!(f, "occurs check failed"),
        }
    }
}

pub type InferResult<T> = Result<T, TypeError>;
