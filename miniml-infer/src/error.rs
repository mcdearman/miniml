use miniml_tir::{ty::Ty, ty_var::TyVar};
use miniml_utils::intern::InternedString;
use std::fmt::Display;

#[derive(Debug, Clone, PartialEq)]
pub enum TypeError {
    UnificationMismatch(Ty, Ty),
    UnboundName(InternedString),
    UnboundVariable(TyVar),
    OccursCheck(Ty, Ty),
}

impl Display for TypeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeError::UnificationMismatch(t1, t2) => write!(f, "cannot unify {} with {}", t1, t2),
            TypeError::UnboundName(n) => write!(f, "unbound name {:?}", n),
            TypeError::UnboundVariable(v) => write!(f, "unbound variable {:?}", v),
            TypeError::OccursCheck(t1, t2) => {
                write!(f, "item has infinite type. {:?} occurs in {:?}", t1, t2)
            }
        }
    }
}

pub type InferResult<T> = Result<T, TypeError>;
