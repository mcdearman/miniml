use crate::{class::Pred, ty::Ty};

#[derive(Debug, Clone, PartialEq)]
pub enum Constraint {
    Eq(Ty, Ty),
    Pred(Pred),
}
