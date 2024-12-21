use miniml_tir::ty::Ty;

#[derive(Debug, Clone, PartialEq)]
pub enum Constraint {
    Eq(Ty, Ty),
}
