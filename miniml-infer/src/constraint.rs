use miniml_tir::ty::Ty;
use miniml_utils::intern::InternedString;

#[derive(Debug, Clone, PartialEq)]
pub enum Constraint {
    Eq(Ty, Ty),
}
