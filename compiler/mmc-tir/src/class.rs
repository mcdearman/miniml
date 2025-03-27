use crate::ty::Ty;
use miniml_utils::intern::InternedString;

#[derive(Debug, Clone, PartialEq)]
pub struct Qualified<T> {
    preds: Vec<Pred>,
    value: T,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Pred {
    name: InternedString,
    ty: Ty,
}
