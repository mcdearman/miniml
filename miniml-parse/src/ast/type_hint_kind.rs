use super::{ident::Ident, type_hint::TypeHint};

#[derive(Debug, Clone, PartialEq)]
pub enum TypeHintKind {
    Num,
    Bool,
    String,
    Ident(Ident),
    List(TypeHint),
    Vec(Vec<TypeHint>),
    Unit,
}
