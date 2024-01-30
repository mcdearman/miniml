use super::{ident::Ident, type_hint::TypeHint};

#[derive(Debug, Clone, PartialEq)]
pub enum DataTypeKind {
    Record {
        fields: Vec<(Ident, TypeHint)>,
    },
    Sum {
        variants: Vec<(Ident, Option<TypeHint>)>,
    },
    Product {
        fields: Vec<TypeHint>,
    },
}
