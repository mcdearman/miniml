use super::{data_type_kind::DataTypeKind, ident::Ident};
use miniml_utils::span::Span;

#[derive(Debug, Clone, PartialEq)]
pub struct DataType {
    name: Ident,
    kind: Box<DataTypeKind>,
    span: Span,
}

impl DataType {
    pub fn new(name: Ident, kind: DataTypeKind, span: Span) -> Self {
        Self {
            name,
            kind: Box::new(kind),
            span,
        }
    }

    pub fn name(&self) -> &Ident {
        &self.name
    }

    pub fn kind(&self) -> &DataTypeKind {
        &self.kind
    }

    pub fn span(&self) -> &Span {
        &self.span
    }
}
