use super::type_hint_kind::TypeHintKind;
use miniml_utils::span::Span;

#[derive(Debug, Clone, PartialEq)]
pub struct TypeHint {
    kind: Box<TypeHintKind>,
    span: Span,
}

impl TypeHint {
    pub fn new(kind: TypeHintKind, span: Span) -> Self {
        Self {
            kind: Box::new(kind),
            span,
        }
    }

    pub fn kind(&self) -> &TypeHintKind {
        &self.kind
    }

    pub fn span(&self) -> &Span {
        &self.span
    }
}
