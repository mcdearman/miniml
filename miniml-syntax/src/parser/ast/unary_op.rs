use super::unary_op_kind::UnaryOpKind;
use miniml_utils::{intern::interned_string::InternedString, span::Span};

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct UnaryOp {
    kind: UnaryOpKind,
    span: Span,
}

impl UnaryOp {
    pub fn new(kind: UnaryOpKind, span: Span) -> Self {
        Self { kind, span }
    }

    pub fn kind(&self) -> &UnaryOpKind {
        &self.kind
    }

    pub fn span(&self) -> &Span {
        &self.span
    }
}

impl From<UnaryOp> for InternedString {
    fn from(op: UnaryOp) -> Self {
        InternedString::from(op.kind.to_string())
    }
}
