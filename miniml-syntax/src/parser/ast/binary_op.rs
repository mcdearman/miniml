use super::binary_op_kind::BinaryOpKind;
use miniml_utils::{intern::interned_string::InternedString, span::Span};

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct BinaryOp {
    kind: BinaryOpKind,
    span: Span,
}

impl BinaryOp {
    pub fn new(kind: BinaryOpKind, span: Span) -> Self {
        Self { kind, span }
    }

    pub fn kind(&self) -> &BinaryOpKind {
        &self.kind
    }

    pub fn span(&self) -> &Span {
        &self.span
    }
}

impl From<BinaryOp> for InternedString {
    fn from(op: BinaryOp) -> Self {
        InternedString::from(op.kind.to_string())
    }
}
