use super::pattern_kind::PatternKind;
use miniml_utils::span::Span;

#[derive(Debug, Clone, PartialEq)]
pub struct Pattern {
    kind: PatternKind,
    span: Span,
}

impl Pattern {
    pub fn new(kind: PatternKind, span: Span) -> Self {
        Self { kind, span }
    }

    pub fn kind(&self) -> &PatternKind {
        &self.kind
    }

    pub fn span(&self) -> &Span {
        &self.span
    }
}
