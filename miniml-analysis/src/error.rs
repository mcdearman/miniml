use miniml_utils::{intern::InternedString, span::Span};

#[derive(Debug, Clone, PartialEq)]
pub struct AnalysisError {
    kind: AnalysisErrorKind,
    span: Span,
}

impl AnalysisError {
    pub fn new(kind: AnalysisErrorKind, span: Span) -> Self {
        Self { kind, span }
    }

    pub fn kind(&self) -> &AnalysisErrorKind {
        &self.kind
    }

    pub fn span(&self) -> Span {
        self.span
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum AnalysisErrorKind {
    MirLowerError(InternedString),
}

pub type AnalysisResult<T> = Result<T, AnalysisError>;
