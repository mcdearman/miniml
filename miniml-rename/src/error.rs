use miniml_utils::{interned_string::InternedString, span::Span};
use std::fmt::Display;

#[derive(Debug, Clone, PartialEq)]
pub struct ResError {
    kind: ResErrorKind,
    span: Span,
}

impl ResError {
    pub fn new(kind: ResErrorKind, span: Span) -> Self {
        Self { kind, span }
    }

    pub fn kind(&self) -> &ResErrorKind {
        &self.kind
    }

    pub fn span(&self) -> &Span {
        &self.span
    }
}

impl Display for ResError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?} @ {}", self.span, self.kind)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ResErrorKind {
    UnboundName(InternedString),
}

impl Display for ResErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ResErrorKind::UnboundName(name) => {
                write!(f, "unbound name '{}'", name)
            }
        }
    }
}

pub type ResResult<T> = Result<T, ResError>;