use miniml_utils::{intern::InternedString, span::Span};
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
    UnboundBuiltIn(InternedString),
    DuplicateName(InternedString),
    InvalidDefPattern,
    InvalidLetPattern,
    EmptyFnMatch,
    TooManyFnNames,
    FnArmParamMismatch,
}

impl Display for ResErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ResErrorKind::UnboundName(name) => {
                write!(f, "unbound name '{}'", name)
            }
            ResErrorKind::UnboundBuiltIn(name) => {
                write!(f, "unbound built-in '{}'", name)
            }
            ResErrorKind::DuplicateName(name) => {
                write!(f, "duplicate name '{}'", name)
            }
            ResErrorKind::InvalidDefPattern => {
                write!(f, "invalid definition pattern")
            }
            ResErrorKind::InvalidLetPattern => {
                write!(f, "invalid let pattern")
            }
            ResErrorKind::EmptyFnMatch => {
                write!(f, "empty function match")
            }
            ResErrorKind::TooManyFnNames => {
                write!(f, "too many function names in match")
            }
            ResErrorKind::FnArmParamMismatch => {
                write!(f, "function arms have mismatched parameters")
            }
        }
    }
}

pub type ResResult<T> = Result<T, ResError>;
