use super::span::Span;
use std::fmt::Display;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Error {
    pub span: Span,
    pub message: String,
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "ERROR: {:?}: {}", self.span, self.message)
    }
}
