use super::span::Span;
use std::fmt::Display;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParserError {
    pub message: String,
}

impl Display for ParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:#}", self.message)
    }
}
