use super::span::Span;
use std::fmt::Display;

#[derive(Debug, Clone, Copy, Eq, PartialEq, Default, Hash)]
pub struct Spanned<T> {
    pub value: T,
    pub span: Span,
}

impl Display for Spanned<&str> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}
