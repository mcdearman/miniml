use super::{intern::InternedString, span::Span, unique_id::UniqueId};
use std::fmt::{Debug, Display};

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Ident {
    pub key: InternedString,
    pub span: Span,
}

impl Ident {
    pub fn new(key: InternedString, span: Span) -> Self {
        Self { key, span }
    }

    pub fn as_str(&self) -> &str {
        &self.key
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct ScopedIdent {
    pub id: UniqueId,
    pub key: InternedString,
    pub span: Span,
}

impl ScopedIdent {
    pub fn new(id: UniqueId, key: InternedString, span: Span) -> Self {
        Self { id, key, span }
    }

    pub fn as_str(&self) -> &str {
        &self.key
    }
}

impl Debug for ScopedIdent {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} @ {}", self.key, self.span)
    }
}

impl Display for ScopedIdent {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.key)
    }
}
