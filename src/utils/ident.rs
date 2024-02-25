use super::{intern::InternedString, span::Span, unique_id::UniqueId};
use std::fmt::Debug;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Ident {
    key: InternedString,
    span: Span,
}

impl Ident {
    pub fn new(key: InternedString, span: Span) -> Self {
        Self { key, span }
    }

    pub fn key(&self) -> InternedString {
        self.key
    }

    pub fn span(&self) -> Span {
        self.span
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ScopedIdent {
    id: UniqueId,
    span: Span,
}

impl ScopedIdent {
    pub fn new(id: UniqueId, span: Span) -> Self {
        Self { id, span }
    }

    pub fn id(&self) -> UniqueId {
        self.id
    }

    pub fn span(&self) -> Span {
        self.span
    }
}
