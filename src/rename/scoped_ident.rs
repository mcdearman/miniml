use std::sync::atomic::{AtomicUsize, Ordering};

use crate::utils::{intern::InternedString, span::Span};

#[derive(Debug, Clone, Copy)]
pub struct ScopedIdent {
    pub id: usize,
    pub name: InternedString,
    pub span: Span,
}

impl ScopedIdent {
    pub fn new(id: usize, name: InternedString, span: Span) -> Self {
        Self { id, name, span }
    }
}

impl std::fmt::Display for ScopedIdent {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}{}", self.name, self.id)
    }
}

impl PartialEq for ScopedIdent {
    fn eq(&self, other: &ScopedIdent) -> bool {
        self.id == other.id
    }
}
