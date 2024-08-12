use crate::utils::{intern::InternedString, span::Span};

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ScopedIdent {
    pub name: InternedString,
    pub level: usize,
    pub span: Span,
}

impl ScopedIdent {
    pub fn new(name: InternedString, level: usize, span: Span) -> Self {
        Self { name, level, span }
    }
}

impl std::fmt::Display for ScopedIdent {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}{}", self.name, self.level)
    }
}
