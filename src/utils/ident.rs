use super::{intern::InternedString, span::Span};
use dbg_pls::DebugPls;
use std::fmt::{Debug, Display};

#[derive(Debug, DebugPls, Clone, Copy, PartialEq)]
pub struct Ident {
    pub name: InternedString,
    pub span: Span,
}

impl Ident {
    pub fn new(name: InternedString, span: Span) -> Self {
        Self { name, span }
    }

    pub fn as_str(&self) -> &str {
        &self.name
    }
}

impl Display for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}
