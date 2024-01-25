use miniml_utils::{intern::interned_string::InternedString, span::Span};

#[derive(Debug, Clone, PartialEq)]
pub struct Ident {
    name: InternedString,
    span: Span,
}

impl Ident {
    pub fn new(name: InternedString, span: Span) -> Self {
        Self { name, span }
    }

    pub fn name(&self) -> &InternedString {
        &self.name
    }

    pub fn span(&self) -> &Span {
        &self.span
    }
}

impl ToString for Ident {
    fn to_string(&self) -> String {
        self.name.clone().to_string()
    }
}
