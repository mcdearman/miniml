use miniml_utils::{span::Span, unique_id::UniqueId};

#[derive(Debug, Clone, PartialEq)]
pub struct Ident {
    name: UniqueId,
    span: Span,
}

impl Ident {
    pub fn new(name: UniqueId, span: Span) -> Self {
        Self { name, span }
    }

    pub fn name(&self) -> &UniqueId {
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
