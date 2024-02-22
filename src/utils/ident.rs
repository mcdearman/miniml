use super::{intern::InternedString, span::Span, unique_id::UniqueId};
use std::fmt::Debug;

// pub trait Ident: Debug + Clone + PartialEq {
//     type Context<'ctx>;

//     fn identify(&self, ctx: Self::Context<'ctx>) -> &str;
//     fn span(&self) -> Span;
// }

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Ident {
    name: InternedString,
    span: Span,
}

impl Ident {
    pub fn new(name: InternedString, span: Span) -> Self {
        Self { name, span }
    }

    pub fn name(&self) -> InternedString {
        self.name
    }

    pub fn span(&self) -> Span {
        self.span
    }
}

// impl Ident for Name {
//     type Context = ();

//     fn identify(&self, ctx: Self::Context) -> &str {
//         &self.name
//     }

//     fn span(&self) -> Span {
//         self.span
//     }
// }

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

// impl Ident for ScopedName {
//     type Context<'src> = UniqueNameContext<'src>;

//     fn identify(&self, ctx: Self::Context) -> &str {
//         &ctx.src[self.span()]
//     }

//     fn span(&self) -> Span {
//         self.span
//     }
// }

// #[derive(Debug, Clone, PartialEq)]
// pub struct UniqueNameContext<'src> {
//     src: &'src str,
// }

// impl<'src> UniqueNameContext<'src> {
//     pub fn new(src: &'src str) -> Self {
//         Self { src }
//     }
// }
