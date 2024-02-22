use super::{intern::InternedString, span::Span, unique_id::UniqueId};
use std::fmt::Debug;

// pub trait Ident: Debug + Clone + PartialEq {
//     fn identify(&self, ctx: &impl Context) -> &str {
//         ctx.resolve(self)
//     }
//     fn span(&self) -> Span;
// }

// // A `Context` is a type that can be used to resolve an `Ident` to a `&str`.
// // It can contain extra information that is needed to resolve the `Ident`.
// pub trait Context {
//     fn resolve(&self, ident: &impl Ident) -> &str;
// }

// #[derive(Debug, Clone, Copy, PartialEq)]
// pub struct Name {
//     name: InternedString,
//     span: Span,
// }

// impl Name {
//     pub fn new(name: InternedString, span: Span) -> Self {
//         Self { name, span }
//     }

//     pub fn name(&self) -> InternedString {
//         self.name
//     }

//     pub fn span(&self) -> Span {
//         self.span
//     }
// }

// impl Ident for Name {
//     fn identify(&self, ctx: &impl Context) -> &str {
//         &self.name
//     }

//     fn span(&self) -> Span {
//         self.span
//     }
// }

// #[derive(Debug, Clone, Copy, PartialEq)]
// pub struct UniqueName {
//     id: UniqueId,
//     span: Span,
// }

// impl UniqueName {
//     pub fn new(id: UniqueId, span: Span) -> Self {
//         Self { id, span }
//     }

//     pub fn id(&self) -> UniqueId {
//         self.id
//     }

//     pub fn span(&self) -> Span {
//         self.span
//     }
// }

// impl Ident for UniqueName {
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

// impl<'src> Context for UniqueNameContext<'src> {
//     fn resolve(&self, ident: &impl Ident) -> &str {
//         &self.src[ident.span()]
//     }
// }
