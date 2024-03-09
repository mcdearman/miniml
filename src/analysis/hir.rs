/*
 * High-level Intermediate Representation (HIR) for the analysis phase. This IR
 * is used after type inference for all subsequent analysis and transformations.
 * It's an ANF (A-normal form) representation extended with join points and jumps.
 */

use crate::utils::span::Span;

// #[derive(Debug, Clone, PartialEq)]
// pub struct Root {
//     decls: Vec<Decl>,
//     span: Span,
// }

// impl Root {
//     pub fn new(decls: Vec<Decl>) -> Self {
//         Self { decls }
//     }

//     pub fn decls(&self) -> &[Decl] {
//         &self.decls
//     }
// }

#[derive(Debug, Clone, PartialEq)]
pub struct Expr {
    kind: Box<ExprKind>,
    span: Span,
}

impl Expr {
    pub fn new(kind: ExprKind, span: Span) -> Self {
        Self {
            kind: Box::new(kind),
            span,
        }
    }

    pub fn kind(&self) -> &ExprKind {
        &self.kind
    }

    pub fn span(&self) -> Span {
        self.span
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind {
    Halt(Expr),
    
}
