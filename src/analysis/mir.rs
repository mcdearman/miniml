/*
 * Mid-level Intermediate Representation (MIR) for the analysis phase. This IR
 * is used after type inference for all subsequent analysis and transformations.
 * It's an ANF (A-normal form) representation extended with join points, jumps,
 * and returns.
 */

use num_rational::Rational64;

use crate::utils::{ident::ScopedIdent, intern::InternedString, span::Span};

use super::infer::r#type::Type;

#[derive(Debug, Clone, PartialEq)]
pub struct Root {
    decls: Vec<Decl>,
    span: Span,
}

impl Root {
    pub fn new(decls: Vec<Decl>, span: Span) -> Self {
        Self { decls, span }
    }

    pub fn decls(&self) -> &[Decl] {
        &self.decls
    }

    pub fn span(&self) -> Span {
        self.span
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Decl {
    kind: DeclKind,
    ty: Type,
    span: Span,
}

impl Decl {
    pub fn new(kind: DeclKind, ty: Type, span: Span) -> Self {
        Self { kind, ty, span }
    }

    pub fn kind(&self) -> &DeclKind {
        &self.kind
    }

    pub fn ty(&self) -> &Type {
        &self.ty
    }

    pub fn span(&self) -> Span {
        self.span
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum DeclKind {
    // DataType(DataType),
    Let {
        name: ScopedIdent,
        expr: Expr,
    },
    Fn {
        name: ScopedIdent,
        params: Vec<ScopedIdent>,
        expr: Expr,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expr {
    pub kind: Box<ExprKind>,
    pub ty: Type,
    pub span: Span,
}

impl Expr {
    pub fn new(kind: ExprKind, ty: Type, span: Span) -> Self {
        Self {
            kind: Box::new(kind),
            ty,
            span,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind {
    Value(Value),
    Let(ScopedIdent, Expr, Expr),
    Fn(ScopedIdent, Vec<ScopedIdent>, Expr, Expr),
    Join(ScopedIdent, Expr, Expr),
    Jump(ScopedIdent, Option<Value>),
    Apply(Expr, Vec<Expr>),
    Match(Expr, Vec<(Pattern, Expr)>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Pattern {
    pub kind: PatternKind,
    pub ty: Type,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum PatternKind {
    Wildcard,
    Ident(ScopedIdent),
    Literal(Value),
    Record(Option<ScopedIdent>, Vec<(InternedString, Pattern)>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Int(i64),
    Rational(Rational64),
    Bool(bool),
    String(InternedString),
    Ident(ScopedIdent),
}
