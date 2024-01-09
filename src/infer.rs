use crate::utils::{InternedString, Span, UniqueId};
use num_rational::Rational64;
use std::fmt::Display;

#[derive(Debug, Clone, PartialEq)]
pub enum InferenceError {
    UnboundVariable(InternedString),
    TypeMismatch(Type, Type),
}

impl Display for InferenceError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            InferenceError::UnboundVariable(name) => write!(f, "Unbound variable: {}", name),
            InferenceError::TypeMismatch(expected, actual) => {
                write!(f, "Type mismatch: expected {}, actual {}", expected, actual)
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Num,
    Bool,
    Str,
    Lambda(Box<Self>, Box<Self>),
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Num => write!(f, "Num"),
            Type::Bool => write!(f, "Bool"),
            Type::Str => write!(f, "Str"),
            Type::Lambda(param, ret) => write!(f, "({} -> {})", param, ret),
        }
    }
}

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

    pub fn span(&self) -> &Span {
        &self.span
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Item {
    kind: ItemKind,
    span: Span,
}

impl Item {
    pub fn new(kind: ItemKind, span: Span) -> Self {
        Self { kind, span }
    }

    pub fn kind(&self) -> &ItemKind {
        &self.kind
    }

    pub fn span(&self) -> &Span {
        &self.span
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ItemKind {
    Expr(Expr),
    Decl(Decl),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expr {
    kind: Box<ExprKind>,
    type_: Type,
    span: Span,
}

impl Expr {
    pub fn new(kind: ExprKind, type_: Type, span: Span) -> Self {
        Self {
            kind: Box::new(kind),
            type_,
            span,
        }
    }

    pub fn kind(&self) -> &ExprKind {
        &self.kind
    }

    pub fn span(&self) -> &Span {
        &self.span
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind {
    Lit(Lit),
    Ident(Ident),
    Apply { fun: Expr, arg: Expr },
    If { cond: Expr, then: Expr, else_: Expr },
    Let { name: Ident, expr: Expr, body: Expr },
    Lambda { param: Ident, expr: Expr },
    Unit,
}

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

#[derive(Debug, Clone, PartialEq)]
pub enum Lit {
    Num(Rational64),
    Bool(bool),
    String(InternedString),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Decl {
    kind: DeclKind,
    span: Span,
}

impl Decl {
    pub fn new(kind: DeclKind, span: Span) -> Self {
        Self { kind, span }
    }

    pub fn kind(&self) -> &DeclKind {
        &self.kind
    }

    pub fn span(&self) -> &Span {
        &self.span
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum DeclKind {
    Let { name: Ident, expr: Expr },
}
