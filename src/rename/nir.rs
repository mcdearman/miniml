use num_rational::Rational64;

use crate::utils::{
    ident::{Ident, ScopedIdent},
    intern::InternedString,
    span::Span,
};

#[derive(Debug, Clone, PartialEq)]
pub struct Root {
    pub decls: Vec<Decl>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Decl {
    pub kind: DeclKind,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum DeclKind {
    Let(ScopedIdent, Expr),
    Fn(ScopedIdent, Vec<ScopedIdent>, Expr),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expr {
    pub kind: Box<ExprKind>,
    pub span: Span,
}

impl Expr {
    pub fn new(kind: ExprKind, span: Span) -> Self {
        Self {
            kind: Box::new(kind),
            span,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind {
    Lit(Lit),
    Var(ScopedIdent),
    Apply(Expr, Vec<Expr>),
    Lambda(Vec<ScopedIdent>, Expr),
    Or(Expr, Expr),
    And(Expr, Expr),
    Let(ScopedIdent, Expr, Expr),
    Fn(ScopedIdent, Vec<ScopedIdent>, Expr, Expr),
    Unit,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Lit {
    Byte(u8),
    Int(i64),
    Rational(Rational64),
    Real(f64),
    Bool(bool),
    String(InternedString),
    Char(char),
}
