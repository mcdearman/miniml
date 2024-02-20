use crate::utils::{intern::InternedString, span::Span, unique_id::UniqueId};

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
    Let {
        name: Ident,
        expr: Expr,
    },
    Fn {
        name: Ident,
        params: Vec<Ident>,
        expr: Expr,
    },
}

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

    pub fn span(&self) -> &Span {
        &self.span
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind {
    Lit(Lit),
    Ident(Ident),
    Apply {
        fun: Expr,
        args: Vec<Expr>,
    },
    If {
        cond: Expr,
        then: Expr,
        else_: Expr,
    },
    Let {
        name: Ident,
        expr: Expr,
        body: Expr,
    },
    Fn {
        name: Ident,
        params: Vec<Ident>,
        expr: Expr,
        body: Expr,
    },
    Lambda {
        params: Vec<Ident>,
        expr: Expr,
    },
    Unit,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Ident {
    id: UniqueId,
    span: Span,
}

impl Ident {
    pub fn new(id: UniqueId, span: Span) -> Self {
        Self { id, span }
    }

    pub fn id(&self) -> &UniqueId {
        &self.id
    }

    pub fn span(&self) -> &Span {
        &self.span
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Lit {
    Int(i64),
    Bool(bool),
    String(InternedString),
}
