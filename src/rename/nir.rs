use crate::utils::{
    ident::{Ident, ScopedIdent},
    intern::InternedString,
    span::Span,
    unique_id::UniqueId,
};

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
    span: Span,
}

impl Decl {
    pub fn new(kind: DeclKind, span: Span) -> Self {
        Self { kind, span }
    }

    pub fn kind(&self) -> DeclKind {
        self.kind
    }

    pub fn span(&self) -> Span {
        self.span
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum DeclKind {
    DataType(DataType),
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
pub struct DataType {
    name: ScopedIdent,
    kind: Box<DataTypeKind>,
    span: Span,
}

impl DataType {
    pub fn new(name: ScopedIdent, kind: DataTypeKind, span: Span) -> Self {
        Self {
            name,
            kind: Box::new(kind),
            span,
        }
    }

    pub fn name(&self) -> ScopedIdent {
        self.name
    }

    pub fn kind(&self) -> DataTypeKind {
        *self.kind
    }

    pub fn span(&self) -> Span {
        self.span
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum DataTypeKind {
    Record { fields: Vec<(Ident, TypeHint)> },
    // Sum {
    //     cases: Vec<(Ident, Option<SumTypeCaseHint>)>,
    // },
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

    pub fn kind(&self) -> ExprKind {
        *self.kind
    }

    pub fn span(&self) -> Span {
        self.span
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind {
    Lit(Lit),
    Ident(ScopedIdent),
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
        name: ScopedIdent,
        expr: Expr,
        body: Expr,
    },
    Fn {
        name: ScopedIdent,
        params: Vec<ScopedIdent>,
        expr: Expr,
        body: Expr,
    },
    Lambda {
        params: Vec<ScopedIdent>,
        expr: Expr,
    },
    List(Vec<Expr>),
    Record {
        name: Option<ScopedIdent>,
        fields: Vec<(Ident, Expr)>,
    },
    Unit,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeHint {
    kind: Box<TypeHintKind>,
    span: Span,
}

impl TypeHint {
    pub fn new(kind: TypeHintKind, span: Span) -> Self {
        Self {
            kind: Box::new(kind),
            span,
        }
    }

    pub fn kind(&self) -> &TypeHintKind {
        &self.kind
    }

    pub fn span(&self) -> &Span {
        &self.span
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeHintKind {
    Int,
    // Byte,
    // Real,
    // Rational,
    Bool,
    String,
    // Char,
    Ident(ScopedIdent),
    List(TypeHint),
    // Array(TypeHint),
    // Tuple(Vec<TypeHint>),
    Fn(Vec<TypeHint>, TypeHint),
    Unit,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Lit {
    Int(i64),
    Bool(bool),
    String(InternedString),
}
