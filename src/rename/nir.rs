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
    DataType(DataType),
    Let {
        name: UniqueIdent,
        expr: Expr,
    },
    Fn {
        name: UniqueIdent,
        params: Vec<UniqueIdent>,
        expr: Expr,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct DataType {
    name: UniqueIdent,
    kind: Box<DataTypeKind>,
    span: Span,
}

impl DataType {
    pub fn new(name: UniqueIdent, kind: DataTypeKind, span: Span) -> Self {
        Self {
            name,
            kind: Box::new(kind),
            span,
        }
    }

    pub fn name(&self) -> &UniqueIdent {
        &self.name
    }

    pub fn kind(&self) -> &DataTypeKind {
        &self.kind
    }

    pub fn span(&self) -> &Span {
        &self.span
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
    Ident(UniqueIdent),
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
        name: UniqueIdent,
        expr: Expr,
        body: Expr,
    },
    Fn {
        name: UniqueIdent,
        params: Vec<UniqueIdent>,
        expr: Expr,
        body: Expr,
    },
    Lambda {
        params: Vec<UniqueIdent>,
        expr: Expr,
    },
    List(Vec<Expr>),
    Record {
        name: Option<UniqueIdent>,
        fields: Vec<(UniqueIdent, Expr)>,
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
    Ident(UniqueIdent),
    List(TypeHint),
    // Array(TypeHint),
    // Tuple(Vec<TypeHint>),
    Fn(Vec<TypeHint>, TypeHint),
    Unit,
}

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

#[derive(Debug, Clone, PartialEq)]
pub struct UniqueIdent {
    id: UniqueId,
    span: Span,
}

impl UniqueIdent {
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
