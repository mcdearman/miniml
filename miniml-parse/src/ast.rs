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
pub enum DeclKind {
    DataType(DataType),
    Let { pattern: Pattern, expr: Expr },
}

#[derive(Debug, Clone, PartialEq)]
pub struct DataType {
    name: Ident,
    kind: Box<DataTypeKind>,
    span: Span,
}

impl DataType {
    pub fn new(name: Ident, kind: DataTypeKind, span: Span) -> Self {
        Self {
            name,
            kind: Box::new(kind),
            span,
        }
    }

    pub fn name(&self) -> &Ident {
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
    Record {
        fields: Vec<(Ident, TypeHint)>,
    },
    Sum {
        variants: Vec<(Ident, Option<TypeHint>)>,
    },
    Product {
        fields: Vec<TypeHint>,
    },
}
