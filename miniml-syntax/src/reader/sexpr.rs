use miniml_common::{interner::InternedString, num::Num, span::Span};

#[derive(Debug, Clone, PartialEq)]
pub struct Root {
    sexprs: Vec<Sexpr>,
    span: Span,
}

impl Root {
    pub fn new(sexprs: Vec<Sexpr>, span: Span) -> Self {
        Self { sexprs, span }
    }

    pub fn sexprs(&self) -> &[Sexpr] {
        &self.sexprs
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Sexpr {
    kind: Box<SexprKind>,
    span: Span,
}

impl Sexpr {
    pub fn new(kind: SexprKind, span: Span) -> Self {
        Self {
            kind: Box::new(kind),
            span,
        }
    }

    pub fn kind(&self) -> &SexprKind {
        &self.kind
    }

    pub fn span(&self) -> &Span {
        &self.span
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum SexprKind {
    Atom(Atom),
    Pair { head: Sexpr, tail: Sexpr },
    List(Vec<Sexpr>),
    Vector(Vec<Sexpr>),
    ByteVector(Vec<u8>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Atom {
    kind: Box<AtomKind>,
    span: Span,
}

impl Atom {
    pub fn new(kind: AtomKind, span: Span) -> Self {
        Self {
            kind: Box::new(kind),
            span,
        }
    }

    pub fn kind(&self) -> &AtomKind {
        &self.kind
    }

    pub fn span(&self) -> &Span {
        &self.span
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum AtomKind {
    Sym(InternedString),
    Num(Num),
    Str(InternedString),
    Bool(bool),
    Char(char),
}
