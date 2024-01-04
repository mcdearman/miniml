#[derive(Debug, Clone, PartialEq)]
pub struct Sexpr {
    pub kind: SexprKind,
    pub span: Span,
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
    pub kind: AtomKind,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum AtomKind {
    Sym(InternedString),
    Num(Num),
    Str(InternedString),
    Bool(bool),
    Char(char),
}
