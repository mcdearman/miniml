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
