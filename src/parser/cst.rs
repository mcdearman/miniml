use crate::intern::InternedString;

#[derive(Debug, Clone, PartialEq)]
pub struct Node {
    pub kind: SyntaxKind,
    pub children: Vec<NodeOrToken>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum SyntaxKind {
    Item,
    Data,
    Decl,
    Expr,
    Logical,
    Cmp,
    Term,
    Factor,
    Power,
    Unary,
    Apply,
    Atom,
    LetExpr,
    FnExpr,
    Let,
    Fn,
    If,
    Match,
    FullPattern,
    FullListPattern,
    FullTuplePattern,
    FullRecordPattern,
    LetPattern,
    LetListPattern,
    LetTuplePattern,
    LetRecordPattern,
    Lit,
    Lambda,
    List,
    Tuple,
    Map,
    String,
    Char,
    Ident,
    Bool,
    Real,
    Int,
    Wildcard,
    Unit,
    Error,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: SyntaxKind,
    pub text: InternedString,
}

#[derive(Debug, Clone, PartialEq)]
pub enum NodeOrToken {
    Node(Node),
    Token(Token),
}
