use crate::intern::InternedString;

#[derive(Debug, Clone, PartialEq)]
pub struct Node {
    pub kind: SyntaxKind,
    pub children: Vec<NodeOrToken>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum SyntaxKind {
    Error,
    Unit,
    Int,
    Bool,
    Ident,
    Lambda,
    Lit,
    If,
    Let,
    Atom,
    Apply,
    Unary,
    Power,
    Factor,
    Term,
    Cmp,
    Eq,
    And,
    Or,
    Expr,
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
