use super::token::Token;

#[derive(Debug, Clone, PartialEq)]
pub struct Tree {
    pub kind: SyntaxKind,
    pub children: Vec<Child>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum SyntaxKind {
    // Tokens
    File,
    Item,
    Decl,
    Mod,
    Data,
    Expr,
    Pipe,
    Stmt,
    Or,
    And,
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

// #[derive(Debug, Clone, PartialEq)]
// pub struct Token {
//     pub kind: TokenKind,
//     pub text: InternedString,
// }

#[derive(Debug, Clone, PartialEq)]
pub enum Child {
    Tree(Tree),
    Token(Token),
}
