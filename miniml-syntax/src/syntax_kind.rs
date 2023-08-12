use cstree::Syntax;

#[repr(u32)]
#[derive(Syntax, Debug, Clone, Copy, PartialEq, Eq)]
pub enum SyntaxKind {
    // Tokens
    Error,
    Whitespace,
    Rational,
    Int,
    #[static_text("+")]
    Plus,
    #[static_text("-")]
    Minus,
    #[static_text("*")]
    Star,
    #[static_text("/")]
    Slash,
    LParen,
    RParen,
    // Nodes
    Lit,
    Atom,
    Factor,
    Term,
    Expr,
    Root,
}

pub type Miniml = SyntaxKind;
