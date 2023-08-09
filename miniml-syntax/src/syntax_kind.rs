use cstree::Syntax;

#[repr(u32)]
#[derive(Syntax, Debug, Clone, Copy, PartialEq, Eq)]
pub enum SyntaxKind {
    // Tokens
    Int,
    Plus,
    Minus,
    Star,
    Slash,
    LParen,
    RParen,
    // Nodes
    Factor,
    Term,
    Root,
}

pub type Miniml = SyntaxKind;
