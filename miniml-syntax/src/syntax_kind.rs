use cstree::{RawSyntaxKind, Syntax};

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
    BinExpr,
    Root,
}
