use crate::num::Num;

#[derive(Debug, Clone)]
pub enum TokenTree {
    Error,
    Comment,
    Space,
    Tab,
    Newline,

    // Literals and identifiers
    Num(Num),
    Bool(bool),
    String(String),
    // FormatString(Vec<Self>),
    Ident(String),

    // Punctuation
    Lambda,
    Arrow,
    Assign,
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Eq,
    Neq,
    Lt,
    Leq,
    Geq,
    Bang,
    LParen,
    RParen,
    RBrace,
    LBrack,
    RBrack,
    Pipe,
    PipeArrow,
    Subtype,

    Semicolon,
    Comma,
    Wildcard,

    // Keywords
    Class,
    Enum,
    Trait,
    Let,
    In,
    Match,
    With,
    If,
    Then,
    Else,
}
