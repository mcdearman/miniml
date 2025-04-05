use mmc_utils::{intern::InternedString, rational::Rational64, span::Span};

#[derive(Debug, Clone, PartialEq)]
pub struct TokenTree {
    pub kind: TokenTreeKind,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenTreeKind {
    Comment,
    Whitespace,
    Delim(Delim),

    // Literals and identifiers
    Int(i64),
    Real(f64),
    Rational(Rational64),
    Bool(bool),
    String(InternedString),
    Char(char),
    LowerIdent(InternedString),
    UpperIdent(InternedString),
    OpIdent(InternedString),
    ConOpIdent(InternedString),

    // Punctuation
    Wildcard,
    Backslash,
    LArrow,
    RArrow,
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Caret,
    Or,
    And,
    Not,
    Eq,
    Neq,
    Lt,
    Gt,
    Leq,
    Geq,
    Bang,
    Comma,
    Period,
    DoublePeriod,
    DoublePeriodEq,
    Colon,
    SemiColon,
    Bar,
    LPipe,
    RPipe,
    At,
    Backtick,

    // Keywords
    Pub,
    Mod,
    Use,
    Def,
    Let,
    In,
    Match,
    With,
    If,
    Then,
    Else,
    Data,
    Type,
    Class,
    Instance,
    As,
    Error,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Delim {
    pub kind: DelimKind,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum DelimKind {
    Paren(Vec<TokenTree>),
    Brace(Vec<TokenTree>),
    Brack(Vec<TokenTree>),
    Error,
}
