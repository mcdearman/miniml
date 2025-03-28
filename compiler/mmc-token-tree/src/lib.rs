use mmc_utils::{intern::InternedString, rational::Rational64};

#[derive(Debug, Clone, PartialEq)]
pub enum TokenTree {
    Paren(Vec<Token>),
    Brace(Vec<Token>),
    Brack(Vec<Token>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Error,
    Comment,
    Whitespace,

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
    End,
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
}
