use mmc_utils::{intern::InternedString, rational::Rational64, span::Span};

#[derive(Debug, Clone, PartialEq)]
pub struct TokenTree {
    kind: TokenTreeKind,
    span: Span,
}

impl TokenTree {
    #[inline(always)]
    pub fn new(kind: TokenTreeKind, span: Span) -> Self {
        Self { kind, span }
    }

    #[inline(always)]
    pub fn kind(&self) -> &TokenTreeKind {
        &self.kind
    }

    #[inline(always)]
    pub fn span(&self) -> Span {
        self.span
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenTreeKind {
    Token(Token),
    Delim(Delim),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    kind: TokenKind,
    span: Span,
}

impl Token {
    #[inline(always)]
    pub fn new(kind: TokenKind, span: Span) -> Self {
        Self { kind, span }
    }

    #[inline(always)]
    pub fn kind(&self) -> &TokenKind {
        &self.kind
    }

    #[inline(always)]
    pub fn span(&self) -> Span {
        self.span
    }

    #[inline(always)]
    pub fn text<'src>(&self, src: &'src str) -> &'src str {
        &src[self.span]
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
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
