use logos::Logos;
use miniml_common::{interner::InternedString, span::Span};
use num_rational::Rational64;
use std::fmt::{Debug, Display};

#[derive(Debug, Clone)]
pub struct Token {
    kind: TokenKind,
    span: Span,
}

impl Token {
    pub fn new(kind: TokenKind, span: Span) -> Self {
        Self { kind, span }
    }

    pub fn kind(&self) -> &TokenKind {
        &self.kind
    }

    pub fn span(&self) -> &Span {
        &self.span
    }
}

#[derive(Logos, Debug, Copy, Clone, Default, PartialEq)]
pub enum TokenKind {
    #[default]
    Error,
    #[regex(r"[ \t\r\n\f]+", logos::skip)]
    Whitespace,
    #[regex(r#";[^\n]*"#)]
    Comment,
    #[regex(r#"[^\[\]()\s,{};]+"#, |lex| InternedString::from(lex.slice()))]
    Symbol(InternedString),
    #[regex(
        r#"((0b[0-1]+)|(0o[0-7]+)|(0x[0-9a-fA-F]+)|([1-9]\d*|0))(/-?((0b[0-1]+)|(0o[0-7]+)|(0x[0-9a-fA-F]+)|([1-9]\d*|0)))?"#,
        priority = 2,
        callback = |lex| lex.slice().parse::<Rational64>().ok()
    )]
    Number(Rational64),
    #[regex(r#""((\\"|\\\\)|[^\\"])*""#, |lex| InternedString::from(lex.slice()))]
    String(InternedString),

    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("[")]
    LBrack,
    #[token("]")]
    RBrack,
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
    #[token(":")]
    Colon,
    #[token(".")]
    Period,
    #[token(",")]
    Comma,
    #[token(",@")]
    CommaAt,
    #[token("#")]
    Hash,
    #[token("'")]
    Quote,
    #[token("`")]
    Backquote,
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use TokenKind::*;
        match self {
            Error => write!(f, "Error"),
            Whitespace => write!(f, "Whitespace"),
            Comment => write!(f, "Comment"),
            Symbol(name) => write!(f, "Symbol({})", name),
            Number(n) => write!(f, "Number({})", n),
            String(s) => write!(f, "String({:?})", s),
            LParen => write!(f, "("),
            RParen => write!(f, ")"),
            LBrack => write!(f, "["),
            RBrack => write!(f, "]"),
            LBrace => write!(f, "{{"),
            RBrace => write!(f, "}}"),
            Colon => write!(f, ":"),
            Period => write!(f, "."),
            Comma => write!(f, ","),
            CommaAt => write!(f, ",@"),
            Hash => write!(f, "#"),
            Quote => write!(f, "'"),
            Backquote => write!(f, "`"),
        }
    }
}
