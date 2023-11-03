use crate::util::intern::InternedString;
use logos::Logos;
use num_rational::Rational64;
use std::fmt::Display;

#[derive(Logos, Debug, Clone, PartialEq)]
pub enum Token {
    #[regex(r#"-?((0b[0-1]+)|(0o[0-7]+)|(0x[0-9a-fA-F]+)|([1-9]\d*|0))(/-?((0b[0-1]+)|(0o[0-7]+)|(0x[0-9a-fA-F]+)|([1-9]\d*|0)))?"#, |lex| lex.slice().parse().ok())]
    Num(Rational64),
    #[regex(r"true|false", |lex| lex.slice().parse().ok())]
    Bool(bool),
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*", |lex| InternedString::from(lex.slice()))]
    Ident(InternedString),
    #[token("\\")]
    Lambda,
    #[token("->")]
    Arrow,
    #[token("=")]
    Eq,
    #[token("let")]
    Let,
    #[token("in")]
    In,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Star,
    #[token("/")]
    Slash,
    #[token("!")]
    Bang,
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[regex(r"[ \t\n\r]", logos::skip)]
    Whitespace,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Num(n) => write!(f, "{}", n),
            Self::Bool(b) => write!(f, "{}", b),
            Self::Ident(name) => write!(f, "{}", name),
            Self::Lambda => write!(f, "\\"),
            Self::Arrow => write!(f, "->"),
            Self::Eq => write!(f, "="),
            Self::Let => write!(f, "let"),
            Self::In => write!(f, "in"),
            Self::Plus => write!(f, "+"),
            Self::Minus => write!(f, "-"),
            Self::Star => write!(f, "*"),
            Self::Slash => write!(f, "/"),
            Self::Bang => write!(f, "!"),
            Self::LParen => write!(f, "("),
            Self::RParen => write!(f, ")"),
            Self::Whitespace => write!(f, "WS"),
        }
    }
}
