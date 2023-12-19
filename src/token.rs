use crate::num::Num;
use logos::Logos;
use std::fmt::Display;

#[derive(Logos, Debug, Clone, PartialEq, Default)]
pub enum Token {
    #[default]
    Eof,
    #[regex(r"[ \t\r\n\f]+", logos::skip)]
    Whitespace,
    #[regex(r#";[^\n]*"#)]
    Comment,
    #[regex(r"[^\[\]()\s,{};]+", |lex| lex.slice().to_string())]
    Ident(String),
    #[regex(
        r"-?([1-9]\d*|0)(/-?-[1-9]\d*|0)?",
        priority = 2,
        callback = |lex| lex.slice().parse().ok()
    )]
    Num(Num),
    #[regex(r#""(\\.|[^"\\])*""#, |lex| lex.slice().to_string())]
    String(String),

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

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Eof => write!(f, "<EOF>"),
            Token::Whitespace => write!(f, "<Whitespace>"),
            Token::Comment => write!(f, "<Comment>"),
            Token::Ident(name) => write!(f, "Ident({})", name),
            Token::Num(n) => write!(f, "Number({})", n),
            Token::String(s) => write!(f, "String({:?})", s),
            Token::LParen => write!(f, "("),
            Token::RParen => write!(f, ")"),
            Token::LBrack => write!(f, "["),
            Token::RBrack => write!(f, "]"),
            Token::LBrace => write!(f, "{{"),
            Token::RBrace => write!(f, "}}"),
            Token::Colon => write!(f, ":"),
            Token::Period => write!(f, "."),
            Token::Comma => write!(f, ","),
            Token::CommaAt => write!(f, ",@"),
            Token::Hash => write!(f, "#"),
            Token::Quote => write!(f, "'"),
            Token::Backquote => write!(f, "`"),
        }
    }
}
