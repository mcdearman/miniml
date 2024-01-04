use crate::util::intern::InternedString;
use logos::Logos;
use num_rational::Rational64;
use std::fmt::{Debug, Display};

#[derive(Logos, Debug, Copy, Clone, Default, PartialEq)]
pub enum Token {
    #[default]
    Eof,
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

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Eof => write!(f, "<EOF>"),
            Token::Whitespace => write!(f, "Whitespace"),
            Token::Comment => write!(f, "Comment"),
            Token::Symbol(name) => write!(f, "Symbol({})", name),
            Token::Number(n) => write!(f, "Number({})", n),
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
