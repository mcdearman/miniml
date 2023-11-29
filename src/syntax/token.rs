use crate::util::intern::InternedString;
use logos::Logos;
use num_rational::Rational64;
use std::fmt::Display;

#[derive(Logos, Debug, Clone, PartialEq)]
pub enum Token {
    #[regex(r"--.*", logos::skip)]
    Comment,
    #[regex(r"[ \t\n\r]", logos::skip)]
    Whitespace,

    // Literals and identifiers
    #[regex(r#"((0b[0-1]+)|(0o[0-7]+)|(0x[0-9a-fA-F]+)|([1-9]\d*|0))(/((0b[0-1]+)|(0o[0-7]+)|(0x[0-9a-fA-F]+)|([1-9]\d*|0)))?"#, |lex| lex.slice().parse().ok())]
    Num(Rational64),
    #[regex(r"true|false", |lex| lex.slice().parse().ok())]
    Bool(bool),
    #[regex(r#""(\\.|[^"\\])*""#, |lex| InternedString::from(lex.slice()))]
    String(InternedString),

    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*", |lex| InternedString::from(lex.slice()))]
    Ident(InternedString),

    // Punctuation
    #[token("\\")]
    Lambda,
    #[token("->")]
    Arrow,
    #[token("=")]
    Assign,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Star,
    #[token("/")]
    Slash,
    #[token("%")]
    Percent,
    #[token("==")]
    Eq,
    #[token("!=")]
    Neq,
    #[token("<")]
    Lt,
    #[token(">")]
    Gt,
    #[token("<=")]
    Leq,
    #[token(">=")]
    Geq,
    #[token("!")]
    Bang,
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
    #[token("[")]
    LBrack,
    #[token("]")]
    RBrack,
    #[token("|")]
    Pipe,
    #[token("|>")]
    PipeArrow,
    #[token("<:")]
    Subtype,

    #[token(":")]
    Colon,
    #[token(";")]
    Semicolon,
    #[token(",")]
    Comma,
    #[token("_")]
    Wildcard,

    // Keywords
    #[token("class")]
    Class,
    #[token("let")]
    Let,
    #[token("in")]
    In,
    #[token("match")]
    Match,
    #[token("with")]
    With,
    #[token("if")]
    If,
    #[token("then")]
    Then,
    #[token("else")]
    Else,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Comment => write!(f, "Comment"),
            Self::Whitespace => write!(f, "WS"),
            Self::Num(n) => write!(f, "{}", n),
            Self::Bool(b) => write!(f, "{}", b),
            Self::String(s) => write!(f, "{}", s),
            Self::Ident(name) => write!(f, "{}", name),
            Self::Lambda => write!(f, "\\"),
            Self::Arrow => write!(f, "->"),
            Self::Assign => write!(f, "="),
            Self::Plus => write!(f, "+"),
            Self::Minus => write!(f, "-"),
            Self::Star => write!(f, "*"),
            Self::Slash => write!(f, "/"),
            Self::Percent => write!(f, "%"),
            Self::Eq => write!(f, "=="),
            Self::Neq => write!(f, "!="),
            Self::Lt => write!(f, "<"),
            Self::Gt => write!(f, ">"),
            Self::Leq => write!(f, "<="),
            Self::Geq => write!(f, ">="),
            Self::Bang => write!(f, "!"),
            Self::LParen => write!(f, "("),
            Self::RParen => write!(f, ")"),
            Self::LBrace => write!(f, "{{"),
            Self::RBrace => write!(f, "}}"),
            Self::LBrack => write!(f, "["),
            Self::RBrack => write!(f, "]"),
            Self::Pipe => write!(f, "|"),
            Self::PipeArrow => write!(f, "|>"),
            Self::Subtype => write!(f, "<:"),
            Self::Colon => write!(f, ":"),
            Self::Semicolon => write!(f, ";"),
            Self::Comma => write!(f, ","),
            Self::Wildcard => write!(f, "_"),
            Self::Class => write!(f, "class"),
            Self::Let => write!(f, "let"),
            Self::In => write!(f, "in"),
            Self::Match => write!(f, "match"),
            Self::With => write!(f, "with"),
            Self::If => write!(f, "if"),
            Self::Then => write!(f, "then"),
            Self::Else => write!(f, "else"),
        }
    }
}
