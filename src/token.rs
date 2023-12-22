use crate::num::Num;
use logos::Logos;
use std::fmt::Display;

fn format_string(lex: &mut logos::Lexer<Token>) -> Option<Vec<Token>> {
    // let mut tokens = Vec::new();
    todo!()
}

#[derive(Logos, Debug, Clone, PartialEq, Default)]
pub enum Token {
    #[default]
    Error,
    #[regex(r"--.*", logos::skip)]
    Comment,
    #[regex(r" +")]
    Whitespace,
    #[regex(r"\t")]
    Tab,
    #[regex(r"[\n\r]+")]
    Newline,

    // Literals and identifiers
    // #[regex(r"-?0b(0|1)+(/-?(0|1)+)?", |lex| Rational64::from_str_radix(&lex.slice()[2..], 2).ok().map(Num))]
    // #[regex(r"-?0o[0-7]+(/-?[0-7]+)?", |lex| Rational64::from_str_radix(&lex.slice()[2..], 8).ok().map(Num))]
    // #[regex(r"-?0x[0-9a-fA-F]+(/-?[0-9a-fA-F]+)?", |lex| Rational64::from_str_radix(&lex.slice()[2..], 16).ok().map(Num))]
    #[regex(r"-?([1-9]\d*|0)(/-?-[1-9]\d*|0)?", |lex| lex.slice().parse().ok())]
    Num(Num),
    #[regex(r"true|false", |lex| lex.slice().parse().ok())]
    Bool(bool),
    #[regex(r#""(\\.|[^"\\])*""#, |lex| lex.slice().to_string())]
    String(String),
    #[token(r#"f""#, format_string)]
    FormatString(Vec<Self>),
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*", |lex| lex.slice().to_string())]
    Ident(String),

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
    #[token("enum")]
    Enum,
    #[token("def")]
    Def,
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
            Self::Error => write!(f, "<Error>"),
            Self::Comment => write!(f, "<Comment>"),
            Self::Whitespace => write!(f, "<Whitespace>"),
            Self::Tab => write!(f, "<Tab>"),
            Self::Newline => write!(f, "<Newline>"),
            Self::Num(n) => write!(f, "Num({})", n),
            Self::Bool(b) => write!(f, "Bool({})", b),
            Self::String(s) => write!(f, "String({})", s),
            Self::FormatString(tokens) => {
                for t
                write!(f, "FormatString({})")
            }
            Self::Ident(name) => write!(f, "Ident({})", name),
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
            Self::Enum => write!(f, "enum"),
            Self::Def => write!(f, "def"),
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
