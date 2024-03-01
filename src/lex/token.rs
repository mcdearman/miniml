use crate::utils::intern::InternedString;
use logos::Logos;
use num_rational::Rational64;
use std::fmt::Display;

#[derive(Logos, Debug, Clone, PartialEq)]
pub enum Token {
    Error,
    #[regex(r"--.*", logos::skip)]
    Comment,
    #[regex(r"[ \t\n\r]+", logos::skip)]
    Whitespace,
    // Literals and identifiers
    #[regex(
        r"-?((0b[0-1]+)|(0o[0-7]+)|(0x[0-9a-fA-F]+)|([1-9]\d*|0))", 
        |lex| lex.slice().parse().ok(),
        priority = 3
    )]
    Int(i64),
    #[regex(
        r"([0-9]*[.])?[0-9]+", 
        |lex| lex.slice().parse().ok(),
        priority = 2
    )]
    Real(f64),
    #[regex(
        r"-?((0b[0-1]+)|(0o[0-7]+)|(0x[0-9a-fA-F]+)|([1-9]\d*|0))(/-?((0b[0-1]+)|(0o[0-7]+)|(0x[0-9a-fA-F]+)|([1-9]\d*|0)))", 
        |lex| lex.slice().parse().ok())]
    Rational(Rational64),
    #[regex(r"true|false", |lex| lex.slice().parse().ok())]
    Bool(bool),
    #[regex(r#""(\\.|[^"\\])*""#, |lex| InternedString::from(lex.slice()))]
    String(InternedString),
    #[regex(r"'(\\.|[^'\\])'", |lex| lex.slice().chars().nth(1))]
    Char(char),
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*", |lex| InternedString::from(lex.slice()))]
    Ident(InternedString),

    // Punctuation
    #[token("_")]
    Wildcard,
    #[token("\\")]
    Backslash,
    #[token("<-")]
    LArrow,
    #[token("->")]
    RArrow,
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
    #[token("^")]
    Caret,
    #[token("or")]
    Or,
    #[token("and")]
    And,
    #[token("not")]
    Not,
    #[token("=")]
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
    #[token(",")]
    Comma,
    #[token(".")]
    Period,
    #[token("..")]
    DoublePeriod,
    #[token(":")]
    Colon,
    #[token("::")]
    DoubleColon,
    #[token(";")]
    SemiColon,
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
    #[token("#[")]
    HashLBrack,
    #[token("|")]
    Bar,
    #[token("|>")]
    Pipe,

    // Keywords
    #[token("pub")]
    Pub,
    #[token("mod")]
    Mod,
    #[token("end")]
    End,
    #[token("use")]
    Use,
    #[token("let")]
    Let,
    #[token("in")]
    In,
    #[token("if")]
    If,
    #[token("match")]
    Match,
    #[token("with")]
    With,
    #[token("then")]
    Then,
    #[token("else")]
    Else,
    #[token("type")]
    Type,
    #[token("alias")]
    Alias,
    #[token("class")]
    Class,
    #[token("as")]
    As,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Token::*;
        match self {
            Error => write!(f, "Error"),
            Comment => write!(f, "Comment"),
            Whitespace => write!(f, "Whitespace"),
            Int(i) => write!(f, "Int({})", i),
            Real(r) => write!(f, "Real({})", r),
            Rational(r) => write!(f, "Rational({})", r),
            Bool(b) => write!(f, "Bool({})", b),
            String(s) => write!(f, "String({})", s),
            Char(c) => write!(f, "Char({})", c),
            Ident(s) => write!(f, "Ident({})", s),
            Wildcard => write!(f, "Wildcard"),
            Backslash => write!(f, "Backslash"),
            LArrow => write!(f, "LArrow"),
            RArrow => write!(f, "RArrow"),
            Plus => write!(f, "Plus"),
            Minus => write!(f, "Minus"),
            Star => write!(f, "Star"),
            Slash => write!(f, "Slash"),
            Percent => write!(f, "Percent"),
            Caret => write!(f, "Caret"),
            Or => write!(f, "Or"),
            And => write!(f, "And"),
            Not => write!(f, "Not"),
            Eq => write!(f, "Eq"),
            Neq => write!(f, "Neq"),
            Lt => write!(f, "Lt"),
            Gt => write!(f, "Gt"),
            Leq => write!(f, "Leq"),
            Geq => write!(f, "Geq"),
            Bang => write!(f, "Bang"),
            Comma => write!(f, "Comma"),
            Period => write!(f, "Period"),
            DoublePeriod => write!(f, "DoublePeriod"),
            Colon => write!(f, "Colon"),
            DoubleColon => write!(f, "DoubleColon"),
            SemiColon => write!(f, "SemiColon"),
            LParen => write!(f, "LParen"),
            RParen => write!(f, "RParen"),
            LBrace => write!(f, "LBrace"),
            RBrace => write!(f, "RBrace"),
            LBrack => write!(f, "LBrack"),
            RBrack => write!(f, "RBrack"),
            HashLBrack => write!(f, "HashLBrack"),
            Bar => write!(f, "Bar"),
            Pipe => write!(f, "Pipe"),
            Pub => write!(f, "Pub"),
            Mod => write!(f, "Mod"),
            End => write!(f, "End"),
            Use => write!(f, "Use"),
            Let => write!(f, "Let"),
            In => write!(f, "In"),
            If => write!(f, "If"),
            Match => write!(f, "Match"),
            With => write!(f, "With"),
            Then => write!(f, "Then"),
            Else => write!(f, "Else"),
            Type => write!(f, "Type"),
            Alias => write!(f, "Alias"),
            Class => write!(f, "Class"),
            As => write!(f, "As"),
        }
    }
}
