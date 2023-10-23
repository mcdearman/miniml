use logos::Logos;
use num_complex::Complex64;
use num_rational::Rational64;
use std::fmt::{Debug, Display};
use crate::util::intern::InternedString;
use super::ast::{Nat, Int};

#[derive(Logos, Debug, Clone, PartialEq)]
pub enum Token {
    Eof,
    #[regex("[ \n\t\r]+", logos::skip)]
    Whitespace,
    #[regex(r#"--[^\n]*|/\*([^*]|\**[^*/])*\*+/"#, logos::skip)]
    Comment,
    #[regex(r##"([A-Za-z]|_)([A-Za-z]|_|\d)*"##, callback = |lex| InternedString::from(lex.slice()))]
    Ident(InternedString),
    #[regex(r#"0b[0-1]+"#, callback = |lex| lex.slice().parse::<Nat>().ok())]
    Nat(Nat),
    #[regex(
        r#"((0b[0-1]+)|(0o[0-7]+)|(0x[0-9a-fA-F]+)|([1-9]\d*|0))"#,
        priority = 2,
        callback = |lex| lex.slice().parse::<Int>().ok()
    )]
    Int(Int),
    #[regex(
        r#"-?((0b[0-1]+)|(0o[0-7]+)|(0x[0-9a-fA-F]+)|([1-9]\d*|0))/-?((0b[0-1]+)|(0o[0-7]+)|(0x[0-9a-fA-F]+)|([1-9]\d*|0))"#,
        priority = 1,
        callback = |lex| lex.slice().parse::<Rational64>().ok()
    )]
    Rational(Rational64),
    #[regex(
        r#"((\d+(\.\d+))|(\.\d+))([Ee](\+|-)?\d+)?"#, 
        priority = 1, 
        callback = |lex| lex.slice().parse::<f64>().ok()
    )]
    Real(f64),
    #[regex(
        r#"((\d+(\.\d+)?)|(\.\d+))([Ee](\+|-)?\d+)?i"#, 
        priority = 0, 
        callback = |lex| lex.slice().parse::<Complex64>().ok()
    )]
    Complex(Complex64),
    #[regex(r#"'\w'"#, callback = |lex| lex.slice().chars().nth(1))]
    Char(char),
    #[regex(r#""((\\"|\\\\)|[^\\"])*""#, callback = |lex| InternedString::from(lex.slice()))]
    String(InternedString),
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
    #[token("\\")]
    Backslash,
    #[token("->")]
    Arrow,
    #[token("=>")]
    FatArrow,
    #[token("|")]
    Pipe,
    #[token("|>")]
    PipeArrow,
    #[token("=")]
    Eq,
    #[token("<")]
    Lt,
    #[token(">")]
    Gt,
    #[token("!=")]
    Neq,
    #[token("<=")]
    Leq,
    #[token(">=")]
    Geq,

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
    #[token(",")]
    Comma,
    #[token(".")]
    Period,
    #[token(";")]
    Semicolon,
    #[token(":")]
    Colon,

    #[token("pub")]
    Pub,
    #[token("mod")]
    Module,
    #[token("end")]
    End,
    #[token("use")]
    Use,
    #[token("let")]
    Let,
    #[token("fn")]
    Fn,
    #[token("struct")]
    Struct,
    #[token("match")]
    Match,
    #[token("with")]
    With,
    #[token("and")]
    And,
    #[token("or")]
    Or,
    #[token("not")]
    Not,
    #[token("if")]
    If,
    #[token("then")]
    Then,
    #[token("elif")]
    Elif,
    #[token("else")]
    Else,
    #[token("in")]
    In,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Eof => f.write_str("<EOF>"),
            Token::Whitespace => f.write_str("<WS>"),
            Token::Comment => f.write_str("Comment"),
            Token::Ident(name) => write!(f, "Ident({})", name),
            Token::Nat(n) => write!(f, "Nat({})", n),
            Token::Int(i) => write!(f, "Int({})", i),
            Token::Rational(r) => write!(f, "Rational({})", r),
            Token::Real(r) => write!(f, "Real({})", r),
            Token::Complex(c) => write!(f, "Complex({})", c),
            Token::Char(c) => write!(f, "Char({})", c),
            Token::String(s) => write!(f, "String({})", s),
            Token::Plus => f.write_str("+"),
            Token::Minus => f.write_str("-"),
            Token::Star => f.write_str("*"),
            Token::Slash => f.write_str("/"),
            Token::Percent => f.write_str("%"),
            Token::Caret => f.write_str("^"),
            Token::Backslash => f.write_str("\\"),
            Token::Arrow => f.write_str("->"),
            Token::FatArrow => f.write_str("=>"),
            Token::Pipe => f.write_str("|"),
            Token::PipeArrow => f.write_str("|>"),  
            Token::Eq => f.write_str("="),
            Token::Lt => f.write_str("<"),
            Token::Gt => f.write_str(">"),
            Token::Neq => f.write_str("!="),
            Token::Leq => f.write_str("<="),
            Token::Geq => f.write_str(">="),
            Token::LParen => f.write_str("("),
            Token::RParen => f.write_str(")"),
            Token::LBrack => f.write_str("["),
            Token::RBrack => f.write_str("]"),
            Token::LBrace => f.write_str("{"),
            Token::RBrace => f.write_str("}"),
            Token::Comma => f.write_str(","),
            Token::Period => f.write_str("."),
            Token::Semicolon => f.write_str(";"),
            Token::Colon => f.write_str(":"),
            Token::Pub => f.write_str("pub"),
            Token::Module => f.write_str("mod"),
            Token::End => f.write_str("end"),
            Token::Use => f.write_str("use"),
            Token::Let => f.write_str("let"),
            Token::Fn => f.write_str("fn"),
            Token::Struct => f.write_str("struct"),
            Token::Match => f.write_str("match"),
            Token::With => f.write_str("with"),
            Token::And => f.write_str("and"),
            Token::Or => f.write_str("or"),
            Token::Not => f.write_str("not"),
            Token::If => f.write_str("if"),
            Token::Then => f.write_str("then"),
            Token::Elif => f.write_str("elif"),
            Token::Else => f.write_str("else"),
            Token::In => f.write_str("in"),
        }
    }
}

mod tests {
    use logos::Logos;

    #[test]
    fn test_lex_sub() {
        let src = "1-1";
        let tokens = super::Token::lexer(src).spanned().collect::<Vec<_>>();
        insta::assert_debug_snapshot!(tokens);
    }
}
