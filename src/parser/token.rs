use super::span::Span;
use crate::intern::InternedString;
use logos::{Lexer, Logos};
use num_bigint::BigInt;
use num_complex::Complex64;
use num_rational::Rational64;
use std::{
    fmt::Display,
    ops::{Index, Range},
};

#[derive(Logos, Debug, Clone, PartialEq)]
pub enum Token {
    Eof,
    #[regex("[ \n\t\r]+", logos::skip)]
    Whitespace,
    #[regex(r#"--[^\n]*|/\*([^*]|\**[^*/])*\*+/"#, |lex| InternedString::from(lex.slice()))]
    Comment(InternedString),
    #[regex(r##"([A-Za-z]|_)([A-Za-z]|_|\d)*"##, |lex| InternedString::from(lex.slice()))]
    Ident(InternedString),
    #[regex(r#"(0b[0-1]+)|(0o[0-7]+)|(0x[0-9a-fA-F]+)|([1-9]\d*|0)"#, |lex| lex.slice().parse().ok(), priority = 2)]
    Int(i64),
    #[regex(r#"-?\d+/\d+"#, |lex| lex.slice().parse().ok(), priority = 1)]
    Rational(Rational64),
    #[regex(r#"((\d+(\.\d+))|(\.\d+))([Ee](\+|-)?\d+)?"#, |lex| lex.slice().parse().ok(), priority = 1)]
    Real(f64),
    #[regex(r#"((\d+(\.\d+)?)|(\.\d+))([Ee](\+|-)?\d+)?i"#, |lex| lex.slice().parse().ok(), priority = 0)]
    Imag(Complex64),
    #[regex(r#"'\w'"#, |lex| lex.slice().chars().nth(1))]
    Char(char),
    #[regex(r#""((\\"|\\\\)|[^\\"])*""#, |lex| InternedString::from(lex.slice()))]
    String(InternedString),
    #[regex(r#"true|false"#, |lex| lex.slice().parse().ok())]
    Bool(bool),
    #[token("+")]
    Add,
    #[token("-")]
    Sub,
    #[token("*")]
    Mul,
    #[token("/")]
    Div,
    #[token("%")]
    Rem,
    #[token("^")]
    Pow,

    #[token("&&")]
    And,
    #[token("||")]
    Or,
    #[token("\\")]
    Lambda,
    #[token("->")]
    Arrow,
    #[token("=>")]
    FatArrow,
    #[token("|")]
    Pipe,
    #[token("|>")]
    PipeArrow,

    #[token("=")]
    Assign,
    #[token("==")]
    Eql,
    #[token("<")]
    Lss,
    #[token(">")]
    Gtr,
    #[token("!")]
    Not,
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
    #[token(";;")]
    DoubleSemicolon,
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
    #[token("data")]
    Data,
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
    #[token("elif")]
    Elif,
    #[token("in")]
    In,
}
