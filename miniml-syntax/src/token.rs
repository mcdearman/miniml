use logos::Logos;
use miniml_util::{intern::InternedString, span::Spanned};
use std::fmt::{Debug, Display};

use crate::ast::Int;

#[derive(Logos, Debug, Clone, PartialEq)]
pub enum Token {
    Eof,
    #[regex("[ \n\t\r]+", logos::skip)]
    Whitespace,
    #[regex(r#"--[^\n]*|/\*([^*]|\**[^*/])*\*+/"#, logos::skip)]
    Comment,
    #[regex(r##"([A-Za-z]|_)([A-Za-z]|_|\d)*"##, callback = |lex| InternedString::from(lex.slice()))]
    Ident(InternedString),
    #[regex(
        r#"((0b[0-1]+)|(0o[0-7]+)|(0x[0-9a-fA-F]+)|([1-9]\d*|0))"#,
        priority = 2
    )]
    Int(Int),
    #[regex(
        r#"((0b[0-1]+)|(0o[0-7]+)|(0x[0-9a-fA-F]+)|([1-9]\d*|0))/-?((0b[0-1]+)|(0o[0-7]+)|(0x[0-9a-fA-F]+)|([1-9]\d*|0))"#,
        priority = 1
    )]
    Rational,
    #[regex(r#"((\d+(\.\d+))|(\.\d+))([Ee](\+|-)?\d+)?"#, priority = 1)]
    Real,
    #[regex(r#"((\d+(\.\d+)?)|(\.\d+))([Ee](\+|-)?\d+)?i"#, priority = 0)]
    Complex,
    #[regex(r#"'\w'"#)]
    Char,
    #[regex(r#""((\\"|\\\\)|[^\\"])*""#)]
    String,
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
    #[token("const")]
    Const,
    #[token("let")]
    Let,
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
        write!(
            f,
            "{}",
            match self {
                Token::Eof => "<EOF>",
                Token::Whitespace => "<WS>",
                Token::Comment => "Comment",
                Token::Ident => "Ident",
                Token::Int => "Int",
                Token::Rational => "Rational",
                Token::Real => "Real",
                Token::Complex => "Complex",
                Token::Char => "Char",
                Token::String => "String",
                Token::Plus => "+",
                Token::Minus => "-",
                Token::Star => "*",
                Token::Slash => "/",
                Token::Percent => "%",
                Token::Caret => "^",
                Token::Backslash => "\\",
                Token::Arrow => "->",
                Token::FatArrow => "=>",
                Token::Pipe => "|",
                Token::PipeArrow => "|>",
                Token::Eq => "=",
                Token::Lt => "<",
                Token::Gt => ">",
                Token::Neq => "!=",
                Token::Leq => "<=",
                Token::Geq => ">=",
                Token::LParen => "(",
                Token::RParen => ")",
                Token::LBrack => "[",
                Token::RBrack => "]",
                Token::LBrace => "{",
                Token::RBrace => "}",
                Token::Comma => ",",
                Token::Period => ".",
                Token::Semicolon => ";",
                Token::Colon => ":",
                Token::Pub => "pub",
                Token::Module => "mod",
                Token::End => "end",
                Token::Use => "use",
                Token::Const => "const",
                Token::Let => "let",
                Token::Struct => "struct",
                Token::Match => "match",
                Token::With => "with",
                Token::And => "and",
                Token::Or => "or",
                Token::Not => "not",
                Token::If => "if",
                Token::Then => "then",
                Token::Elif => "elif",
                Token::Else => "else",
                Token::In => "in",
            }
        )
    }
}

// pub type Token = Spanned<Token>;

mod tests {
    use logos::Logos;

    #[test]
    fn test_lex_sub() {
        let src = "1-1";
        let tokens = super::Token::lexer(src).spanned().collect::<Vec<_>>();
        insta::assert_debug_snapshot!(tokens);
    }
}
