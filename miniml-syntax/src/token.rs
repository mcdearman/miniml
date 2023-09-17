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
        priority = 2,
        callback = |lex| lex.slice().parse::<Int>().ok()
    )]
    Int(Int),
    #[regex(
        r#"((0b[0-1]+)|(0o[0-7]+)|(0x[0-9a-fA-F]+)|([1-9]\d*|0))/-?((0b[0-1]+)|(0o[0-7]+)|(0x[0-9a-fA-F]+)|([1-9]\d*|0))"#,
        priority = 1
    )]
    // Rational,
    // #[regex(r#"((\d+(\.\d+))|(\.\d+))([Ee](\+|-)?\d+)?"#, priority = 1)]
    // Real,
    // #[regex(r#"((\d+(\.\d+)?)|(\.\d+))([Ee](\+|-)?\d+)?i"#, priority = 0)]
    // Complex,
    // #[regex(r#"'\w'"#)]
    // Char,
    // #[regex(r#""((\\"|\\\\)|[^\\"])*""#)]
    // String,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Star,
    #[token("/")]
    Slash,
    // #[token("%")]
    // Percent,
    // #[token("^")]
    // Caret,
    #[token("\\")]
    Backslash,
    #[token("->")]
    Arrow,
    // #[token("=>")]
    // FatArrow,
    // #[token("|")]
    // Pipe,
    // #[token("|>")]
    // PipeArrow,
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

    // #[token("pub")]
    // Pub,
    // #[token("mod")]
    // Module,
    // #[token("end")]
    // End,
    // #[token("use")]
    // Use,
    // #[token("const")]
    // Const,
    #[token("let")]
    Let,
    // #[token("struct")]
    // Struct,
    // #[token("match")]
    // Match,
    // #[token("with")]
    // With,
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
            Token::Int(i) => write!(f, "Int({})", i),
            // Token::Rational => "Rational",
            // Token::Real => "Real",
            // Token::Complex => "Complex",
            // Token::Char => "Char",
            // Token::String => "String",
            Token::Plus => f.write_str("+"),
            Token::Minus => f.write_str("-"),
            Token::Star => f.write_str("*"),
            Token::Slash => f.write_str("/"),
            // Token::Percent => "%",
            // Token::Caret => "^",
            Token::Backslash => f.write_str("\\"),
            Token::Arrow => f.write_str("->"),
            // Token::FatArrow => "=>",
            // Token::Pipe => "|",
            // Token::PipeArrow => "|>",
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
            // Token::Pub => "pub",
            // Token::Module => "mod",
            // Token::End => "end",
            // Token::Use => "use",
            // Token::Const => "const",
            Token::Let => f.write_str("let"),
            // Token::Struct => "struct",
            // Token::Match => "match",
            // Token::With => "with",
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
