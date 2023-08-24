use logos::Logos;
use miniml_util::span::Spanned;
use std::fmt::{Debug, Display};

#[derive(Logos, Debug, Clone, PartialEq)]
pub enum TokenKind {
    Eof,
    #[regex("[ \n\t\r]+", logos::skip)]
    Whitespace,
    #[regex(r#"--[^\n]*|/\*([^*]|\**[^*/])*\*+/"#, logos::skip)]
    Comment,
    #[regex(r##"([A-Za-z]|_)([A-Za-z]|_|\d)*"##)]
    Ident,
    #[regex(
        r#"((0b[0-1]+)|(0o[0-7]+)|(0x[0-9a-fA-F]+)|([1-9]\d*|0))"#,
        priority = 2
    )]
    Int,
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

impl Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                TokenKind::Eof => "<EOF>",
                TokenKind::Whitespace => "<WS>",
                TokenKind::Comment => "Comment",
                TokenKind::Ident => "Ident",
                TokenKind::Int => "Int",
                TokenKind::Rational => "Rational",
                TokenKind::Real => "Real",
                TokenKind::Complex => "Complex",
                TokenKind::Char => "Char",
                TokenKind::String => "String",
                TokenKind::Plus => "+",
                TokenKind::Minus => "-",
                TokenKind::Star => "*",
                TokenKind::Slash => "/",
                TokenKind::Percent => "%",
                TokenKind::Caret => "^",
                TokenKind::Backslash => "\\",
                TokenKind::Arrow => "->",
                TokenKind::FatArrow => "=>",
                TokenKind::Pipe => "|",
                TokenKind::PipeArrow => "|>",
                TokenKind::Eq => "=",
                TokenKind::Lt => "<",
                TokenKind::Gt => ">",
                TokenKind::Neq => "!=",
                TokenKind::Leq => "<=",
                TokenKind::Geq => ">=",
                TokenKind::LParen => "(",
                TokenKind::RParen => ")",
                TokenKind::LBrack => "[",
                TokenKind::RBrack => "]",
                TokenKind::LBrace => "{",
                TokenKind::RBrace => "}",
                TokenKind::Comma => ",",
                TokenKind::Period => ".",
                TokenKind::Semicolon => ";",
                TokenKind::Colon => ":",
                TokenKind::Pub => "pub",
                TokenKind::Module => "mod",
                TokenKind::End => "end",
                TokenKind::Use => "use",
                TokenKind::Const => "const",
                TokenKind::Let => "let",
                TokenKind::Fn => "fn",
                TokenKind::Struct => "struct",
                TokenKind::Match => "match",
                TokenKind::With => "with",
                TokenKind::And => "and",
                TokenKind::Or => "or",
                TokenKind::Not => "not",
                TokenKind::If => "if",
                TokenKind::Then => "then",
                TokenKind::Elif => "elif",
                TokenKind::Else => "else",
                TokenKind::In => "in",
            }
        )
    }
}

pub type Token = Spanned<TokenKind>;

mod tests {
    use logos::Logos;

    #[test]
    fn test_lex_sub() {
        let src = "1-1";
        let tokens = super::TokenKind::lexer(src).spanned().collect::<Vec<_>>();
        insta::assert_debug_snapshot!(tokens);
    }
}
