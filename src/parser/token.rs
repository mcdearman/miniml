use super::span::Span;
use logos::Logos;
use std::{
    fmt::{Debug, Display},
    iter::Peekable,
    vec::IntoIter,
};

#[derive(Logos, Debug, Clone, PartialEq)]
pub enum TokenKind {
    Eof,
    #[regex("[ \n\t\r]+", logos::skip)]
    Whitespace,
    #[regex(r#"--[^\n]*|/\*([^*]|\**[^*/])*\*+/"#)]
    Comment,
    #[regex(r##"([A-Za-z]|_)([A-Za-z]|_|\d)*"##)]
    Ident,
    #[regex(
        r#"(\+|-)*((0b[0-1]+)|(0o[0-7]+)|(0x[0-9a-fA-F]+)|([1-9]\d*|0))(i8|u8|i16|u16|i32|u32|i64|u64|i128|u128)?"#,
        priority = 2
    )]
    Int,
    #[regex(r#"-?\d+/\d+"#, priority = 1)]
    Rational,
    #[regex(r#"((\d+(\.\d+))|(\.\d+))([Ee](\+|-)?\d+)?(f32|f64)?"#, priority = 1)]
    Real,
    #[regex(r#"((\d+(\.\d+)?)|(\.\d+))([Ee](\+|-)?\d+)?i(c32|c64)?"#, priority = 0)]
    Imag,
    #[regex(r#"'\w'"#)]
    Char,
    #[regex(r#""((\\"|\\\\)|[^\\"])*""#)]
    String,
    #[regex(r#"true|false"#)]
    Bool,
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
    #[token("type")]
    Type,
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

#[macro_export]
macro_rules! T {
    [EOF] => {
       $crate::parser::token::TokenKind::Eof
    };
    [ws] => {
       $crate::parser::token::TokenKind::Whitespace
    };
    [comment] => {
       $crate::parser::token::TokenKind::Comment
    };
    [ident] => {
       $crate::parser::token::TokenKind::Ident
    };
    [int] => {
       $crate::parser::token::TokenKind::Int
    };
    [rational] => {
       $crate::parser::token::TokenKind::Rational
    };
    [real] => {
       $crate::parser::token::TokenKind::Real
    };
    [imag] => {
       $crate::parser::token::TokenKind::Imag
    };
    [char] => {
       $crate::parser::token::TokenKind::Char
    };
    [str] => {
       $crate::parser::token::TokenKind::String
    };
    [bool] => {
       $crate::parser::token::TokenKind::Bool
    };
    [+] => {
       $crate::parser::token::TokenKind::Add
    };
    [-] => {
       $crate::parser::token::TokenKind::Sub
    };
    [*] => {
       $crate::parser::token::TokenKind::Mul
    };
    [/] => {
       $crate::parser::token::TokenKind::Div
    };
    [%] => {
       $crate::parser::token::TokenKind::Rem
    };
    [^] => {
       $crate::parser::token::TokenKind::Pow
    };
    [&&] => {
       $crate::parser::token::TokenKind::And
    };
    [||] => {
       $crate::parser::token::TokenKind::Or
    };
    [lambda] => {
       $crate::parser::token::TokenKind::Lambda
    };
    [->] => {
       $crate::parser::token::TokenKind::Arrow
    };
    [=>] => {
       $crate::parser::token::TokenKind::FatArrow
    };
    [|] => {
       $crate::parser::token::TokenKind::Pipe
    };
    [|>] => {
       $crate::parser::token::TokenKind::PipeArrow
    };
    [=] => {
       $crate::parser::token::TokenKind::Assign
    };
    [==] => {
       $crate::parser::token::TokenKind::Eql
    };
    [<] => {
       $crate::parser::token::TokenKind::Lss
    };
    [>] => {
       $crate::parser::token::TokenKind::Gtr
    };
    [!] => {
       $crate::parser::token::TokenKind::Not
    };
    [!=] => {
       $crate::parser::token::TokenKind::Neq
    };
    [<=] => {
       $crate::parser::token::TokenKind::Leq
    };
    [>=] => {
       $crate::parser::token::TokenKind::Geq
    };
    ['('] => {
       $crate::parser::token::TokenKind::LParen
    };
    [')'] => {
       $crate::parser::token::TokenKind::RParen
    };
    ['['] => {
       $crate::parser::token::TokenKind::LBrack
    };
    [']'] => {
       $crate::parser::token::TokenKind::RBrack
    };
    ['{'] => {
       $crate::parser::token::TokenKind::LBrace
    };
    ['}'] => {
       $crate::parser::token::TokenKind::RBrace
    };
    [,] => {
       $crate::parser::token::TokenKind::Comma
    };
    [.] => {
       $crate::parser::token::TokenKind::Period
    };
    [;] => {
       $crate::parser::token::TokenKind::Semicolon
    };
    [;;] => {
       $crate::parser::token::TokenKind::DoubleSemicolon
    };
    [:] => {
       $crate::parser::token::TokenKind::Colon
    };
    [pub] => {
       $crate::parser::token::TokenKind::Pub
    };
    [mod] => {
       $crate::parser::token::TokenKind::Module
    };
    [end] => {
       $crate::parser::token::TokenKind::End
    };
    [use] => {
       $crate::parser::token::TokenKind::Use
    };
    [let] => {
       $crate::parser::token::TokenKind::Let
    };
    [fn] => {
       $crate::parser::token::TokenKind::Fn
    };
    [type] => {
       $crate::parser::token::TokenKind::Type
    };
    [match] => {
       $crate::parser::token::TokenKind::Match
    };
    [with] => {
       $crate::parser::token::TokenKind::With
    };
    [if] => {
       $crate::parser::token::TokenKind::If
    };
    [then] => {
       $crate::parser::token::TokenKind::Then
    };
    [else] => {
       $crate::parser::token::TokenKind::Else
    };
    [elif] => {
       $crate::parser::token::TokenKind::Elif
    };
    [in] => {
       $crate::parser::token::TokenKind::In
    };
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                T![EOF] => "<EOF>",
                T![ws] => "<WS>",
                T![comment] => "Comment",
                T![ident] => "Ident",
                T![int] => "Int",
                T![rational] => "Rational",
                T![real] => "Float",
                T![imag] => "Imag",
                T![char] => "Char",
                T![str] => "String",
                T![bool] => "Bool",
                T![+] => "+",
                T![-] => "-",
                T![*] => "*",
                T![/] => "/",
                T![%] => "%",
                T![^] => "^",
                T![&&] => "&&",
                T![||] => "||",
                T![lambda] => "lambda",
                T![->] => "->",
                T![=>] => "=>",
                T![|] => "|",
                T![|>] => "|>",
                T![==] => "==",
                T![<] => "<",
                T![>] => ">",
                T![=] => "=",
                T![!] => "!",
                T![!=] => "!=",
                T![<=] => "<=",
                T![>=] => ">=",
                T!['('] => "(",
                T![')'] => ")",
                T!['['] => "[",
                T![']'] => "]",
                T!['{'] => "{",
                T!['}'] => "}",
                T![,] => ",",
                T![.] => ".",
                T![;] => ";",
                T![;;] => ";;",
                T![:] => ":",
                T![pub] => "pub",
                T![mod] => "mod",
                T![end] => "end",
                T![use] => "use",
                T![let] => "let",
                T![fn] => "fn",
                T![type] => "type",
                T![match] => "match",
                T![with] => "with",
                T![if] => "if",
                T![then] => "then",
                T![else] => "else",
                T![elif] => "elif",
                T![in] => "in",
            }
        )
    }
}

#[derive(Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl Debug for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?} - {}", self.kind, self.span)
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} - {}", self.kind, self.span)
    }
}

// #[derive(Debug, Clone)]
// pub struct TokenStream {
//     tokens: Peekable<IntoIter<Token>>,
// }

// impl TokenStream {
//     pub fn new(tokens: Vec<Token>) -> Self {
//         Self {
//             tokens: tokens.into_iter().peekable(),
//         }
//     }

//     pub fn peek(&mut self) -> Token {
//         self.tokens
//             .peek()
//             .unwrap_or(&Token {
//                 kind: TokenKind::Eof,
//                 span: Span::new(0, 0),
//             })
//             .clone()
//     }

//     pub fn next(&mut self) -> Token {
//         self.tokens.next().unwrap_or(Token {
//             kind: TokenKind::Eof,
//             span: Span::new(0, 0),
//         })
//     }
// }
// pub trait TokenStream {
//     fn peek(&mut self) -> Token;
//     fn next(&mut self) -> Token;
//     fn at(&mut self, kind: TokenKind) -> bool;
// }
