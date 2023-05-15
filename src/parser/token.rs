use logos::Logos;
use std::{
    fmt::Display,
    ops::{Index, Range},
};

#[derive(Logos, Debug, Clone, PartialEq, Eq)]
pub enum TokenKind {
    Eof,
    #[error]
    Err,
    #[regex("[ \n\t\r]+", logos::skip)]
    Whitespace,
    #[regex(r#"--[^\n]*|/\*([^*]|\**[^*/])*\*+/"#)]
    Comment,
    #[regex(r##"([A-Za-z]|_)([A-Za-z]|_|\d)*"##)]
    Ident,
    #[regex(r#"(0b[0-1]+)|(0o[0-7]+)|(0x[0-9a-fA-F]+)|([1-9]\d*|0)"#, priority = 2)]
    Int,
    #[regex(r#"((\d+(\.\d+))|(\.\d+))([Ee](\+|-)?\d+)?"#, priority = 1)]
    Real,
    #[regex(r#"((\d+(\.\d+)?)|(\.\d+))([Ee](\+|-)?\d+)?i"#, priority = 0)]
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

    #[token("import")]
    Module,
    #[token("use")]
    Use,
    #[token("pub")]
    Pub,
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

#[macro_export]
macro_rules! T {
    [EOF] => {
       $crate::parser::token::TokenKind::Eof
    };
    [err] => {
       $crate::parser::token::TokenKind::Err
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
    [mod] => {
       $crate::parser::token::TokenKind::Module
    };
    [use] => {
       $crate::parser::token::TokenKind::Use
    };
    [pub] => {
       $crate::parser::token::TokenKind::Pub
    };
    [let] => {
       $crate::parser::token::TokenKind::Let
    };
    [fn] => {
       $crate::parser::token::TokenKind::Fn
    };
    [data] => {
       $crate::parser::token::TokenKind::Data
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
                T![err] => "<?>",
                T![ws] => "<WS>",
                T![comment] => "Comment",
                T![ident] => "Ident",
                T![int] => "Int",
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
                T![mod] => "mod",
                T![use] => "use",
                T![pub] => "pub",
                T![let] => "let",
                T![fn] => "fn",
                T![data] => "data",
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Span {
    start: u64,
    end: u64,
}

impl Span {
    pub fn new(start: u64, end: u64) -> Self {
        Self { start, end }
    }
}

impl Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<{}, {}>", self.start, self.end)
    }
}

impl From<Span> for Range<usize> {
    fn from(span: Span) -> Self {
        span.start as usize..span.end as usize
    }
}

impl From<Range<usize>> for Span {
    fn from(range: Range<usize>) -> Self {
        Self {
            start: range.start as u64,
            end: range.end as u64,
        }
    }
}

impl Index<Span> for str {
    type Output = str;

    fn index(&self, index: Span) -> &Self::Output {
        &self[Range::<usize>::from(index)]
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Spanned<T> {
    pub span: Span,
    pub value: T,
}

impl<T> Spanned<T> {
    pub fn new(span: Span, value: T) -> Self {
        Self { span, value }
    }
}

impl<T> Display for Spanned<T>
where
    T: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}", self.span, self.value)
    }
}

pub type Token = Spanned<TokenKind>;

impl<T> From<(T, Span)> for Spanned<T> {
    fn from((value, span): (T, Span)) -> Self {
        Self { span, value }
    }
}
