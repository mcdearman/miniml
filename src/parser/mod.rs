use crate::intern::InternedString;
use logos::{Lexer, Logos};
use num_complex::Complex64;
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
    #[regex(r#"([1-9]\d*|0)"#, priority = 2)]
    Int,
    #[regex(r#"((\d+(\.\d+))|(\.\d+))([Ee](\+|-)?\d+)?"#, priority = 1)]
    Float,
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
    #[token("type")]
    Type,
    #[token("match")]
    Match,
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
       $crate::parser::TokenKind::Eof
    };
    [err] => {
       $crate::parser::TokenKind::Err
    };
    [ws] => {
       $crate::parser::TokenKind::Whitespace
    };
    [comment] => {
       $crate::parser::TokenKind::Comment
    };
    [ident] => {
       $crate::parser::TokenKind::Ident
    };
    [int] => {
       $crate::parser::TokenKind::Int
    };
    [float] => {
       $crate::parser::TokenKind::Float
    };
    [imag] => {
       $crate::parser::TokenKind::Imag
    };
    [char] => {
       $crate::parser::TokenKind::Char
    };
    [str] => {
       $crate::parser::TokenKind::String
    };
    [bool] => {
       $crate::parser::TokenKind::Bool
    };
    [+] => {
       $crate::parser::TokenKind::Add
    };
    [-] => {
       $crate::parser::TokenKind::Sub
    };
    [*] => {
       $crate::parser::TokenKind::Mul
    };
    [/] => {
       $crate::parser::TokenKind::Div
    };
    [%] => {
       $crate::parser::TokenKind::Rem
    };
    [^] => {
       $crate::parser::TokenKind::Pow
    };
    [&&] => {
       $crate::parser::TokenKind::And
    };
    [||] => {
       $crate::parser::TokenKind::Or
    };
    [lambda] => {
       $crate::parser::TokenKind::Lambda
    };
    [->] => {
       $crate::parser::TokenKind::Arrow
    };
    [=>] => {
       $crate::parser::TokenKind::FatArrow
    };
    [|] => {
       $crate::parser::TokenKind::Pipe
    };
    [|>] => {
       $crate::parser::TokenKind::PipeArrow
    };
    [=] => {
       $crate::parser::TokenKind::Assign
    };
    [==] => {
       $crate::parser::TokenKind::Eql
    };
    [<] => {
       $crate::parser::TokenKind::Lss
    };
    [>] => {
       $crate::parser::TokenKind::Gtr
    };
    [!] => {
       $crate::parser::TokenKind::Not
    };
    [!=] => {
       $crate::parser::TokenKind::Neq
    };
    [<=] => {
       $crate::parser::TokenKind::Leq
    };
    [>=] => {
       $crate::parser::TokenKind::Geq
    };
    ['('] => {
       $crate::parser::TokenKind::LParen
    };
    [')'] => {
       $crate::parser::TokenKind::RParen
    };
    ['['] => {
       $crate::parser::TokenKind::LBrack
    };
    [']'] => {
       $crate::parser::TokenKind::RBrack
    };
    ['{'] => {
       $crate::parser::TokenKind::LBrace
    };
    ['}'] => {
       $crate::parser::TokenKind::RBrace
    };
    [,] => {
       $crate::parser::TokenKind::Comma
    };
    [.] => {
       $crate::parser::TokenKind::Period
    };
    [;] => {
       $crate::parser::TokenKind::Semicolon
    };
    [;;] => {
       $crate::parser::TokenKind::DoubleSemicolon
    };
    [:] => {
       $crate::parser::TokenKind::Colon
    };
    [package] => {
       $crate::parser::TokenKind::Package
    };
    [mod] => {
       $crate::parser::TokenKind::Module
    };
    [use] => {
       $crate::parser::TokenKind::Use
    };
    [pub] => {
       $crate::parser::TokenKind::Pub
    };
    [let] => {
       $crate::parser::TokenKind::Let
    };
    [type] => {
       $crate::parser::TokenKind::Type
    };
    [match] => {
       $crate::parser::TokenKind::Match
    };
    [if] => {
       $crate::parser::TokenKind::If
    };
    [then] => {
       $crate::parser::TokenKind::Then
    };
    [else] => {
       $crate::parser::TokenKind::Else
    };
    [elif] => {
       $crate::parser::TokenKind::Elif
    };
    [in] => {
       $crate::parser::TokenKind::In
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
                T![float] => "Float",
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
                T![type] => "type",
                T![match] => "match",
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

#[derive(Debug, Clone, PartialEq)]
pub enum Item {
    Data(Data),
    Expr(Expr),
}

#[derive(Debug, Clone, PartialEq)]
// pub struct Data {
//     pub name: InternedString,
//     pub variants: Vec<Variant>,
// }
pub enum Data {
    Record {
        name: InternedString,
        fields: Vec<(InternedString, Type)>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct Variant {
    pub name: InternedString,
    pub fields: Vec<InternedString>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Ident(InternedString),
    Lit(Lit),
    List(Vec<Self>),
    Record(Vec<(InternedString, Self)>),
    Sum {
        constructor: InternedString,
        arg: Box<Self>,
    },
    Product(Vec<Self>),
    Prefix {
        op: PrefixOp,
        expr: Box<Self>,
    },
    Infix {
        op: InfixOp,
        lhs: Box<Self>,
        rhs: Box<Self>,
    },
    Let {
        name: InternedString,
        value: Box<Self>,
        body: Option<Box<Self>>,
    },
    Lambda {
        param: InternedString,
        body: Box<Self>,
    },
    Apply(Box<Self>, Box<Self>),
    If {
        cond: Box<Self>,
        then: Box<Self>,
        else_: Box<Self>,
    },
    Unit,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Lit {
    Int(InternedString),
    Float(InternedString),
    Complex(Complex64),
    String(InternedString),
    Char(char),
    Bool(bool),
}

impl Display for Lit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Lit::Int(i) => write!(f, "{}", i),
            Lit::Float(i) => write!(f, "{}", i),
            Lit::Complex(i) => write!(f, "{}", i),
            Lit::String(i) => write!(f, "{}", i),
            Lit::Char(i) => write!(f, "{}", i),
            Lit::Bool(i) => write!(f, "{}", i),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PrefixOp {
    Neg,
    Not,
}

impl From<TokenKind> for PrefixOp {
    fn from(token: TokenKind) -> Self {
        match token {
            T![-] => PrefixOp::Neg,
            T![!] => PrefixOp::Not,
            _ => panic!("Not a prefix operator: {:?}", token),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum InfixOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Pow,
    Eq,
    Neq,
    Lss,
    Gtr,
    Leq,
    Geq,
    And,
    Or,
}

impl From<TokenKind> for InfixOp {
    fn from(token: TokenKind) -> Self {
        match token {
            T![+] => InfixOp::Add,
            T![-] => InfixOp::Sub,
            T![*] => InfixOp::Mul,
            T![/] => InfixOp::Div,
            T![%] => InfixOp::Mod,
            T![^] => InfixOp::Pow,
            T![=] => InfixOp::Eq,
            T![!=] => InfixOp::Neq,
            T![<] => InfixOp::Lss,
            T![>] => InfixOp::Gtr,
            T![<=] => InfixOp::Leq,
            T![>=] => InfixOp::Geq,
            T![&&] => InfixOp::And,
            T![||] => InfixOp::Or,
            _ => panic!("Not an infix operator: {:?}", token),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ParserError(pub String);

impl ParserError {
    pub fn new(msg: &str) -> Self {
        Self(msg.to_string())
    }
}

impl Display for ParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

pub type Result<T> = std::result::Result<T, ParserError>;

pub struct Parser<'src> {
    src: &'src str,
    logos: Lexer<'src, TokenKind>,
    peek: Option<Token>,
}

impl<'src> Parser<'src> {
    pub fn new(src: &'src str) -> Self {
        Self {
            src,
            logos: TokenKind::lexer(src),
            peek: None,
        }
    }

    pub fn tokenize(&mut self) -> Vec<Token> {
        self.logos
            .clone()
            .spanned()
            .map(|(t, s)| Token::from((t, s.into())))
            .collect()
    }

    fn text(&self, token: Token) -> &'src str {
        &self.src[token.span]
    }

    fn next(&mut self) -> Token {
        if let Some(t) = self.peek.take() {
            t
        } else {
            self.generate()
        }
    }

    fn peek(&mut self) -> Token {
        if let Some(t) = self.peek.clone() {
            t
        } else {
            let t = self.generate();
            self.peek = Some(t.clone());
            t
        }
    }

    fn generate(&mut self) -> Token {
        match self.logos.next().map(|t| (t, self.logos.span())) {
            None => Token::from((T![EOF], Span::new(0, 0))),
            Some((T![comment], _)) => self.generate(),
            Some((t, s)) => Token::from((t, s.into())),
        }
    }

    fn at(&mut self, kind: TokenKind) -> bool {
        self.peek().value == kind
    }

    fn consume(&mut self, expected: TokenKind) {
        let token = self.next();
        assert_eq!(
            token.value, expected,
            "Expected to consume `{}`, but found `{:?}`",
            expected, token
        );
    }

    pub fn item(&mut self) -> Result<Item> {
        todo!()
    }

    fn expr(&mut self) -> Result<Expr> {
        todo!()
    }
}
