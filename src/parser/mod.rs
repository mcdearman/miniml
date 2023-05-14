use crate::{intern::InternedString, list::List};
use logos::{Lexer, Logos};
use num_bigint::BigInt;
use num_complex::Complex64;
use num_rational::Rational64;
use std::{
    collections::HashMap,
    fmt::{Display, Write},
    hash::Hash,
    ops::{Index, Range},
};

mod tests;

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
    #[token("rec")]
    Rec,
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
    [real] => {
       $crate::parser::TokenKind::Real
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
    [rec] => {
       $crate::parser::TokenKind::Rec
    };
    [data] => {
       $crate::parser::TokenKind::Data
    };
    [match] => {
       $crate::parser::TokenKind::Match
    };
    [with] => {
       $crate::parser::TokenKind::With
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
                T![rec] => "rec",
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

#[derive(Debug, Clone, PartialEq)]
pub enum Item {
    Data(Data),
    Decl(Decl),
    Expr(Expr),
}

impl Display for Item {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Item::Data(data) => write!(f, "{}", data),
            Item::Decl(decl) => write!(f, "{}", decl),
            Item::Expr(expr) => write!(f, "{}", expr),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Data {
    name: InternedString,
    fields: Vec<InternedString>,
}

impl Display for Data {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "data {} {}", self.name, self.fields.join(" "))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Decl {
    pub pattern: Pattern,
    pub value: Expr,
}

impl Display for Decl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "let {} = {}", self.pattern, self.value)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Ident(InternedString),
    Lit(Lit),
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
        pattern: Pattern,
        value: Box<Self>,
        body: Box<Self>,
    },
    Apply(Box<Self>, Box<Self>),
    If {
        cond: Box<Self>,
        then: Box<Self>,
        else_: Box<Self>,
    },
    Match {
        expr: Box<Self>,
        arms: Vec<MatchArm>,
    },
    Unit,
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Ident(i) => write!(f, "{}", i),
            Expr::Lit(l) => write!(f, "{}", l),
            Expr::Prefix { op, expr } => write!(f, "({} {})", op, expr),
            Expr::Infix { op, lhs, rhs } => write!(f, "({} {} {})", lhs, op, rhs),
            Expr::Let {
                pattern,
                value,
                body,
            } => write!(f, "let {} = {} in {}", pattern, value, body),
            Expr::Apply(lhs, rhs) => write!(f, "({} {})", lhs, rhs),
            Expr::If { cond, then, else_ } => {
                write!(f, "if {} then {} else {}", cond, then, else_)
            }
            Expr::Match { expr, arms } => {
                write!(f, "(match {} with ", expr)?;
                for arm in arms {
                    write!(f, "{}", arm)?;
                }
                write!(f, ")")
            }
            Expr::Unit => write!(f, "()"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Lit {
    Int(Int),
    Real(Real),
    Complex(Complex64),
    String(InternedString),
    Char(char),
    Bool(bool),
    List(List<Expr>),
    Tuple(Tuple),
    Map(Map),
    Record(Record),
    Lambda(Lambda),
}

impl Display for Lit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Lit::Int(i) => write!(f, "{}", i),
            Lit::Real(r) => write!(f, "{}", r),
            Lit::Complex(c) => write!(f, "{}", c),
            Lit::String(s) => write!(f, "{}", s),
            Lit::Char(i) => write!(f, "{}", i),
            Lit::Bool(i) => write!(f, "{}", i),
            Lit::List(l) => write!(f, "{}", l),
            Lit::Tuple(t) => write!(f, "{:?}", t),
            Lit::Map(m) => write!(f, "{}", m),
            Lit::Record(r) => write!(f, "{}", r),
            Lit::Lambda(l) => write!(f, "{}", l),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Int {
    pub value: BigInt,
    pub radix: Radix,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Radix {
    Bin,
    Oct,
    Dec,
    Hex,
}

impl Display for Int {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.radix {
            Radix::Bin => write!(f, "0b{:b}", self.value),
            Radix::Oct => write!(f, "0o{:o}", self.value),
            Radix::Dec => write!(f, "{}", self.value),
            Radix::Hex => write!(f, "0x{:x}", self.value),
        }
    }
}

impl TryFrom<String> for Int {
    type Error = ParserError;

    fn try_from(s: String) -> std::result::Result<Self, Self::Error> {
        if s.starts_with("0b") {
            Ok(Self {
                value: BigInt::parse_bytes(
                    s.strip_prefix("0b")
                        .expect("expected integer prefix")
                        .as_bytes(),
                    2,
                )
                .ok_or(ParserError::new("Invalid binary integer literal"))?,
                radix: Radix::Bin,
            })
        } else if s.starts_with("0o") {
            Ok(Self {
                value: BigInt::parse_bytes(
                    s.strip_prefix("0o")
                        .expect("expected integer prefix")
                        .as_bytes(),
                    8,
                )
                .ok_or(ParserError::new("Invalid octal integer literal"))?,
                radix: Radix::Oct,
            })
        } else if s.starts_with("0x") {
            Ok(Self {
                value: BigInt::parse_bytes(
                    s.strip_prefix("0x")
                        .expect("expected integer prefix")
                        .as_bytes(),
                    16,
                )
                .ok_or(ParserError::new("Invalid hexadecimal integer literal"))?,
                radix: Radix::Hex,
            })
        } else {
            Ok(Self {
                value: BigInt::parse_bytes(s.as_bytes(), 10)
                    .ok_or(ParserError::new("Invalid decimal integer literal"))?,
                radix: Radix::Dec,
            })
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Real(pub f64);

impl Display for Real {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl From<String> for Real {
    fn from(s: String) -> Self {
        Self(s.parse().expect("Invalid float"))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Tuple {
    items: Vec<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Map {
    pub items: HashMap<InternedString, Expr>,
}

impl Display for Map {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let map = self.clone();
        write!(f, "{{")?;
        while let Some((key, value)) = map.items.clone().into_iter().next() {
            write!(f, "{}: {}", key, value)?;
        }
        write!(f, "}}")
    }
}

impl Display for Tuple {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(")?;
        while let Some(item) = self.items.clone().into_iter().next() {
            write!(f, "{}", item)?;
        }
        write!(f, ")")
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Record {
    name: InternedString,
    fields: HashMap<InternedString, Expr>,
}

impl Display for Record {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let struct_ = self.clone();
        write!(f, "{} {{", struct_.name)?;
        for (key, value) in struct_.fields.clone() {
            write!(f, "{}: {}", key, value)?;
            if struct_.fields.keys().last().unwrap() != &key {
                write!(f, ", ")?;
            }
        }
        write!(f, "}}")
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Lambda {
    pub param: InternedString,
    pub body: Box<Expr>,
}

impl Display for Lambda {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "\\{} -> {}", self.param, self.body)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub expr: Expr,
}

impl Display for MatchArm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} => {}", self.pattern, self.expr)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PrefixOp {
    Neg,
    Not,
}

impl Display for PrefixOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PrefixOp::Neg => write!(f, "-"),
            PrefixOp::Not => write!(f, "!"),
        }
    }
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

impl Display for InfixOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            InfixOp::Add => write!(f, "+"),
            InfixOp::Sub => write!(f, "-"),
            InfixOp::Mul => write!(f, "*"),
            InfixOp::Div => write!(f, "/"),
            InfixOp::Mod => write!(f, "%"),
            InfixOp::Pow => write!(f, "^"),
            InfixOp::Eq => write!(f, "="),
            InfixOp::Neq => write!(f, "!="),
            InfixOp::Lss => write!(f, "<"),
            InfixOp::Gtr => write!(f, ">"),
            InfixOp::Leq => write!(f, "<="),
            InfixOp::Geq => write!(f, ">="),
            InfixOp::And => write!(f, "&&"),
            InfixOp::Or => write!(f, "||"),
        }
    }
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

#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    Ident(InternedString),
    Int(Int),
    BigInt(BigInt),
    Real(Real),
    Complex(Complex64),
    Rational(Rational64),
    Bool(bool),
    Str(InternedString),
    Char(char),
    List(ListPattern),
    Tuple(TuplePattern),
    Map(MapPattern),
    Record(RecordPattern),
    Wildcard,
    Unit,
}

impl Display for Pattern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Pattern::Ident(i) => write!(f, "{}", i),
            Pattern::Int(i) => write!(f, "{}", i),
            Pattern::BigInt(i) => write!(f, "{}", i),
            Pattern::Real(r) => write!(f, "{}", r),
            Pattern::Complex(c) => write!(f, "{}", c),
            Pattern::Rational(r) => write!(f, "{}", r),
            Pattern::Bool(b) => write!(f, "{}", b),
            Pattern::Str(s) => write!(f, "{}", s),
            Pattern::Char(c) => write!(f, "{}", c),
            Pattern::List(l) => write!(f, "{}", l),
            Pattern::Tuple(t) => write!(f, "{}", t),
            Pattern::Map(m) => write!(f, "{}", m),
            Pattern::Record(r) => write!(f, "{}", r),
            Pattern::Wildcard => write!(f, "_"),
            Pattern::Unit => write!(f, "()"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ListPattern {
    pub items: Vec<Pattern>,
}

impl Display for ListPattern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[")?;
        for (i, item) in self.items.iter().enumerate() {
            if i != 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", item)?;
        }
        write!(f, "]")
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TuplePattern {
    pub items: Vec<Pattern>,
}

impl Display for TuplePattern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(")?;
        for (i, item) in self.items.iter().enumerate() {
            if i != 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", item)?;
        }
        write!(f, ")")
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct MapPattern {
    pub items: Vec<(Pattern, Pattern)>,
}

impl Display for MapPattern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{")?;
        for (i, (key, value)) in self.items.iter().enumerate() {
            if i != 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}: {}", key, value)?;
        }
        write!(f, "}}")
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct RecordPattern {
    pub items: Vec<(InternedString, Pattern)>,
}

impl Display for RecordPattern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{")?;
        for (i, (key, value)) in self.items.iter().enumerate() {
            if i != 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}: {}", key, value)?;
        }
        write!(f, "}}")
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
        match self.peek().value {
            T![data] => Ok(Item::Data(self.data()?)),
            _ => Ok(Item::Expr(self.expr()?)),
        }
    }

    fn data(&mut self) -> Result<Data> {
        self.consume(T![data]);
        let name = self.ident()?;
        let mut fields = vec![];
        self.consume(T!['{']);
        while !self.at(T!['}']) {
            fields.push(self.ident()?);
            if !self.at(T!['}']) {
                self.consume(T![,]);
            }
        }
        Ok(Data {
            name,
            fields: fields,
        })
    }

    fn expr(&mut self) -> Result<Expr> {
        self.or()
    }

    fn or(&mut self) -> Result<Expr> {
        let lhs = self.and()?;
        match self.peek().value {
            T![||] => {
                self.consume(T![||]);
                let rhs = self.and()?;
                Ok(Expr::Infix {
                    op: InfixOp::Or,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                })
            }
            _ => Ok(lhs),
        }
    }

    fn and(&mut self) -> Result<Expr> {
        let lhs = self.eq()?;
        match self.peek().value {
            T![&&] => {
                self.consume(T![&&]);
                let rhs = self.eq()?;
                Ok(Expr::Infix {
                    op: InfixOp::And,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                })
            }
            _ => Ok(lhs),
        }
    }

    fn eq(&mut self) -> Result<Expr> {
        let lhs = self.cmp()?;
        match self.peek().value {
            T![=] => {
                self.consume(T![=]);
                let rhs = self.cmp()?;
                Ok(Expr::Infix {
                    lhs: Box::new(lhs),
                    op: InfixOp::Eq,
                    rhs: Box::new(rhs),
                })
            }
            T![!=] => {
                self.consume(T![!=]);
                let rhs = self.cmp()?;
                Ok(Expr::Infix {
                    lhs: Box::new(lhs),
                    op: InfixOp::Neq,
                    rhs: Box::new(rhs),
                })
            }
            _ => Ok(lhs),
        }
    }

    fn cmp(&mut self) -> Result<Expr> {
        let lhs = self.term()?;
        match self.peek().value {
            T![<] => {
                self.consume(T![<]);
                let rhs = self.term()?;
                Ok(Expr::Infix {
                    lhs: Box::new(lhs),
                    op: InfixOp::Lss,
                    rhs: Box::new(rhs),
                })
            }
            T![>] => {
                self.consume(T![>]);
                let rhs = self.term()?;
                Ok(Expr::Infix {
                    lhs: Box::new(lhs),
                    op: InfixOp::Gtr,
                    rhs: Box::new(rhs),
                })
            }
            T![<=] => {
                self.consume(T![<=]);
                let rhs = self.term()?;
                Ok(Expr::Infix {
                    lhs: Box::new(lhs),
                    op: InfixOp::Leq,
                    rhs: Box::new(rhs),
                })
            }
            T![>=] => {
                self.consume(T![>=]);
                let rhs = self.term()?;
                Ok(Expr::Infix {
                    lhs: Box::new(lhs),
                    op: InfixOp::Geq,
                    rhs: Box::new(rhs),
                })
            }
            _ => Ok(lhs),
        }
    }

    fn term(&mut self) -> Result<Expr> {
        let mut lhs = self.factor()?;
        loop {
            match self.peek().value {
                T![+] => {
                    self.consume(T![+]);
                    let rhs = self.factor()?;
                    lhs = Expr::Infix {
                        lhs: Box::new(lhs),
                        op: InfixOp::Add,
                        rhs: Box::new(rhs),
                    };
                }
                T![-] => {
                    self.consume(T![-]);
                    let rhs = self.factor()?;
                    lhs = Expr::Infix {
                        lhs: Box::new(lhs),
                        op: InfixOp::Sub,
                        rhs: Box::new(rhs),
                    };
                }
                _ => break,
            }
        }
        Ok(lhs)
    }

    fn factor(&mut self) -> Result<Expr> {
        let mut lhs = self.power()?;
        loop {
            match self.peek().value {
                T![*] => {
                    self.consume(T![*]);
                    let rhs = self.power()?;
                    lhs = Expr::Infix {
                        lhs: Box::new(lhs),
                        op: InfixOp::Mul,
                        rhs: Box::new(rhs),
                    };
                }
                T![/] => {
                    self.consume(T![/]);
                    let rhs = self.power()?;
                    lhs = Expr::Infix {
                        lhs: Box::new(lhs),
                        op: InfixOp::Div,
                        rhs: Box::new(rhs),
                    };
                }
                T![%] => {
                    self.consume(T![%]);
                    let rhs = self.power()?;
                    lhs = Expr::Infix {
                        lhs: Box::new(lhs),
                        op: InfixOp::Mod,
                        rhs: Box::new(rhs),
                    };
                }
                _ => break,
            }
        }
        Ok(lhs)
    }

    fn power(&mut self) -> Result<Expr> {
        let mut lhs = self.unary()?;
        while self.at(T![^]) {
            self.consume(T![^]);
            let rhs = self.unary()?;
            lhs = Expr::Infix {
                lhs: Box::new(lhs),
                op: InfixOp::Pow,
                rhs: Box::new(rhs),
            };
        }
        Ok(lhs)
    }

    fn unary(&mut self) -> Result<Expr> {
        match self.peek().value {
            T![!] => {
                self.consume(T![!]);
                let rhs = self.unary()?;
                Ok(Expr::Prefix {
                    op: PrefixOp::Not,
                    expr: Box::new(rhs),
                })
            }
            T![-] => {
                self.consume(T![-]);
                let rhs = self.unary()?;
                Ok(Expr::Prefix {
                    op: PrefixOp::Neg,
                    expr: Box::new(rhs),
                })
            }
            _ => self.apply(),
        }
    }

    fn apply(&mut self) -> Result<Expr> {
        let lhs = self.atom()?;
        let mut args = vec![];
        loop {
            match self.peek().value {
                T![ident]
                | T![int]
                | T![real]
                | T![str]
                | T![char]
                | T![bool]
                | T![lambda]
                | T![if]
                | T![let]
                | T!['(']
                | T!['[']
                | T!['{'] => {
                    let arg = self.atom()?;
                    args.push(arg);
                }
                _ => break,
            }
        }
        Ok(self.curry_apply(args, lhs))
    }

    fn atom(&mut self) -> Result<Expr> {
        let tok = self.peek();
        match tok.value {
            T![if] => self.if_(),
            T![let] => self.let_(),
            T![int]
            | T![real]
            | T![str]
            | T![char]
            | T![bool]
            | T![lambda]
            | T!['[']
            | T!['(']
            | T!['{'] => Ok(Expr::Lit(self.lit()?)),
            T![ident] => Ok(Expr::Ident(self.ident()?)),
            T!['('] => {
                self.consume(T!['(']);
                let expr = self.expr()?;
                self.consume(T![')']);
                Ok(expr)
            }
            _ => Err(ParserError(format!(
                "Unexpected token in atom got `{:?}` - `{}`",
                self.peek(),
                self.text(tok)
            ))),
        }
    }

    fn let_(&mut self) -> Result<Expr> {
        self.consume(T![let]);
        let pattern = self.pattern()?;
        let mut params = vec![];
        while !self.at(T![=]) {
            let tok = self.peek();
            match tok.value {
                T![ident] => params.push(self.ident()?),
                T!['('] => {
                    self.consume(T!['(']);
                    let p = self.ident()?;
                    self.consume(T![')']);
                    params.push(p);
                }
                _ => {
                    return Err(ParserError(format!(
                        "Unexpected token in let got `{:?}` - `{}`",
                        self.peek(),
                        self.text(tok)
                    )))
                }
            }
        }

        self.consume(T![=]);

        if params.is_empty() {
            self.let_bind(pattern)
        } else {
            self.let_fn(pattern, params)
        }
    }

    fn let_bind(&mut self, pattern: Pattern) -> Result<Expr> {
        let val = self.expr()?;
        self.consume(T![in]);
        let body = self.expr()?;
        Ok(Expr::Let {
            pattern,
            value: Box::new(val),
            body: Box::new(body),
        })
    }

    fn let_fn(&mut self, pattern: Pattern, params: Vec<InternedString>) -> Result<Expr> {
        let val = self.expr()?;
        self.consume(T![in]);
        let body = self.expr()?;
        Ok(Expr::Let {
            pattern,
            value: Box::new(Expr::Lit(Lit::Lambda(self.curry_fn(params, val)?))),
            body: Box::new(body),
        })
    }

    fn if_(&mut self) -> Result<Expr> {
        self.consume(T![if]);
        let cond = self.expr()?;
        self.consume(T![then]);
        let then = self.expr()?;
        let mut elifs = vec![];
        while self.at(T![elif]) {
            self.consume(T![elif]);
            let cond = self.expr()?;
            self.consume(T![then]);
            let then = self.expr()?;
            elifs.push((cond, then));
        }
        self.consume(T![else]);
        let else_ = self.expr()?;

        let top_else = elifs
            .into_iter()
            .fold(else_, |acc, (elif_cond, elif_then)| Expr::If {
                cond: Box::new(elif_cond),
                then: Box::new(elif_then),
                else_: Box::new(acc),
            });
        Ok(Expr::If {
            cond: Box::new(cond),
            then: Box::new(then),
            else_: Box::new(top_else),
        })
    }

    fn match_(&mut self) -> Result<Expr> {
        self.consume(T![match]);
        let expr = self.expr()?;
        self.consume(T![with]);
        let mut arms = vec![];
        while !self.at(T![|]) {
            let pattern = self.pattern()?;
            self.consume(T![->]);
            let expr = self.expr()?;
            arms.push(MatchArm { pattern, expr });
        }
        Ok(Expr::Match {
            expr: Box::new(expr),
            arms,
        })
    }

    fn pattern(&mut self) -> Result<Pattern> {
        let tok = self.peek();
        match tok.value {
            T![ident] => Ok(Pattern::Ident(self.ident()?)),
            T![int] => todo!(),
            T![real] => todo!(),
            T![str] => todo!(),
            T![char] => todo!(),
            T![bool] => todo!(),
            T!['['] => self.list_pattern(),
            T!['('] => self.tuple_pattern(),
            T!['{'] => self.map_pattern(),
            _ => Err(ParserError(format!(
                "Unexpected token in pattern got `{:?}` - `{}`",
                self.peek(),
                self.text(tok)
            ))),
        }
    }

    fn int_pattern(&mut self) -> Result<Pattern> {
        Ok(Pattern::Int(self.int()?))
    }

    fn list_pattern(&mut self) -> Result<Pattern> {
        self.consume(T!['[']);
        let mut items = vec![];
        while !self.at(T![']']) {
            items.push(self.pattern()?);
            if !self.at(T![']']) {
                self.consume(T![,]);
            }
        }
        self.consume(T![']']);
        Ok(Pattern::List(ListPattern { items }))
    }

    fn tuple_pattern(&mut self) -> Result<Pattern> {
        self.consume(T!['(']);
        if self.at(T![')']) {
            self.consume(T![')']);
            return Ok(Pattern::Unit);
        }
        let mut items = vec![];
        while !self.at(T![')']) {
            items.push(self.pattern()?);
            if !self.at(T![')']) {
                self.consume(T![,]);
            }
        }
        self.consume(T![')']);
        Ok(Pattern::Tuple(TuplePattern { items }))
    }

    fn map_pattern(&mut self) -> Result<Pattern> {
        self.consume(T!['{']);
        let mut items = vec![];
        while !self.at(T!['}']) {
            let key = self.pattern()?;
            self.consume(T![:]);
            let value = self.pattern()?;
            items.push((key, value));
            if !self.at(T!['}']) {
                self.consume(T![,]);
            }
        }
        self.consume(T!['}']);
        Ok(Pattern::Map(MapPattern { items }))
    }

    fn lit(&mut self) -> Result<Lit> {
        match self.peek().value {
            T![int] => Ok(Lit::Int(self.int()?)),
            T![real] => Ok(Lit::Real(self.real()?)),
            T![str] => Ok(Lit::String(self.string()?)),
            T![char] => Ok(Lit::Char(self.char()?)),
            T![bool] => Ok(Lit::Bool(self.bool()?)),
            T![lambda] => Ok(Lit::Lambda(self.lambda()?)),
            T!['['] => Ok(Lit::List(self.list()?)),
            T!['('] => Ok(Lit::Tuple(self.tuple()?)),
            T!['{'] => Ok(Lit::Map(self.map()?)),
            _ => Err(ParserError(format!("Unexpected token: {:?}", self.peek()))),
        }
    }

    fn int(&mut self) -> Result<Int> {
        let token = self.next();
        let text = self.text(token);
        Ok(Int::from(text.to_string().try_into()?))
    }

    fn real(&mut self) -> Result<Real> {
        let token = self.next();
        let text = self.text(token);
        Ok(Real::from(text.to_string()))
    }

    fn string(&mut self) -> Result<InternedString> {
        let token = self.next();
        let text = self.text(token);
        Ok(InternedString::from(&text[1..(text.len() - 1)]))
    }

    fn char(&mut self) -> Result<char> {
        let token = self.next();
        let text = self.text(token);
        Ok(text
            .chars()
            .nth(1)
            .ok_or(ParserError::new("Invalid character literal"))?)
    }

    fn bool(&mut self) -> Result<bool> {
        let token = self.next();
        let text = self.text(token);
        text.parse()
            .map_err(|_| ParserError(format!("Invalid boolean literal: {}", text)))
    }

    fn lambda(&mut self) -> Result<Lambda> {
        self.consume(T![lambda]);
        let mut params = vec![];
        while !self.at(T![->]) {
            params.push(self.ident()?);
        }

        self.consume(T![->]);

        let body = self.expr()?;

        self.curry_fn(params, body)
    }

    fn list(&mut self) -> Result<List<Expr>> {
        self.consume(T!['[']);
        let mut items = vec![];
        while !self.at(T![']']) {
            items.push(self.expr()?);
            if !self.at(T![']']) {
                self.consume(T![,]);
            }
        }
        self.consume(T![']']);
        Ok(items.into())
    }

    fn tuple(&mut self) -> Result<Tuple> {
        self.consume(T!['(']);
        let mut items = vec![];
        while !self.at(T![')']) {
            items.push(self.expr()?);
            if !self.at(T![')']) {
                self.consume(T![,]);
            }
        }
        self.consume(T![')']);
        Ok(Tuple { items })
    }

    fn map(&mut self) -> Result<Map> {
        self.consume(T!['{']);
        let mut items = HashMap::new();
        while !self.at(T!['}']) {
            let key = self.ident()?;
            self.consume(T![:]);
            let value = self.expr()?;
            items.insert(key, value);
            if !self.at(T!['}']) {
                self.consume(T![,]);
            }
        }
        self.consume(T!['}']);
        Ok(Map { items })
    }

    fn ident(&mut self) -> Result<InternedString> {
        let token = self.next();
        let text = self.text(token);
        Ok(InternedString::from(text))
    }

    fn curry_fn(&mut self, params: Vec<InternedString>, body: Expr) -> Result<Lambda> {
        let last = params.first().ok_or(ParserError(
            "Cannot create a lambda with no parameters".to_string(),
        ))?;
        let iter = params.clone().into_iter().rev().filter(|p| p != last);
        let b = iter.fold(body, |acc, p| {
            Expr::Lit(Lit::Lambda(Lambda {
                param: p,
                body: Box::new(acc),
            }))
        });
        Ok(Lambda {
            param: last.clone(),
            body: Box::new(b),
        })
    }

    fn curry_apply(&mut self, args: Vec<Expr>, func: Expr) -> Expr {
        args.into_iter()
            .fold(func, |acc, arg| Expr::Apply(Box::new(acc), Box::new(arg)))
    }
}
