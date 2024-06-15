use num_rational::Rational64;

use crate::{
    lex::token::Token,
    utils::{ident::Ident, intern::InternedString, span::Span},
};

#[derive(Debug, Clone, PartialEq)]
pub struct Root {
    pub decls: Vec<Decl>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Decl {
    pub kind: DeclKind,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum DeclKind {
    Def(Pattern, Expr),
    Fn(Ident, Vec<Pattern>, Expr),
    FnMatch(Vec<(Ident, Vec<Pattern>, Expr)>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expr {
    pub kind: Box<ExprKind>,
    pub span: Span,
}

impl Expr {
    pub fn new(kind: ExprKind, span: Span) -> Self {
        Self {
            kind: Box::new(kind),
            span,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind {
    Lit(Lit),
    Var(Ident),
    Apply(Expr, Vec<Expr>),
    Lambda(Vec<Pattern>, Expr),
    UnaryOp(UnaryOp, Expr),
    BinaryOp(BinaryOp, Expr, Expr),
    Or(Expr, Expr),
    And(Expr, Expr),
    Let(Pattern, Expr, Expr),
    Fn(Ident, Vec<Pattern>, Expr, Expr),
    If(Expr, Expr, Expr),
    Match(Expr, Vec<(Pattern, Expr)>),
    List(Vec<Expr>),
    Unit,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct UnaryOp {
    pub kind: UnaryOpKind,
    pub span: Span,
}

impl From<UnaryOp> for InternedString {
    fn from(op: UnaryOp) -> Self {
        InternedString::from(op.kind.to_string())
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum UnaryOpKind {
    Neg,
    Not,
}

impl From<Token> for UnaryOpKind {
    fn from(t: Token) -> Self {
        match t {
            Token::Minus => Self::Neg,
            Token::Not => Self::Not,
            _ => unreachable!(),
        }
    }
}

impl ToString for UnaryOpKind {
    fn to_string(&self) -> String {
        match self {
            Self::Neg => "neg".to_string(),
            Self::Not => "not".to_string(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct BinaryOp {
    pub kind: BinaryOpKind,
    pub span: Span,
}

impl From<BinaryOp> for InternedString {
    fn from(op: BinaryOp) -> Self {
        InternedString::from(op.kind.to_string())
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BinaryOpKind {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Pow,
    Eq,
    Neq,
    Lt,
    Lte,
    Gt,
    Gte,
    Pair,
}

impl From<Token> for BinaryOpKind {
    fn from(t: Token) -> Self {
        match t {
            Token::Plus => Self::Add,
            Token::Minus => Self::Sub,
            Token::Star => Self::Mul,
            Token::Slash => Self::Div,
            Token::Percent => Self::Rem,
            Token::Caret => Self::Pow,
            Token::Eq => Self::Eq,
            Token::Neq => Self::Neq,
            Token::Lt => Self::Lt,
            Token::Leq => Self::Lte,
            Token::Gt => Self::Gt,
            Token::Geq => Self::Gte,
            Token::DoubleColon => Self::Pair,
            _ => unreachable!(),
        }
    }
}

impl ToString for BinaryOpKind {
    fn to_string(&self) -> String {
        match self {
            Self::Add => "add".to_string(),
            Self::Sub => "sub".to_string(),
            Self::Mul => "mul".to_string(),
            Self::Div => "div".to_string(),
            Self::Rem => "rem".to_string(),
            Self::Pow => "pow".to_string(),
            Self::Eq => "eq".to_string(),
            Self::Neq => "neq".to_string(),
            Self::Lt => "lt".to_string(),
            Self::Lte => "lte".to_string(),
            Self::Gt => "gt".to_string(),
            Self::Gte => "gte".to_string(),
            Self::Pair => "pair".to_string(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Pattern {
    pub kind: Box<PatternKind>,
    pub span: Span,
}

impl Pattern {
    pub fn new(kind: PatternKind, span: Span) -> Self {
        Self {
            kind: Box::new(kind),
            span,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum PatternKind {
    Wildcard,
    Lit(Lit),
    Ident(Ident, Option<TypeHint>),
    List(Vec<Pattern>),
    Pair(Pattern, Pattern),
    Unit,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeHint {
    pub kind: Box<TypeHintKind>,
    pub span: Span,
}

impl TypeHint {
    pub fn new(kind: TypeHintKind, span: Span) -> Self {
        Self {
            kind: Box::new(kind),
            span,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeHintKind {
    Byte,
    Int,
    Rational,
    Real,
    Bool,
    String,
    Char,
    Ident(Ident),
    Fn(Vec<TypeHint>, TypeHint),
    List(TypeHint),
    Unit,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Lit {
    Byte(u8),
    Int(i64),
    Rational(Rational64),
    Real(f64),
    Bool(bool),
    String(InternedString),
    Char(char),
}
