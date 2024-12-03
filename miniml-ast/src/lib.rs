use crate::token::Token;
use miniml_utils::{
    box_node::BoxNode, intern::InternedString, node::Node, rational::Rational, span::Span,
};

pub mod token;
pub mod token_stream;

pub type SynNode<T> = Node<T, Span>;
pub type SynBoxNode<T> = BoxNode<T, Span>;

pub type Prog = SynNode<Module>;
pub type Imports = Vec<Path>;
pub type Path = Vec<Ident>;
pub type Decls = Vec<Decl>;
pub type Decl = SynNode<DeclKind>;
pub type Expr = SynBoxNode<ExprKind>;
pub type UnaryOp = SynNode<UnaryOpKind>;
pub type BinaryOp = SynNode<BinaryOpKind>;
pub type Pattern = SynBoxNode<PatternKind>;
pub type TypeAnno = SynBoxNode<TypeAnnoKind>;
pub type Ident = SynNode<InternedString>;

#[derive(Debug, Clone, PartialEq)]
pub struct Module {
    pub name: Ident,
    pub imports: Imports,
    pub decls: Decls,
}

#[derive(Debug, Clone, PartialEq)]
pub enum DeclKind {
    Def(Pattern, Expr),
    Fn(Ident, Vec<Pattern>, Expr),
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
            Self::Neg => "__neg__".to_string(),
            Self::Not => "__not__".to_string(),
        }
    }
}

impl From<UnaryOpKind> for InternedString {
    fn from(value: UnaryOpKind) -> Self {
        InternedString::from(value.to_string())
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
            Self::Add => "__add__".to_string(),
            Self::Sub => "__sub__".to_string(),
            Self::Mul => "__mul__".to_string(),
            Self::Div => "__div__".to_string(),
            Self::Rem => "__rem__".to_string(),
            Self::Pow => "__pow__".to_string(),
            Self::Eq => "__eq__".to_string(),
            Self::Neq => "__neq__".to_string(),
            Self::Lt => "__lt__".to_string(),
            Self::Lte => "__lte__".to_string(),
            Self::Gt => "__gt__".to_string(),
            Self::Gte => "__gte__".to_string(),
            Self::Pair => "__pair__".to_string(),
        }
    }
}

impl From<BinaryOpKind> for InternedString {
    fn from(value: BinaryOpKind) -> Self {
        InternedString::from(value.to_string())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum PatternKind {
    Wildcard,
    Lit(Lit),
    Ident(Ident, Option<TypeAnno>),
    List(Vec<Pattern>),
    Pair(Pattern, Pattern),
    Unit,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeAnnoKind {
    Byte,
    Int,
    Rational,
    Real,
    Bool,
    String,
    Char,
    Ident(Ident),
    Fn(Vec<TypeAnno>, TypeAnno),
    List(TypeAnno),
    Unit,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Lit {
    Byte(u8),
    Int(i64),
    Rational(Rational),
    Real(f64),
    Bool(bool),
    String(InternedString),
    Char(char),
}
