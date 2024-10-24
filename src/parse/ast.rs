use crate::{
    lex::token::Token,
    utils::{ident::Ident, intern::InternedString, rational::Rational, span::Span},
};
use dbg_pls::DebugPls;

#[derive(Debug, DebugPls, Clone, PartialEq)]
pub struct Prog {
    // pub modules: Vec<Module>,
    pub decls: Vec<Decl>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Module {
    pub name: InternedString,
    pub imports: Vec<Import>,
    pub decls: Vec<Decl>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Import {
    pub path: Vec<Ident>,
    pub span: Span,
}

#[derive(Debug, DebugPls, Clone, PartialEq)]
pub struct Decl {
    pub kind: DeclKind,
    pub span: Span,
}

#[derive(Debug, DebugPls, Clone, PartialEq)]
pub enum DeclKind {
    Def(Pattern, Expr),
    Fn(Ident, Vec<Pattern>, Expr),
    FnMatch(Ident, Vec<(Vec<Pattern>, Expr)>),
}

#[derive(Debug, DebugPls, Clone, PartialEq)]
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

    pub fn is_lambda(&self) -> bool {
        matches!(self.kind.as_ref(), &ExprKind::Lambda(_, _))
    }
}

#[derive(Debug, DebugPls, Clone, PartialEq)]
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

#[derive(Debug, DebugPls, Clone, Copy, PartialEq)]
pub struct UnaryOp {
    pub kind: UnaryOpKind,
    pub span: Span,
}

impl From<UnaryOp> for InternedString {
    fn from(op: UnaryOp) -> Self {
        InternedString::from(op.kind.to_string())
    }
}

#[derive(Debug, DebugPls, Clone, Copy, PartialEq)]
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

#[derive(Debug, DebugPls, Clone, Copy, PartialEq)]
pub struct BinaryOp {
    pub kind: BinaryOpKind,
    pub span: Span,
}

impl From<BinaryOp> for InternedString {
    fn from(op: BinaryOp) -> Self {
        InternedString::from(op.kind.to_string())
    }
}

#[derive(Debug, DebugPls, Clone, Copy, PartialEq)]
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

#[derive(Debug, DebugPls, Clone, PartialEq)]
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

#[derive(Debug, DebugPls, Clone, PartialEq)]
pub enum PatternKind {
    Wildcard,
    Lit(Lit),
    Ident(Ident, Option<TypeHint>),
    List(Vec<Pattern>),
    Pair(Pattern, Pattern),
    Unit,
}

#[derive(Debug, DebugPls, Clone, PartialEq)]
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

#[derive(Debug, DebugPls, Clone, PartialEq)]
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

#[derive(Debug, DebugPls, Clone, PartialEq)]
pub enum Lit {
    Byte(u8),
    Int(i64),
    Rational(Rational),
    Real(f64),
    Bool(bool),
    String(InternedString),
    Char(char),
}
