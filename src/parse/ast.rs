use crate::{
    lex::token::Token,
    utils::{intern::InternedString, list::List, span::Span},
};
use num_bigint::BigInt;
use num_complex::Complex64;
use num_rational::{BigRational, Rational64};

#[derive(Debug, Clone, PartialEq)]
pub struct Root {
    decls: Vec<Decl>,
    span: Span,
}

impl Root {
    pub fn new(decls: Vec<Decl>, span: Span) -> Self {
        Self { decls, span }
    }

    pub fn decls(&self) -> &[Decl] {
        &self.decls
    }

    pub fn span(&self) -> &Span {
        &self.span
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Module {
    decls: Vec<Decl>,
    span: Span,
}

impl Module {
    pub fn new(decls: Vec<Decl>, span: Span) -> Self {
        Self { decls, span }
    }

    pub fn decls(&self) -> &[Decl] {
        &self.decls
    }

    pub fn span(&self) -> &Span {
        &self.span
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Decl {
    kind: DeclKind,
    span: Span,
}

impl Decl {
    pub fn new(kind: DeclKind, span: Span) -> Self {
        Self { kind, span }
    }

    pub fn kind(&self) -> &DeclKind {
        &self.kind
    }

    pub fn span(&self) -> &Span {
        &self.span
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum DeclKind {
    DataType(DataType),
    Let { pattern: Pattern, expr: Expr },
}

#[derive(Debug, Clone, PartialEq)]
pub struct DataType {
    name: Ident,
    kind: Box<DataTypeKind>,
    span: Span,
}

impl DataType {
    pub fn new(name: Ident, kind: DataTypeKind, span: Span) -> Self {
        Self {
            name,
            kind: Box::new(kind),
            span,
        }
    }

    pub fn name(&self) -> &Ident {
        &self.name
    }

    pub fn kind(&self) -> &DataTypeKind {
        &self.kind
    }

    pub fn span(&self) -> &Span {
        &self.span
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum DataTypeKind {
    Record {
        fields: Vec<(Ident, TypeHint)>,
    },
    Sum {
        variants: Vec<(Ident, Option<TypeHint>)>,
    },
    Product {
        fields: Vec<TypeHint>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expr {
    kind: Box<ExprKind>,
    span: Span,
}

impl Expr {
    pub fn new(kind: ExprKind, span: Span) -> Self {
        Self {
            kind: Box::new(kind),
            span,
        }
    }

    pub fn kind(&self) -> &ExprKind {
        &self.kind
    }

    pub fn span(&self) -> &Span {
        &self.span
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind {
    Lit(Lit),
    Ident(Ident),
    Apply {
        fun: Expr,
        args: Vec<Expr>,
    },
    Unary {
        op: UnaryOp,
        expr: Expr,
    },
    Binary {
        op: BinaryOp,
        lhs: Expr,
        rhs: Expr,
    },
    If {
        cond: Expr,
        then: Expr,
        else_: Expr,
    },
    Let {
        pattern: Pattern,
        expr: Expr,
        body: Expr,
    },
    Lambda {
        params: Vec<Pattern>,
        expr: Expr,
    },
    List(Vec<Expr>),
    Array(Vec<Expr>),
    Tuple(Vec<Expr>),
    Range {
        start: Expr,
        end: Expr,
        inclusive: bool,
        step: Option<Expr>,
    },
    Record {
        fields: Vec<(Ident, Expr)>,
    },
    FieldAccess {
        record: Expr,
        field: Ident,
    },
    Unit,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeHint {
    kind: Box<TypeHintKind>,
    span: Span,
}

impl TypeHint {
    pub fn new(kind: TypeHintKind, span: Span) -> Self {
        Self {
            kind: Box::new(kind),
            span,
        }
    }

    pub fn kind(&self) -> &TypeHintKind {
        &self.kind
    }

    pub fn span(&self) -> &Span {
        &self.span
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeHintKind {
    Int,
    Real,
    Rational,
    Bool,
    String,
    Char,
    Ident(Ident),
    List(TypeHint),
    Vec(Vec<TypeHint>),
    Tuple(Vec<TypeHint>),
    Fn(Vec<TypeHint>, TypeHint),
    Unit,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Pattern {
    kind: PatternKind,
    span: Span,
}

impl Pattern {
    pub fn new(kind: PatternKind, span: Span) -> Self {
        Self { kind, span }
    }

    pub fn kind(&self) -> &PatternKind {
        &self.kind
    }

    pub fn span(&self) -> &Span {
        &self.span
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct UnaryOp {
    kind: UnaryOpKind,
    span: Span,
}

impl UnaryOp {
    pub fn new(kind: UnaryOpKind, span: Span) -> Self {
        Self { kind, span }
    }

    pub fn kind(&self) -> &UnaryOpKind {
        &self.kind
    }

    pub fn span(&self) -> &Span {
        &self.span
    }
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
            Token::Bang => Self::Not,
            _ => unreachable!(),
        }
    }
}

impl ToString for UnaryOpKind {
    fn to_string(&self) -> String {
        match self {
            Self::Neg => "-".to_string(),
            Self::Not => "!".to_string(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct BinaryOp {
    kind: BinaryOpKind,
    span: Span,
}

impl BinaryOp {
    pub fn new(kind: BinaryOpKind, span: Span) -> Self {
        Self { kind, span }
    }

    pub fn kind(&self) -> &BinaryOpKind {
        &self.kind
    }

    pub fn span(&self) -> &Span {
        &self.span
    }
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
    And,
    Or,
}

impl From<Token> for BinaryOpKind {
    fn from(t: Token) -> Self {
        match t {
            Token::Assign => Self::Eq,
            Token::Plus => Self::Add,
            Token::Minus => Self::Sub,
            Token::Star => Self::Mul,
            Token::Slash => Self::Div,
            Token::Percent => Self::Rem,
            Token::Caret => Self::Pow,
            Token::Neq => Self::Neq,
            Token::Lt => Self::Lt,
            Token::Leq => Self::Lte,
            Token::Gt => Self::Gt,
            Token::Geq => Self::Gte,
            Token::And => Self::And,
            Token::Or => Self::Or,
            _ => unreachable!(),
        }
    }
}

impl ToString for BinaryOpKind {
    fn to_string(&self) -> String {
        match self {
            Self::Add => "+".to_string(),
            Self::Sub => "-".to_string(),
            Self::Mul => "*".to_string(),
            Self::Div => "/".to_string(),
            Self::Rem => "%".to_string(),
            Self::Pow => "^".to_string(),
            Self::Eq => "==".to_string(),
            Self::Neq => "!=".to_string(),
            Self::Lt => "<".to_string(),
            Self::Lte => "<=".to_string(),
            Self::Gt => ">".to_string(),
            Self::Gte => ">=".to_string(),
            Self::And => "&&".to_string(),
            Self::Or => "||".to_string(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum PatternKind {
    Wildcard,
    Ident(Ident),
    Num(Rational64),
    Bool(bool),
    String(InternedString),
    Tuple(Vec<Pattern>),
    List(Vec<Pattern>),
    Cons(Vec<Pattern>, Box<Pattern>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Ident {
    name: InternedString,
    span: Span,
}

impl Ident {
    pub fn new(name: InternedString, span: Span) -> Self {
        Self { name, span }
    }

    pub fn name(&self) -> &InternedString {
        &self.name
    }

    pub fn span(&self) -> &Span {
        &self.span
    }
}

impl ToString for Ident {
    fn to_string(&self) -> String {
        self.name.clone().to_string()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Lit {
    Int(i64),
    Real(f64),
    Rational(Rational64),
    Bool(bool),
    String(InternedString),
    Char(char),
}
