use crate::token::Token;
use miniml_utils::{
    box_node::BoxNode, intern::InternedString, node::Node, rational::Rational64, span::Span,
};

pub mod token;
pub mod token_stream;

pub type SynNode<T> = Node<T, Span>;
pub type SynBoxNode<T> = BoxNode<T, Span>;

pub type Prog = SynNode<Module>;
pub type Imports = Vec<Path>;
pub type ClassDef = SynNode<ClassDefInner>;
pub type InstDef = SynNode<InstDefInner>;
pub type StructDef = SynNode<StructDefInner>;
pub type DataDef = SynNode<DataDefInner>;
pub type TypeAlias = SynNode<TypeAliasInner>;
pub type Def = SynNode<DefInner>;
pub type FnDef = SynNode<FnDefInner>;
pub type Defs = Vec<Def>;
pub type Path = Vec<Ident>;
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
    pub classes: Vec<ClassDef>,
    pub insts: Vec<InstDef>,
    pub structs: Vec<StructDef>,
    pub data_defs: Vec<DataDef>,
    pub type_aliases: Vec<TypeAlias>,
    pub defs: Defs,
    pub fn_defs: Vec<FnDef>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ClassDefInner {
    pub name: Ident,
    pub type_params: Vec<Ident>,
    pub fields: Vec<(Ident, TypeAnno)>,
    pub visibility: Visibility,
}

#[derive(Debug, Clone, PartialEq)]
pub struct InstDefInner {
    pub class_name: Ident,
    pub type_name: Ident,
    pub type_params: Vec<TypeAnno>,
    pub defs: Defs,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructDefInner {
    pub name: Ident,
    pub type_params: Vec<Ident>,
    pub fields: Vec<(Ident, TypeAnno, Visibility)>,
    pub visibility: Visibility,
}

#[derive(Debug, Clone, PartialEq)]
pub struct DataDefInner {
    pub name: Pattern,
    pub type_params: Vec<Ident>,
    pub variants: Vec<(Ident, Vec<TypeAnno>)>,
    pub visibility: Visibility,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeAliasInner {
    pub name: Ident,
    pub type_params: Vec<Ident>,
    pub ty_anno: TypeAnno,
    pub visibility: Visibility,
}

#[derive(Debug, Clone, PartialEq)]
pub struct DefInner {
    pub name: Ident,
    pub ty_anno: Option<TypeAnno>,
    pub expr: Expr,
    pub visibility: Visibility,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FnDefInner {
    pub name: Ident,
    pub ty_anno: TypeAnno,
    pub params: Vec<Pattern>,
    pub body: Expr,
    pub visibility: Visibility,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Visibility {
    Public,
    Private,
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

#[derive(Debug, Copy, Clone, PartialEq)]
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

#[derive(Debug, Copy, Clone, PartialEq)]
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
    Con(Ident, Vec<Pattern>),
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
    Fn(TypeAnno, TypeAnno),
    List(TypeAnno),
    Unit,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Lit {
    Byte(u8),
    Int(i64),
    Rational(Rational64),
    Real(f64),
    Bool(bool),
    String(InternedString),
    Char(char),
}
