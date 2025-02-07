use miniml_ast::{SynBoxNode, SynNode};
use miniml_utils::{
    intern::InternedString,
    rational::{Rational, Rational64},
};
use scoped_ident::ScopedIdent;

pub mod res_id;
pub mod scoped_ident;
pub mod scoped_intern;

pub type Prog = SynNode<Module>;
pub type Imports = Vec<Path>;
pub type Path = Vec<SynNode<ScopedIdent>>;
pub type Decls = Vec<Decl>;
pub type Decl = SynNode<DeclKind>;
pub type Defs = Vec<SynNode<DefRec>>;
pub type Expr = SynBoxNode<ExprKind>;
pub type Pattern = SynBoxNode<PatternKind>;
pub type TypeAnno = SynBoxNode<TypeAnnoKind>;
pub type Ident = SynNode<ScopedIdent>;

#[derive(Debug, Clone, PartialEq)]
pub struct Module {
    pub name: Ident,
    pub imports: Imports,
    pub decls: Decls,
}

#[derive(Debug, Clone, PartialEq)]
pub enum DeclKind {
    DefGroup(DefGroup),
}

#[derive(Debug, Clone, PartialEq)]
pub enum DefGroup {
    Rec(Defs),
    NonRec(Pattern, Expr),
}

#[derive(Debug, Clone, PartialEq)]
pub struct DefRec {
    pub name: Ident,
    pub anno: Option<TypeAnno>,
    pub expr: Expr,
}

impl DefRec {
    pub fn new(name: Ident, anno: Option<TypeAnno>, expr: Expr) -> Self {
        Self { name, anno, expr }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind {
    Lit(Lit),
    Var(Ident),
    Apply(Expr, Expr),
    Lambda(Pattern, Expr),
    Or(Expr, Expr),
    And(Expr, Expr),
    Let(Pattern, bool, Expr, Expr),
    If(Expr, Expr, Expr),
    Match(Expr, Vec<(Pattern, Expr)>),
    List(Vec<Expr>),
    Unit,
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
    Fn(TypeAnno, TypeAnno),
    List(TypeAnno),
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
