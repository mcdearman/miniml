use super::scoped_ident::ScopedIdent;
use crate::{
    parse::ast::{SynBoxNode, SynNode},
    utils::{intern::InternedString, rational::Rational},
};

pub type Prog = SynNode<Module>;
pub type Imports = Vec<Path>;
pub type Path = Vec<ScopedIdent>;
pub type Decls = Vec<Decl>;
pub type Decl = SynNode<DeclKind>;
pub type Expr = SynBoxNode<ExprKind>;
pub type Pattern = SynBoxNode<PatternKind>;
pub type TypeAnno = SynBoxNode<TypeAnnoKind>;

#[derive(Debug, Clone, PartialEq)]
pub struct Module {
    pub name: InternedString,
    pub imports: Imports,
    pub decls: Decls,
}

#[derive(Debug, Clone, PartialEq)]
pub enum DeclKind {
    Def(DefGroup),
    // TypeAlias(ScopedIdent, TypeHint),
    // Data(ScopedIdent, Vec<(ScopedIdent, Vec<TypeHint>)>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum DefGroup {
    Rec(Vec<DefRec>),
    NonRec(Def),
}

#[derive(Debug, Clone, PartialEq)]
pub struct DefRec {
    pub ident: ScopedIdent,
    pub anno: Option<TypeAnno>,
    pub body: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Def {
    pub pat: Pattern,
    pub body: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind {
    Lit(Lit),
    Var(ScopedIdent),
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
    Ident(ScopedIdent, Option<TypeAnno>),
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
    ScopedIdent(ScopedIdent),
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
