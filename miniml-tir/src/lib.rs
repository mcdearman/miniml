use std::ops::Deref;

use miniml_ast::{SynBoxNode, SynNode};
use miniml_nir::scoped_ident::ScopedIdent;
use miniml_utils::{
    box_node::BoxNode, intern::InternedString, node::Node, pretty::Pretty, rational::Rational64,
    span::Span,
};
use ty::Ty;

pub mod class;
pub mod kind;
pub mod meta;
pub mod scheme;
pub mod ty;

#[derive(Debug, Clone, PartialEq)]
pub struct Typed<T> {
    pub inner: T,
    pub ty: Ty,
}

impl<T> Typed<T> {
    pub fn new(inner: T, ty: Ty) -> Self {
        Self { inner, ty }
    }
}

impl<T> Deref for Typed<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<T: Pretty> Pretty for Typed<T> {
    fn pretty(&self) -> String {
        format!("{} : {}", self.inner.pretty(), self.ty.pretty())
    }
}

pub type TyNode<T> = Node<Typed<T>, Span>;
pub type TyBoxNode<T> = BoxNode<Typed<T>, Span>;

pub type Prog = SynNode<Module>;
pub type Imports = Vec<Path>;
pub type Path = Vec<SynNode<ScopedIdent>>;
pub type Decls = Vec<Decl>;
pub type Decl = SynNode<DeclKind>;
pub type Defs = Vec<TyNode<DefRec>>;
pub type Expr = TyBoxNode<ExprKind>;
pub type Pattern = TyBoxNode<PatternKind>;
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
    pub ident: Ident,
    pub expr: Expr,
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
    Ident(Ident),
    List(Vec<Pattern>),
    Pair(Pattern, Pattern),
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
