use miniml_ast::{SynBoxNode, SynNode};
use miniml_nir::scoped_ident::ScopedIdent;
use miniml_utils::{
    box_node::BoxNode, intern::InternedString, node::Node, rational::Rational, span::Span,
};
use ty::Ty;

pub mod meta;
pub mod meta_context;
pub mod poly_type;
pub mod ty;

pub type TyNode<T> = Node<T, (Ty, Span)>;
pub type TyBoxNode<T> = BoxNode<T, (Ty, Span)>;

pub type Prog = SynNode<Module>;
pub type Imports = Vec<Path>;
pub type Path = Vec<SynNode<ScopedIdent>>;
pub type Decls = Vec<Decl>;
pub type Decl = SynNode<DeclKind>;
pub type Def = TyNode<DefKind>;
pub type Expr = TyBoxNode<ExprKind>;
pub type Pattern = TyBoxNode<PatternKind>;
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
    Def(Def),
    DefGroup(Vec<Def>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum DefKind {
    Rec { ident: Ident, body: Expr },
    NonRec { pat: Pattern, body: Expr },
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
