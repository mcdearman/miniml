/*
 * Core IR for the analysis phase. This IR is used after type inference for all
 * subsequent analysis and transformations. It's an ANF (A-normal form) representation
 * extended with join points and jumps.
 */

use miniml_ast::SynNode;
use miniml_nir::scoped_ident::ScopedIdent;
use miniml_tir::{TyBoxNode, TyNode};
use miniml_utils::{intern::InternedString, rational::Rational64};

pub type Prog = SynNode<Module>;
pub type Imports = Vec<Path>;
pub type Path = Vec<SynNode<ScopedIdent>>;
pub type Decls = Vec<Decl>;
pub type Decl = SynNode<DeclKind>;
pub type Def = TyNode<DefKind>;
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
    Value(Value),
    Let(Ident, Expr, Expr),
    LetRec(Ident, Expr, Expr),
    Lambda(Pattern, Expr),
    Apply(Expr, Expr),
    Match(Expr, Vec<(Pattern, Expr)>),
    Join(Ident, Expr, Expr),
    Jump(Ident, Vec<Value>),
    Unit,
}

#[derive(Debug, Clone, PartialEq)]
pub enum PatternKind {
    Wildcard,
    Ident(Ident),
    Literal(Value),
    List(Vec<Pattern>),
    Pair(Pattern, Pattern),
    // Record(Option<ScopedIdent>, Vec<(InternedString, Pattern)>),
    Unit,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Int(i64),
    Rational(Rational64),
    Bool(bool),
    String(InternedString),
    Var(Ident),
}
