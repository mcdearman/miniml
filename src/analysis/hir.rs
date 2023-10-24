/*
 * High-level Intermediate Representation (HIR)
 */

use super::{hir, res};
use crate::util::{intern::InternedString, node::SrcNode, span::Span, unique_id::UniqueId};
use num_complex::Complex64;
use num_rational::Rational64;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct HirError {
    pub msg: InternedString,
    pub span: Span,
}

pub type HirResult<T> = Result<T, HirError>;

#[derive(Debug, Clone, PartialEq)]
pub struct Root {
    pub decls: Vec<SrcNode<Decl>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Decl {
    Let {
        name: SrcNode<UniqueId>,
        expr: SrcNode<Expr>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Ident(UniqueId),
    Lit(Lit),
    Lambda {
        params: Vec<SrcNode<Pattern>>,
        body: Box<SrcNode<Self>>,
    },
    Match {
        expr: Box<SrcNode<Self>>,
        cases: Vec<SrcNode<MatchCase>>,
    },
    Apply {
        fun: Box<SrcNode<Self>>,
        arg: Box<SrcNode<Self>>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct MatchCase {
    pub pattern: SrcNode<Pattern>,
    pub body: SrcNode<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    Ident(UniqueId),
    Lit(Lit),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Lit {
    Nat(u64),
    Int(i64),
    Rational(Rational64),
    Real(f64),
    Complex(Complex64),
    String(InternedString),
}

pub fn lower(root: &SrcNode<res::Root>) -> HirResult<SrcNode<hir::Root>> {
    let mut decls = Vec::new();
    let mut errors = vec![];
    for decl in &root.decls {
        match lower_decl(decl) {
            Ok(decl) => decls.push(decl),
            Err(err) => errors.push(err),
        }
    }
    Ok(SrcNode::new(hir::Root { decls }, root.span()))
}

fn lower_decl(decl: &SrcNode<res::Decl>) -> HirResult<SrcNode<hir::Decl>> {
    // match decl {
    //     res::Decl::Let { name, expr } => hir::Decl::Let {
    //         name: name.clone(),
    //         expr: lower_expr(expr),
    //     },
    //     res::Decl::Fn { name, params, expr } => todo!(),
    // }
    todo!()
}

fn lower_expr(expr: &SrcNode<res::Expr>) -> HirResult<SrcNode<hir::Expr>> {
    todo!()
}
