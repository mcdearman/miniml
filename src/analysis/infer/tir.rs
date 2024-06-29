use super::{meta_context::MetaContext, r#type::Type};
use crate::{
    rename::scoped_ident::ScopedIdent,
    utils::{intern::InternedString, span::Span},
};
use itertools::{join, Itertools};
use num_rational::Rational64;
use std::fmt::{Debug, Display};

#[derive(Debug, Clone, PartialEq)]
pub struct Root {
    pub decls: Vec<Decl>,
    pub span: Span,
}

impl Root {
    pub fn zonk(&self, meta_ctx: &mut MetaContext) -> Root {
        let decls = self
            .decls
            .iter()
            .map(|decl| decl.zonk(meta_ctx))
            .collect_vec();
        Root {
            decls,
            span: self.span,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Decl {
    pub kind: DeclKind,
    pub ty: Type,
    pub span: Span,
}

impl Decl {
    fn zonk(&self, meta_ctx: &mut MetaContext) -> Decl {
        match &self.kind {
            DeclKind::Def(pat, let_expr) => {
                let zonked_pat = pat.zonk(meta_ctx);
                let zonked_expr = let_expr.zonk(meta_ctx);
                Decl {
                    kind: DeclKind::Def(zonked_pat, zonked_expr),
                    ty: self.ty.zonk(meta_ctx),
                    span: self.span,
                }
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum DeclKind {
    Def(Pattern, Expr),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expr {
    pub kind: Box<ExprKind>,
    pub ty: Type,
    pub span: Span,
}

impl Expr {
    pub fn new(kind: ExprKind, ty: Type, span: Span) -> Self {
        Self {
            kind: Box::new(kind),
            ty,
            span,
        }
    }

    fn zonk(&self, meta_ctx: &mut MetaContext) -> Expr {
        match self.kind.as_ref() {
            ExprKind::Lit(lit) => Expr::new(
                ExprKind::Lit(lit.clone()),
                self.ty.zonk(meta_ctx),
                self.span,
            ),
            ExprKind::Var(name) => {
                Expr::new(ExprKind::Var(*name), self.ty.zonk(meta_ctx), self.span)
            }
            ExprKind::Lambda(param, fn_expr) => {
                let zonked_param = param.zonk(meta_ctx);
                let zonked_expr = fn_expr.zonk(meta_ctx);
                Expr::new(
                    ExprKind::Lambda(zonked_param, zonked_expr),
                    self.ty.zonk(meta_ctx),
                    self.span,
                )
            }
            ExprKind::Apply(fun, arg) => {
                let zonked_fun = fun.zonk(meta_ctx);
                let zonked_arg = arg.zonk(meta_ctx);
                Expr::new(
                    ExprKind::Apply(zonked_fun, zonked_arg),
                    self.ty.zonk(meta_ctx),
                    self.span,
                )
            }
            ExprKind::Or(lhs, rhs) => {
                let zonked_lhs = lhs.zonk(meta_ctx);
                let zonked_rhs = rhs.zonk(meta_ctx);
                Expr::new(
                    ExprKind::Or(zonked_lhs, zonked_rhs),
                    self.ty.zonk(meta_ctx),
                    self.span,
                )
            }
            ExprKind::And(lhs, rhs) => {
                let zonked_lhs = lhs.zonk(meta_ctx);
                let zonked_rhs = rhs.zonk(meta_ctx);
                Expr::new(
                    ExprKind::And(zonked_lhs, zonked_rhs),
                    self.ty.zonk(meta_ctx),
                    self.span,
                )
            }
            ExprKind::Let(pat, rec, let_expr, body) => {
                let zonked_pat = pat.zonk(meta_ctx);
                let zonked_let_expr = let_expr.zonk(meta_ctx);
                let zonked_body = body.zonk(meta_ctx);
                Expr::new(
                    ExprKind::Let(zonked_pat, *rec, zonked_let_expr, zonked_body),
                    self.ty.zonk(meta_ctx),
                    self.span,
                )
            }
            // ExprKind::Fn(name, params, expr, body) => {
            //     let zonked_params = params
            //         .iter()
            //         .map(|param| param.zonk(meta_ctx))
            //         .collect_vec();
            //     let zonked_expr = expr.zonk(meta_ctx);
            //     let zonked_body = body.zonk(meta_ctx);
            //     Expr::new(
            //         ExprKind::Fn(*name, zonked_params, zonked_expr, zonked_body),
            //         self.ty.zonk(meta_ctx),
            //         self.span,
            //     )
            // }
            ExprKind::If(cond, then, else_) => {
                let zonked_cond = cond.zonk(meta_ctx);
                let zonked_then = then.zonk(meta_ctx);
                let zonked_else = else_.zonk(meta_ctx);
                Expr::new(
                    ExprKind::If(zonked_cond, zonked_then, zonked_else),
                    self.ty.zonk(meta_ctx),
                    self.span,
                )
            }
            ExprKind::Match(expr, arms) => {
                let zonked_expr = expr.zonk(meta_ctx);
                let zonked_arms = arms
                    .iter()
                    .map(|(pat, body)| {
                        let zonked_pat = pat.zonk(meta_ctx);
                        let zonked_body = body.zonk(meta_ctx);
                        (zonked_pat, zonked_body)
                    })
                    .collect_vec();
                Expr::new(
                    ExprKind::Match(zonked_expr, zonked_arms),
                    self.ty.zonk(meta_ctx),
                    self.span,
                )
            }
            ExprKind::List(exprs) => {
                let zonked_exprs = exprs.iter().map(|expr| expr.zonk(meta_ctx)).collect_vec();
                Expr::new(
                    ExprKind::List(zonked_exprs),
                    self.ty.zonk(meta_ctx),
                    self.span,
                )
            }
            ExprKind::Unit => Expr::new(ExprKind::Unit, self.ty.zonk(meta_ctx), self.span),
        }
    }
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
pub struct Pattern {
    pub kind: Box<PatternKind>,
    pub ty: Type,
    pub span: Span,
}

impl Pattern {
    pub fn new(kind: PatternKind, ty: Type, span: Span) -> Self {
        Self {
            kind: Box::new(kind),
            ty,
            span,
        }
    }

    fn zonk(&self, meta_ctx: &mut MetaContext) -> Pattern {
        match self.kind.as_ref() {
            PatternKind::Wildcard => {
                Pattern::new(PatternKind::Wildcard, self.ty.zonk(meta_ctx), self.span)
            }
            PatternKind::Ident(name) => {
                Pattern::new(PatternKind::Ident(*name), self.ty.zonk(meta_ctx), self.span)
            }
            PatternKind::Lit(lit) => Pattern::new(
                PatternKind::Lit(lit.clone()),
                self.ty.zonk(meta_ctx),
                self.span,
            ),
            PatternKind::List(pats) => {
                let zonked_pats = pats.iter().map(|pat| pat.zonk(meta_ctx)).collect_vec();
                Pattern::new(
                    PatternKind::List(zonked_pats),
                    self.ty.zonk(meta_ctx),
                    self.span,
                )
            }
            PatternKind::Pair(lhs, rhs) => {
                let zonked_lhs = lhs.zonk(meta_ctx);
                let zonked_rhs = rhs.zonk(meta_ctx);
                Pattern::new(
                    PatternKind::Pair(zonked_lhs, zonked_rhs),
                    self.ty.zonk(meta_ctx),
                    self.span,
                )
            }
            PatternKind::Unit => Pattern::new(PatternKind::Unit, self.ty.zonk(meta_ctx), self.span),
        }
    }
}

impl Display for Pattern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.kind.as_ref() {
            PatternKind::Wildcard => write!(f, "_"),
            PatternKind::Lit(lit) => write!(f, "{}", lit),
            PatternKind::Ident(ident) => write!(f, "{}", ident),
            PatternKind::List(pats) => write!(f, "[{}]", join(pats, ", ")),
            PatternKind::Pair(lhs, rhs) => write!(f, "({}, {})", lhs, rhs),
            PatternKind::Unit => write!(f, "()"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum PatternKind {
    Wildcard,
    Lit(Lit),
    Ident(ScopedIdent),
    List(Vec<Pattern>),
    Pair(Pattern, Pattern),
    Unit,
}

#[derive(Clone, PartialEq)]
pub enum Lit {
    Byte(u8),
    Int(i64),
    Rational(Rational64),
    Real(f64),
    Bool(bool),
    String(InternedString),
    Char(char),
}

impl Debug for Lit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Lit::Byte(b) => write!(f, "Byte({})", b),
            Lit::Int(i) => write!(f, "Int({})", i),
            Lit::Rational(r) => write!(f, "Rational({})", r),
            Lit::Real(r) => write!(f, "Real({})", r),
            Lit::Bool(b) => write!(f, "Bool({})", b),
            Lit::String(s) => write!(f, "String({})", s),
            Lit::Char(c) => write!(f, "Char({})", c),
        }
    }
}

impl Display for Lit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Lit::Byte(b) => write!(f, "{}", b),
            Lit::Int(i) => write!(f, "{}", i),
            Lit::Rational(r) => write!(f, "{}", r),
            Lit::Real(r) => write!(f, "{}", r),
            Lit::Bool(b) => write!(f, "{}", b),
            Lit::String(s) => write!(f, "{}", s),
            Lit::Char(c) => write!(f, "{}", c),
        }
    }
}
