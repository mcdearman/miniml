use itertools::{join, Itertools};
use num_rational::Rational64;

use super::{
    meta_context::{self, MetaContext},
    r#type::Type,
};
use crate::utils::{
    ident::{Ident, ScopedIdent},
    intern::InternedString,
    span::Span,
};
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

// impl Debug for Root {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         writeln!(f, "Root @ {}", self.span)?;
//         for decl in &self.decls {
//             writeln!(f, "{:?}", decl)?;
//         }
//         Ok(())
//     }
// }

#[derive(Debug, Clone, PartialEq)]
pub struct Decl {
    pub kind: DeclKind,
    pub ty: Type,
    pub span: Span,
}

impl Decl {
    fn zonk(&self, meta_ctx: &mut MetaContext) -> Decl {
        match &self.kind {
            DeclKind::Let(pat, let_expr) => {
                let zonked_pat = pat.zonk(meta_ctx);
                let zonked_expr = let_expr.zonk(meta_ctx);
                Decl {
                    kind: DeclKind::Let(zonked_pat, zonked_expr),
                    ty: self.ty.zonk(meta_ctx),
                    span: self.span,
                }
            }
            DeclKind::Fn(name, params, fn_expr) => {
                let zonked_params = params
                    .iter()
                    .map(|param| param.zonk(meta_ctx))
                    .collect_vec();
                let zonked_expr = fn_expr.zonk(meta_ctx);
                Decl {
                    kind: DeclKind::Fn(*name, zonked_params, zonked_expr),
                    ty: self.ty.zonk(meta_ctx),
                    span: self.span,
                }
            }
        }
    }
}

// impl Debug for Decl {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         match &self.kind {
//             DeclKind::Let(name, expr) => {
//                 writeln!(f, "Let @ {}", self.span)?;
//                 writeln!(f, "Var @ {}", name.span)?;
//                 writeln!(f, "{}", name)?;
//                 writeln!(f, "{:?}", expr)
//             }
//             DeclKind::Fn(name, params, fn_expr) => {
//                 writeln!(f, "Fn @ {}", self.span)?;
//                 writeln!(f, "ScopedIdent @ {}", name.span)?;
//                 writeln!(f, "{}", name)?;
//                 for param in params {
//                     writeln!(f, "ScopedIdent @ {}", param.span)?;
//                     writeln!(f, "{}", param)?;
//                 }
//                 writeln!(f, "{:?}", fn_expr)
//             }
//         }
//     }
// }

#[derive(Debug, Clone, PartialEq)]
pub enum DeclKind {
    Let(Pattern, Expr),
    Fn(ScopedIdent, Vec<Pattern>, Expr),
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
            ExprKind::Lambda(params, fn_expr) => {
                let zonked_params = params
                    .iter()
                    .map(|param| param.zonk(meta_ctx))
                    .collect_vec();
                let zonked_expr = fn_expr.zonk(meta_ctx);
                Expr::new(
                    ExprKind::Lambda(zonked_params, zonked_expr),
                    self.ty.zonk(meta_ctx),
                    self.span,
                )
            }
            ExprKind::Apply(fun, args) => {
                let zonked_fun = fun.zonk(meta_ctx);
                let zonked_args = args.iter().map(|arg| arg.zonk(meta_ctx)).collect_vec();
                Expr::new(
                    ExprKind::Apply(zonked_fun, zonked_args),
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
            ExprKind::Let(pat, let_expr, body) => {
                let zonked_pat = pat.zonk(meta_ctx);
                let zonked_let_expr = let_expr.zonk(meta_ctx);
                let zonked_body = body.zonk(meta_ctx);
                Expr::new(
                    ExprKind::Let(zonked_pat, zonked_let_expr, zonked_body),
                    self.ty.zonk(meta_ctx),
                    self.span,
                )
            }
            ExprKind::Fn(name, params, expr, body) => {
                let zonked_params = params
                    .iter()
                    .map(|param| param.zonk(meta_ctx))
                    .collect_vec();
                let zonked_expr = expr.zonk(meta_ctx);
                let zonked_body = body.zonk(meta_ctx);
                Expr::new(
                    ExprKind::Fn(*name, zonked_params, zonked_expr, zonked_body),
                    self.ty.zonk(meta_ctx),
                    self.span,
                )
            }
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

// impl Debug for Expr {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         fn debug(
//             f: &mut std::fmt::Formatter<'_>,
//             mut indent: i32,
//             expr: &Expr,
//         ) -> std::fmt::Result {
//             fn spaces(indent: i32) -> String {
//                 "  ".repeat(indent as usize)
//             }

//             match expr.kind.as_ref() {
//                 ExprKind::Lit(lit) => {
//                     writeln!(f, "{}Lit @ {}", spaces(indent), expr.span)?;
//                     indent += 1;
//                     writeln!(f, "{}{} : {:?}", spaces(indent), lit, expr.ty)
//                 }
//                 ExprKind::Var(var) => {
//                     writeln!(f, "{}Var @ {}", spaces(indent), var.span)?;
//                     indent += 1;
//                     writeln!(f, "{}{} : {:?}", spaces(indent), var, expr.ty)
//                 }
//                 ExprKind::Apply(fun, args) => {
//                     writeln!(f, "{}Apply @ {}", spaces(indent), expr.span)?;
//                     indent += 1;
//                     debug(f, indent, fun)?;
//                     // debug(f, indent, arg)
//                     for arg in args {
//                         debug(f, indent, arg)?;
//                     }
//                     Ok(())
//                 }
//                 ExprKind::Lambda(params, fn_expr) => {
//                     writeln!(f, "{}Lambda @ {}", spaces(indent), expr.span)?;
//                     indent += 1;
//                     for param in params {
//                         writeln!(f, "{}ScopedIdent @ {}", spaces(indent), param.span)?;
//                     }
//                     indent += 1;
//                     writeln!(f, "{}{}", spaces(indent), join(params, ", "))?;
//                     indent -= 1;
//                     debug(f, indent, &fn_expr)
//                 }
//                 ExprKind::Or(lhs, rhs) => {
//                     writeln!(f, "{}Or @ {}", spaces(indent), expr.span)?;
//                     indent += 1;
//                     debug(f, indent, lhs)?;
//                     debug(f, indent, rhs)
//                 }
//                 ExprKind::And(lhs, rhs) => {
//                     writeln!(f, "{}And @ {}", spaces(indent), expr.span)?;
//                     indent += 1;
//                     debug(f, indent, lhs)?;
//                     debug(f, indent, rhs)
//                 }
//                 ExprKind::Let(name, let_expr, body) => {
//                     writeln!(f, "{}Let @ {}", spaces(indent), expr.span)?;
//                     indent += 1;
//                     writeln!(f, "{}Var @ {}", spaces(indent), name.span)?;
//                     indent += 1;
//                     writeln!(f, "{}{}", spaces(indent), name)?;
//                     indent -= 1;
//                     debug(f, indent, let_expr)?;
//                     debug(f, indent, body)
//                 }
//                 ExprKind::Fn(name, params, fn_expr, body) => {
//                     writeln!(f, "{}Fn @ {}", spaces(indent), expr.span)?;
//                     indent += 1;
//                     writeln!(f, "{}ScopedIdent @ {}", spaces(indent), name.span)?;
//                     indent += 1;
//                     writeln!(f, "{}{}", spaces(indent), name)?;
//                     indent -= 1;
//                     for param in params {
//                         writeln!(f, "{}ScopedIdent @ {}", spaces(indent), param.span)?;
//                     }
//                     indent += 1;
//                     writeln!(f, "{}{}", spaces(indent), join(params, ", "))?;
//                     indent -= 1;
//                     debug(f, indent, fn_expr)?;
//                     debug(f, indent, body)
//                 }
//                 ExprKind::If(cond, then_expr, else_expr) => {
//                     writeln!(f, "{}If @ {}", spaces(indent), expr.span)?;
//                     indent += 1;
//                     debug(f, indent, cond)?;
//                     debug(f, indent, then_expr)?;
//                     debug(f, indent, else_expr)
//                 }
//                 ExprKind::Match(match_expr, arms) => {
//                     writeln!(f, "{}Match @ {}", spaces(indent), expr.span)?;
//                     indent += 1;
//                     debug(f, indent, match_expr)?;
//                     for (pat, arm) in arms {
//                         writeln!(f, "{}Pattern @ {}", spaces(indent), pat.span)?;
//                         indent += 1;
//                         writeln!(f, "{}{}", spaces(indent), pat)?;
//                         indent -= 1;
//                         debug(f, indent, arm)?;
//                     }
//                     Ok(())
//                 }
//                 ExprKind::List(exprs) => {
//                     writeln!(f, "{}List @ {}", spaces(indent), expr.span)?;
//                     indent += 1;
//                     for expr in exprs {
//                         debug(f, indent, expr)?;
//                     }
//                     Ok(())
//                 }
//                 ExprKind::Unit => todo!(),
//             }
//         }

//         debug(f, 0, &self)
//     }
// }

#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind {
    Lit(Lit),
    Var(ScopedIdent),
    Apply(Expr, Vec<Expr>),
    Lambda(Vec<Pattern>, Expr),
    Or(Expr, Expr),
    And(Expr, Expr),
    Let(Pattern, Expr, Expr),
    Fn(ScopedIdent, Vec<Pattern>, Expr, Expr),
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
