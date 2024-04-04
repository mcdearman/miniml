use itertools::Itertools;

use super::{r#type::Type, substitution::Substitution};
use crate::utils::{
    ident::{Ident, ScopedIdent},
    intern::InternedString,
    list::List,
    span::Span,
};
use std::fmt::{Debug, Display};

#[derive(Clone, PartialEq)]
pub struct Root {
    pub decls: Vec<Decl>,
    pub span: Span,
}

#[derive(Clone, PartialEq)]
pub struct Decl {
    pub kind: DeclKind,
    pub span: Span,
}

#[derive(Clone, PartialEq)]
pub enum DeclKind {
    Let(ScopedIdent, Expr),
    Fn(ScopedIdent, Vec<ScopedIdent>, Expr),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expr {
    kind: Box<ExprKind>,
    ty: Type,
    span: Span,
}

impl Expr {
    pub fn new(kind: ExprKind, ty: Type, span: Span) -> Self {
        Self {
            kind: Box::new(kind),
            ty,
            span,
        }
    }

    pub fn kind(&self) -> &ExprKind {
        &self.kind
    }

    pub fn ty(&self) -> Type {
        self.ty.clone()
    }

    pub fn span(&self) -> Span {
        self.span
    }

    pub fn apply_subst(&self, subst: &Substitution) -> Self {
        match self.kind.as_ref() {
            ExprKind::Lit(l) => Expr::new(
                ExprKind::Lit(l.clone()),
                self.ty.apply_subst(subst),
                self.span,
            ),
            ExprKind::Var(name) => Expr::new(
                ExprKind::Var(name.clone()),
                self.ty.apply_subst(subst),
                self.span,
            ),
            ExprKind::Apply(fun, args) => Expr::new(
                ExprKind::Apply(
                    fun.apply_subst(subst),
                    args.iter().map(|arg| arg.apply_subst(subst)).collect_vec(),
                ),
                self.ty.apply_subst(subst),
                self.span,
            ),
            ExprKind::Or(lhs, rhs) => Expr::new(
                ExprKind::Or(lhs.apply_subst(subst), rhs.apply_subst(subst)),
                self.ty.apply_subst(subst),
                self.span,
            ),
            ExprKind::And(lhs, rhs) => Expr::new(
                ExprKind::And(lhs.apply_subst(subst), rhs.apply_subst(subst)),
                self.ty.apply_subst(subst),
                self.span,
            ),
            ExprKind::Let(name, expr, body) => Expr::new(
                ExprKind::Let(
                    name.clone(),
                    expr.apply_subst(subst),
                    body.apply_subst(subst),
                ),
                self.ty.apply_subst(subst),
                self.span,
            ),
            ExprKind::Fn(name, params, expr, body) => Expr::new(
                ExprKind::Fn(
                    name.clone(),
                    params.clone(),
                    expr.apply_subst(subst),
                    body.apply_subst(subst),
                ),
                self.ty.apply_subst(subst),
                self.span,
            ),
            // ExprKind::If { cond, then, else_ } => Expr::new(
            //     ExprKind::If {
            //         cond: cond.apply_subst(subst),
            //         then: then.apply_subst(subst),
            //         else_: else_.apply_subst(subst),
            //     },
            //     self.ty.apply_subst(subst),
            //     self.span,
            // ),
            ExprKind::Lambda(params, expr) => Expr::new(
                ExprKind::Lambda(params.clone(), expr.apply_subst(subst)),
                self.ty.apply_subst(subst),
                self.span,
            ),
            ExprKind::Unit => self.clone(),
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

//             match expr.kind() {
//                 ExprKind::Lit(lit) => {
//                     writeln!(f, "{}Lit @ {}", spaces(indent), expr.span)?;
//                     indent += 1;
//                     writeln!(f, "{}{} : {:?}", spaces(indent), lit, expr.ty)
//                 }
//                 ExprKind::Var(var) => {
//                     writeln!(f, "{}Var @ {}", spaces(indent), var.span())?;
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
//                     writeln!(f, "{}Var @ {}", spaces(indent), )?;
//                     indent += 1;
//                     writeln!(f, "{}{}", spaces(indent), param)?;
//                     indent -= 1;
//                     debug(f, indent, fn_expr)
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
//                 ExprKind::Let(name, _, let_expr, body) => {
//                     writeln!(f, "{}Let @ {}", spaces(indent), expr.span)?;
//                     indent += 1;
//                     writeln!(f, "{}Var @ {}", spaces(indent), name.span())?;
//                     indent += 1;
//                     writeln!(f, "{}{}", spaces(indent), name)?;
//                     indent -= 1;
//                     debug(f, indent, let_expr)?;
//                     debug(f, indent, body)
//                 }
//                 ExprKind::Unit => todo!(),
//             }
//         }

//         debug(f, 0, &self)
//     }
// }

#[derive(Clone, PartialEq)]
pub enum ExprKind {
    Lit(Lit),
    Var(ScopedIdent),
    Apply(Expr, Vec<Expr>),
    Lambda(Vec<ScopedIdent>, Expr),
    Or(Expr, Expr),
    And(Expr, Expr),
    Let(ScopedIdent, Expr, Expr),
    Fn(ScopedIdent, Vec<ScopedIdent>, Expr, Expr),
    Unit,
}

impl Debug for ExprKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
        // match self {
        //     Self::Lit(lit) => write!(f, "Lit({:?})", lit),
        //     Self::Var(var) => write!(f, "Var({:?})", var),
        //     Self::App(fun, arg) => {
        //         // write!(f, "App({:?}, {:?})", fun, arg)?;

        //     }
        //     Self::Abs(arg0, arg1) => f.debug_tuple("Abs").field(arg0).field(arg1).finish(),
        //     Self::Or(arg0, arg1) => f.debug_tuple("Or").field(arg0).field(arg1).finish(),
        //     Self::And(arg0, arg1) => f.debug_tuple("And").field(arg0).field(arg1).finish(),
        //     Self::Let(arg0, arg1, arg2, arg3) => f
        //         .debug_tuple("Let")
        //         .field(arg0)
        //         .field(arg1)
        //         .field(arg2)
        //         .field(arg3)
        //         .finish(),
        //     Self::Unit => write!(f, "Unit"),
        // }
    }
}

#[derive(Clone, PartialEq)]
pub enum Lit {
    Int(i64),
    Bool(bool),
}

impl Debug for Lit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Lit::Int(i) => write!(f, "Int({})", i),
            Lit::Bool(b) => write!(f, "Bool({})", b),
        }
    }
}

impl Display for Lit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Lit::Int(i) => write!(f, "{}", i),
            Lit::Bool(b) => write!(f, "{}", b),
        }
    }
}