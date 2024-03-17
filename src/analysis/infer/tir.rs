use super::{r#type::Type, substitution::Substitution};
use crate::utils::{
    ident::{Ident, ScopedIdent},
    intern::InternedString,
    list::List,
    span::Span,
};
use num_rational::Rational64;
use std::fmt::{Debug, Display};

#[derive(Clone, PartialEq)]
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
        match self.kind() {
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
            ExprKind::App(fun, arg) => Expr::new(
                ExprKind::App(fun.apply_subst(subst), arg.apply_subst(subst)),
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
            ExprKind::Let(name, rec, expr, body) => Expr::new(
                ExprKind::Let(
                    name.clone(),
                    *rec,
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
            ExprKind::Abs(params, expr) => Expr::new(
                ExprKind::Abs(params.clone(), expr.apply_subst(subst)),
                self.ty.apply_subst(subst),
                self.span,
            ),
            ExprKind::Unit => self.clone(),
        }
    }
}

impl Debug for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fn debug(
            f: &mut std::fmt::Formatter<'_>,
            mut indent: i32,
            expr: &Expr,
        ) -> std::fmt::Result {
            fn spaces(indent: i32) -> String {
                "  ".repeat(indent as usize)
            }

            match expr.kind() {
                ExprKind::Lit(lit) => {
                    writeln!(f, "{}Lit @ {}", spaces(indent), expr.span)?;
                    indent += 1;
                    writeln!(f, "{}{} : {:?}", spaces(indent), lit, expr.ty)
                }
                ExprKind::Var(var) => {
                    writeln!(f, "{}Var @ {}", spaces(indent), var.span())?;
                    indent += 1;
                    writeln!(f, "{}{} : {:?}", spaces(indent), var, expr.ty)
                }
                ExprKind::App(fun, arg) => {
                    writeln!(f, "{}App @ {}", spaces(indent), expr.span)?;
                    indent += 1;
                    debug(f, indent, fun)?;
                    debug(f, indent, arg)
                }
                ExprKind::Abs(param, expr) => {
                    writeln!(f, "{}Abs @ {}", spaces(indent), expr.span)?;
                    indent += 1;
                    writeln!(f, "{}Var @ {}", spaces(indent), param.span())?;
                    indent += 1;
                    writeln!(f, "{}{}", spaces(indent), param)?;
                    indent -= 1;
                    debug(f, indent, expr)
                }
                ExprKind::Or(_, _) => todo!(),
                ExprKind::And(_, _) => todo!(),
                ExprKind::Let(name, _, let_expr, body) => {
                    writeln!(f, "{}Let @ {}", spaces(indent), expr.span)?;
                    indent += 1;
                    writeln!(f, "{}Var @ {}", spaces(indent), name.span())?;
                    indent += 1;
                    writeln!(f, "{}{}", spaces(indent), name)?;
                    indent -= 1;
                    debug(f, indent, let_expr)?;
                    debug(f, indent, body)
                }
                ExprKind::Unit => todo!(),
            }
        }

        debug(f, 0, &self)
    }
}

#[derive(Clone, PartialEq)]
pub enum ExprKind {
    Lit(Lit),
    Var(ScopedIdent),
    App(Expr, Expr),
    Abs(ScopedIdent, Expr),
    Or(Expr, Expr),
    And(Expr, Expr),
    Let(ScopedIdent, bool, Expr, Expr),
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
