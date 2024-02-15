use super::{r#type::Type, substitution::Substitution};
use crate::utils::{span::Span, unique_id::UniqueId};

#[derive(Debug, Clone, PartialEq)]
pub struct Root {
    decls: Vec<Decl>,
    span: Span,
}

impl Root {
    pub fn new(decls: Vec<Decl>, span: Span) -> Self {
        Self { decls, span }
    }

    pub fn decls(&self) -> &[Decl] {
        &self.decls
    }

    pub fn span(&self) -> &Span {
        &self.span
    }

    pub fn apply_subst(&self, subst: Substitution) -> Self {
        Self {
            decls: self
                .decls
                .into_iter()
                .map(|d| d.apply_subst(subst.clone()))
                .collect(),
            span: self.span,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Decl {
    kind: DeclKind,
    ty: Type,
    span: Span,
}

impl Decl {
    pub fn new(kind: DeclKind, ty: Type, span: Span) -> Self {
        Self { kind, ty, span }
    }

    pub fn kind(&self) -> &DeclKind {
        &self.kind
    }

    pub fn span(&self) -> &Span {
        &self.span
    }

    pub fn apply_subst(&self, subst: Substitution) -> Decl {
        match self.kind {
            DeclKind::Let { name, expr } => Decl::new(
                DeclKind::Let {
                    name,
                    expr: expr.apply_subst(subst),
                },
                self.ty.apply_subst(subst),
                self.span,
            ),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum DeclKind {
    Let { name: Ident, expr: Expr },
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

    pub fn ty(&self) -> &Type {
        &self.ty
    }

    pub fn span(&self) -> &Span {
        &self.span
    }

    pub fn apply_subst(&self, subst: Substitution) -> Self {
        match *self.kind {
            ExprKind::Lit(l) => Expr::new(ExprKind::Lit(l), self.ty.apply_subst(subst), self.span),
            ExprKind::Ident(name) => {
                Expr::new(ExprKind::Ident(name), self.ty.apply_subst(subst), self.span)
            }
            ExprKind::Lambda { params, expr } => Expr::new(
                ExprKind::Lambda {
                    params,
                    expr: expr.apply_subst(subst),
                },
                self.ty.apply_subst(subst),
                self.span,
            ),
            ExprKind::Apply { fun, args } => Expr::new(
                ExprKind::Apply {
                    fun: fun.apply_subst(subst.clone()),
                    args: args
                        .into_iter()
                        .map(|arg| arg.apply_subst(subst.clone()))
                        .collect(),
                },
                self.ty.apply_subst(subst),
                self.span,
            ),
            ExprKind::Let { name, expr, body } => Expr::new(
                ExprKind::Let {
                    name,
                    expr: expr.apply_subst(subst.clone()),
                    body: body.apply_subst(subst.clone()),
                },
                self.ty.apply_subst(subst),
                self.span,
            ),
            ExprKind::If { cond, then, else_ } => Expr::new(
                ExprKind::If {
                    cond: cond.apply_subst(subst.clone()),
                    then: then.apply_subst(subst.clone()),
                    else_: else_.apply_subst(subst.clone()),
                },
                self.ty.apply_subst(subst),
                self.span,
            ),
            ExprKind::Unit => *self,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind {
    Lit(Lit),
    Ident(Ident),
    Apply { fun: Expr, args: Vec<Expr> },
    If { cond: Expr, then: Expr, else_: Expr },
    Let { name: Ident, expr: Expr, body: Expr },
    Lambda { params: Vec<Ident>, expr: Expr },
    Unit,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Ident {
    id: UniqueId,
    span: Span,
}

impl Ident {
    pub fn new(id: UniqueId, span: Span) -> Self {
        Self { id, span }
    }

    pub fn id(&self) -> &UniqueId {
        &self.id
    }

    pub fn span(&self) -> &Span {
        &self.span
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Lit {
    Int(i64),
    Bool(bool),
}
