use super::{r#type::Type, substitution::Substitution};
use crate::utils::{
    ident::{Ident, ScopedIdent},
    intern::InternedString,
    list::List,
    span::Span,
};

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

    pub fn span(&self) -> Span {
        self.span
    }

    pub fn apply_subst(&self, subst: &Substitution) -> Self {
        Self {
            decls: self
                .clone()
                .decls
                .into_iter()
                .map(|d| d.apply_subst(subst))
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

    pub fn kind(&self) -> DeclKind {
        self.kind
    }

    pub fn span(&self) -> Span {
        self.span
    }

    pub fn apply_subst(&self, subst: &Substitution) -> Decl {
        match self.kind() {
            DeclKind::DataType(dt) => match dt.kind() {
                DataTypeKind::Record { fields } => Decl::new(
                    DeclKind::DataType(DataType::new(
                        dt.name().clone(),
                        DataTypeKind::Record {
                            fields: fields
                                .iter()
                                .map(|(name, ty)| (name.clone(), ty.apply_subst(subst)))
                                .collect(),
                        },
                        dt.ty().apply_subst(subst),
                        self.span,
                    )),
                    self.ty.apply_subst(subst),
                    self.span,
                ),
            },
            DeclKind::Let { name, expr } => Decl::new(
                DeclKind::Let {
                    name: name.clone(),
                    expr: expr.apply_subst(subst),
                },
                self.ty.apply_subst(subst),
                self.span,
            ),
            DeclKind::Fn { name, params, expr } => Decl::new(
                DeclKind::Fn {
                    name: name.clone(),
                    params: params.clone(),
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
    DataType(DataType),
    Let {
        name: ScopedIdent,
        expr: Expr,
    },
    Fn {
        name: ScopedIdent,
        params: Vec<ScopedIdent>,
        expr: Expr,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct DataType {
    name: ScopedIdent,
    kind: DataTypeKind,
    ty: Type,
    span: Span,
}

impl DataType {
    pub fn new(name: ScopedIdent, kind: DataTypeKind, ty: Type, span: Span) -> Self {
        Self {
            name,
            kind,
            ty,
            span,
        }
    }

    pub fn name(&self) -> ScopedIdent {
        self.name
    }

    pub fn kind(&self) -> DataTypeKind {
        self.kind
    }

    pub fn ty(&self) -> Type {
        self.ty
    }

    pub fn span(&self) -> Span {
        self.span
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum DataTypeKind {
    Record { fields: Vec<(Ident, Type)> },
    // Sum {
    //     cases: Vec<(Ident, Option<SumTypeCaseHint>)>,
    // },
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

    pub fn kind(&self) -> ExprKind {
        *self.kind
    }

    pub fn ty(&self) -> Type {
        self.ty
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
            ExprKind::Ident(name) => Expr::new(
                ExprKind::Ident(name.clone()),
                self.ty.apply_subst(subst),
                self.span,
            ),
            ExprKind::Apply { fun, args } => Expr::new(
                ExprKind::Apply {
                    fun: fun.apply_subst(subst),
                    args: args.into_iter().map(|arg| arg.apply_subst(subst)).collect(),
                },
                self.ty.apply_subst(subst),
                self.span,
            ),
            ExprKind::Let { name, expr, body } => Expr::new(
                ExprKind::Let {
                    name: name.clone(),
                    expr: expr.apply_subst(subst),
                    body: body.apply_subst(subst),
                },
                self.ty.apply_subst(subst),
                self.span,
            ),
            ExprKind::Fn {
                name,
                params,
                expr,
                body,
            } => Expr::new(
                ExprKind::Fn {
                    name: name.clone(),
                    params: params.clone(),
                    expr: expr.apply_subst(subst),
                    body: body.apply_subst(subst),
                },
                self.ty.apply_subst(subst),
                self.span,
            ),
            ExprKind::If { cond, then, else_ } => Expr::new(
                ExprKind::If {
                    cond: cond.apply_subst(subst),
                    then: then.apply_subst(subst),
                    else_: else_.apply_subst(subst),
                },
                self.ty.apply_subst(subst),
                self.span,
            ),
            ExprKind::Lambda { params, expr } => Expr::new(
                ExprKind::Lambda {
                    params: params.clone(),
                    expr: expr.apply_subst(subst),
                },
                self.ty.apply_subst(subst),
                self.span,
            ),
            ExprKind::List(l) => Expr::new(
                ExprKind::List(l.clone()),
                self.ty.apply_subst(subst),
                self.span,
            ),
            ExprKind::Record { name, fields } => Expr::new(
                ExprKind::Record {
                    name: name.clone(),
                    fields: fields
                        .into_iter()
                        .map(|(k, v)| (k.clone(), v.apply_subst(subst)))
                        .collect(),
                },
                self.ty.apply_subst(subst),
                self.span,
            ),
            ExprKind::Unit => self.clone(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind {
    Lit(Lit),
    Ident(ScopedIdent),
    Apply {
        fun: Expr,
        args: Vec<Expr>,
    },
    If {
        cond: Expr,
        then: Expr,
        else_: Expr,
    },
    Let {
        name: ScopedIdent,
        expr: Expr,
        body: Expr,
    },
    Fn {
        name: ScopedIdent,
        params: Vec<ScopedIdent>,
        expr: Expr,
        body: Expr,
    },
    Lambda {
        params: Vec<ScopedIdent>,
        expr: Expr,
    },
    List(List<Expr>),
    Record {
        name: ScopedIdent,
        fields: Vec<(Ident, Expr)>,
    },
    Unit,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Lit {
    Int(i64),
    Bool(bool),
    String(InternedString),
}
