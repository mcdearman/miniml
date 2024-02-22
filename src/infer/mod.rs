use self::{
    constraint::Constraint,
    context::Context,
    error::{InferResult, TypeError},
    r#type::Type,
    registry::Registry,
    scheme::Scheme,
    substitution::Substitution,
    tir::*,
    ty_var::TyVar,
};
use crate::{
    rename::nir,
    utils::{intern::InternedString, list::List, unique_id::UniqueId},
};
use itertools::Itertools;
use std::collections::HashMap;

mod constraint;
pub mod context;
pub mod error;
pub mod registry;
mod scheme;
pub mod substitution;
pub mod tir;
mod ty_var;
pub mod r#type;

#[derive(Debug)]
pub struct TypeSolver<'src> {
    src: &'src str,
    nir: nir::Root,
    ctx: Context,
    reg: Registry,
    builtins: HashMap<UniqueId, InternedString>,
    constraints: Vec<Constraint>,
    sub: Substitution,
}

impl<'src> TypeSolver<'src> {
    pub fn new(
        src: &'src str,
        nir: nir::Root,
        builtins: HashMap<UniqueId, InternedString>,
    ) -> Self {
        Self {
            src,
            nir,
            ctx: Context::from_builtins(&builtins),
            reg: Registry::new(),
            builtins,
            constraints: vec![],
            sub: Substitution::new(),
        }
    }

    pub fn solve(&mut self) -> InferResult<Root> {
        let mut decls = vec![];
        for decl in self.nir.decls() {
            let (cs, ctx_ret, d) = self.infer_decl(*decl)?;
            for c in cs {
                match c {
                    Constraint::Equal(t1, t2) => {
                        self.sub = t1
                            .apply_subst(&self.sub)
                            .unify(t2.apply_subst(&self.sub))?
                            .compose(&self.sub);
                    }
                }
            }
            // self.ctx = ctx_ret.union(ctx.clone());
            decls.push(d.clone());
        }
        Ok(Root::new(decls, *self.nir.span()).apply_subst(&self.sub))
    }

    fn infer_decl(&mut self, decl: nir::Decl) -> InferResult<(Vec<Constraint>, Context, Decl)> {
        match decl.kind() {
            nir::DeclKind::DataType(dt) => match dt.kind() {
                nir::DataTypeKind::Record { fields } => {
                    let mut constraints = vec![];
                    let mut new_fields = vec![];
                    let mut tmp_ctx = self.ctx.clone();
                    for (field, hint) in fields {
                        let var = Type::Var(TyVar::fresh());
                        let field_type = match hint.kind() {
                            nir::TypeHintKind::Int => Type::Int,
                            nir::TypeHintKind::Bool => Type::Bool,
                            nir::TypeHintKind::String => Type::String,
                            nir::TypeHintKind::Ident(ident) => {
                                if let Some(scm) = self.ctx.get(ident.id()) {
                                    scm.instantiate()
                                } else {
                                    return Err(TypeError::from(format!(
                                        "unbound type: {:?} - \"{}\"",
                                        ident,
                                        self.src[*ident.span()].to_string()
                                    )));
                                }
                            }
                            nir::TypeHintKind::List(_) => todo!(),
                            nir::TypeHintKind::Fn(_, _) => todo!(),
                            nir::TypeHintKind::Unit => Type::Unit,
                        };
                        constraints.push(Constraint::Equal(var.clone(), field_type.clone()));
                        new_fields.push((Ident::new(*field.name(), *field.span()), var.clone()));
                        tmp_ctx = self.ctx.union(tmp_ctx);
                        // tmp_ctx.extend(*field.name(), Scheme::new(vec![], field_type.clone()));
                    }
                    let ty = Type::Record(
                        *dt.name().id(),
                        new_fields
                            .iter()
                            .map(|(k, v)| (*k.name(), v.clone()))
                            .collect(),
                    );
                    tmp_ctx.extend(*dt.name().id(), Scheme::new(vec![], ty.clone()));
                    Ok((
                        constraints,
                        tmp_ctx,
                        Decl::new(
                            DeclKind::DataType(DataType::new(
                                UniqueIdent::new(*dt.name().id(), *dt.name().span()),
                                DataTypeKind::Record { fields: new_fields },
                                ty.clone(),
                                *decl.span(),
                            )),
                            ty,
                            *decl.span(),
                        ),
                    ))
                }
            },
            nir::DeclKind::Let { name, expr } => {
                let (cs, ty, ectx, new_expr) = self.infer_expr(*expr)?;
                let scheme = Scheme::generalize(ectx.clone(), ty.clone());
                let mut tmp_ctx = ectx.clone();
                tmp_ctx.extend(*name.id(), scheme.clone());
                Ok((
                    cs,
                    self.ctx.union(tmp_ctx),
                    Decl::new(
                        DeclKind::Let {
                            name: UniqueIdent::new(*name.id(), *name.span()),
                            expr: new_expr,
                        },
                        ty,
                        *decl.span(),
                    ),
                ))
            }
            nir::DeclKind::Fn { name, params, expr } => {
                let mut ty_binders = vec![];
                let mut tmp_ctx = self.ctx.clone();
                let mut new_params = vec![];
                for p in params.clone() {
                    let ty_binder = Type::Var(TyVar::fresh());
                    ty_binders.push(ty_binder.clone());
                    tmp_ctx = self.ctx.union(tmp_ctx);
                    tmp_ctx.extend(*p.id(), Scheme::new(vec![], ty_binder.clone()));
                    new_params.push(UniqueIdent::new(*p.id(), *p.span()));
                }
                let ty = Type::Var(TyVar::fresh());
                tmp_ctx.extend(*name.id(), Scheme::new(vec![], ty.clone()));
                let (mut cs, t, c, new_expr) = self.infer_expr(*expr)?;
                cs.push(Constraint::Equal(
                    ty.clone(),
                    Type::Lambda(ty_binders.clone(), Box::new(t.clone())),
                ));
                Ok((
                    cs,
                    tmp_ctx.union(c),
                    Decl::new(
                        DeclKind::Fn {
                            name: UniqueIdent::new(*name.id(), *name.span()),
                            params: new_params,
                            expr: new_expr,
                        },
                        ty,
                        *decl.span(),
                    ),
                ))
            }
        }
    }

    fn infer_expr(
        &mut self,
        expr: nir::Expr,
    ) -> InferResult<(Vec<Constraint>, Type, Context, Expr)> {
        match expr.kind() {
            nir::ExprKind::Lit(lit) => match *lit {
                nir::Lit::Int(n) => Ok((
                    vec![],
                    Type::Int,
                    self.ctx,
                    Expr::new(ExprKind::Lit(Lit::Int(n)), Type::Int, *expr.span()),
                )),
                nir::Lit::Bool(b) => Ok((
                    vec![],
                    Type::Bool,
                    self.ctx,
                    Expr::new(ExprKind::Lit(Lit::Bool(b)), Type::Bool, *expr.span()),
                )),
                nir::Lit::String(s) => Ok((
                    vec![],
                    Type::String,
                    self.ctx,
                    Expr::new(ExprKind::Lit(Lit::String(s)), Type::String, *expr.span()),
                )),
            },
            nir::ExprKind::Ident(name) => {
                if let Some(scm) = self.ctx.get(name.id()) {
                    let ty = scm.instantiate();
                    // println!("inst ident: {:?} {:?}", name, ty);
                    Ok((
                        vec![],
                        ty.clone(),
                        self.ctx.clone(),
                        Expr::new(
                            ExprKind::Ident(UniqueIdent::new(*name.id(), *name.span())),
                            ty.clone(),
                            *expr.span(),
                        ),
                    ))
                } else {
                    // println!("{:#?}", ctx);
                    Err(TypeError::from(format!(
                        "unbound variable: {:?} - \"{}\"",
                        expr,
                        self.src[*name.span()].to_string()
                    )))
                }
            }
            nir::ExprKind::Apply { fun, args } => {
                let (mut cs1, t1, mut ctx1, fun) = self.infer_expr(*fun)?;
                let mut new_args = vec![];
                let mut ty_args = vec![];
                for a in args {
                    let (cs2, t2, ctx2, e2) = self.infer_expr(*a)?;
                    ctx1 = ctx2.union(ctx1);
                    cs1 = cs1.into_iter().chain(cs2.into_iter()).collect();
                    new_args.push(e2);
                    ty_args.push(t2);
                }
                let ty_ret = Type::Var(TyVar::fresh());
                cs1.push(Constraint::Equal(
                    t1.clone(),
                    Type::Lambda(ty_args, Box::new(ty_ret.clone())),
                ));
                // println!("apply constraints: {:#?}", cs1);
                Ok((
                    cs1,
                    ty_ret.clone(),
                    ctx1,
                    Expr::new(
                        ExprKind::Apply {
                            fun,
                            args: new_args,
                        },
                        ty_ret,
                        *expr.span(),
                    ),
                ))
            }
            nir::ExprKind::Let { name, expr, body } => {
                let (cs1, t1, mut ctx1, expr) = self.infer_expr(*expr)?;
                let scheme = Scheme::generalize(ctx1.clone(), t1.clone());
                ctx1.extend(*name.id(), scheme);
                let (cs2, t2, ctx2, body) = self.infer_expr(*body)?;
                let ctx = ctx2.union(ctx1);
                let cs = cs1.into_iter().chain(cs2.into_iter()).collect::<Vec<_>>();
                Ok((
                    cs,
                    t2.clone(),
                    ctx,
                    Expr::new(
                        ExprKind::Let {
                            name: UniqueIdent::new(*name.id(), *name.span()),
                            expr: expr.clone(),
                            body,
                        },
                        t2,
                        *expr.span(),
                    ),
                ))
            }
            nir::ExprKind::Fn {
                name,
                params,
                expr,
                body,
            } => {
                let mut ty_binders = vec![];
                let mut tmp_ctx = self.ctx.clone();
                let mut new_params = vec![];
                for p in params.clone() {
                    let ty_binder = Type::Var(TyVar::fresh());
                    ty_binders.push(ty_binder.clone());
                    tmp_ctx = self.ctx.union(tmp_ctx);
                    tmp_ctx.extend(*p.id(), Scheme::new(vec![], ty_binder.clone()));
                    new_params.push(UniqueIdent::new(*p.id(), *p.span()));
                }
                let ty = Type::Var(TyVar::fresh());
                tmp_ctx.extend(*name.id(), Scheme::new(vec![], ty.clone()));
                let (cs, t, c, expr) = self.infer_expr(*expr)?;
                // let ty = Type::Lambda(ty_binders.clone(), Box::new(t.clone()));
                let (cs_body, ty_body, ctx_body, body) = self.infer_expr(*body)?;
                let cs = cs
                    .into_iter()
                    .chain(cs_body.into_iter())
                    .collect::<Vec<_>>();
                Ok((
                    cs,
                    ty_body.clone(),
                    ctx_body.union(tmp_ctx),
                    Expr::new(
                        ExprKind::Fn {
                            name: UniqueIdent::new(*name.id(), *name.span()),
                            params: new_params,
                            expr: expr.clone(),
                            body,
                        },
                        ty_body,
                        *expr.span(),
                    ),
                ))
            }
            nir::ExprKind::If { cond, then, else_ } => {
                let (cs1, t1, mut ctx1, cond) = self.infer_expr(*cond)?;
                let (cs2, t2, mut ctx2, then) = self.infer_expr(*then)?;
                let (cs3, t3, ctx3, else_) = self.infer_expr(*else_)?;
                let ctx = ctx3.union(ctx2).union(ctx1);
                let mut cs = cs1
                    .into_iter()
                    .chain(cs2.into_iter())
                    .chain(cs3.into_iter())
                    .collect_vec();
                cs.push(Constraint::Equal(t1, Type::Bool));
                cs.push(Constraint::Equal(t2.clone(), t3));
                // println!("if constraints: {:#?}", cs);
                Ok((
                    cs,
                    t2.clone(),
                    ctx,
                    Expr::new(ExprKind::If { cond, then, else_ }, t2, *expr.span()),
                ))
            }
            nir::ExprKind::Lambda { params, expr } => {
                let mut ty_binders = vec![];
                let mut tmp_ctx = self.ctx.clone();
                let mut new_params = vec![];
                for p in params.clone() {
                    let ty_binder = Type::Var(TyVar::fresh());
                    ty_binders.push(ty_binder.clone());
                    tmp_ctx = self.ctx.union(tmp_ctx);
                    tmp_ctx.extend(*p.id(), Scheme::new(vec![], ty_binder.clone()));
                    new_params.push(UniqueIdent::new(*p.id(), *p.span()));
                }
                let (cs, t, c, expr) = self.infer_expr(*expr)?;
                let ty = Type::Lambda(ty_binders.clone(), Box::new(t.clone()));
                Ok((
                    cs,
                    ty.clone(),
                    tmp_ctx.union(c),
                    Expr::new(
                        ExprKind::Lambda {
                            params: new_params,
                            expr: expr.clone(),
                        },
                        ty,
                        *expr.span(),
                    ),
                ))
            }
            nir::ExprKind::List(exprs) => {
                let mut cs = vec![];
                let mut ty_args = vec![];
                let mut new_exprs = vec![];
                for e in exprs {
                    let (cs2, t2, ctx2, e2) = self.infer_expr(*e)?;
                    self.ctx = ctx2.union(self.ctx.clone());
                    cs = cs.into_iter().chain(cs2.into_iter()).collect();
                    new_exprs.push(e2);
                    ty_args.push(t2);
                }
                let ty = Type::List(Box::new(ty_args.clone()[0].clone()));
                for t in ty_args.clone() {
                    cs.push(Constraint::Equal(t, ty_args[0].clone()));
                }
                Ok((
                    cs,
                    ty.clone(),
                    self.ctx.clone(),
                    Expr::new(ExprKind::List(List::from(new_exprs)), ty, *expr.span()),
                ))
            }
            nir::ExprKind::Record { name, fields } => {
                if let Some(ident) = name {
                    if let Some(scm) = self.ctx.get(ident.id()) {
                        todo!()
                    } else {
                        return Err(TypeError::from(format!(
                            "unbound type: {:?} - \"{}\"",
                            ident,
                            self.src[*ident.span()].to_string()
                        )));
                    }
                }
                todo!()
            }
            nir::ExprKind::Unit => Ok((
                vec![],
                Type::Unit,
                self.ctx.clone(),
                Expr::new(ExprKind::Unit, Type::Unit, *expr.span()),
            )),
            _ => todo!(),
        }
    }
}
