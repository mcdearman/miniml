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
use std::{any::Any, collections::HashMap};

mod constraint;
pub mod context;
pub mod error;
pub mod registry;
mod scheme;
pub mod substitution;
pub mod tir;
mod ty_var;
pub mod r#type;

#[derive(Debug, Clone, PartialEq)]
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

    pub fn solve(&mut self) -> (Option<Root>, Vec<TypeError>) {
        let mut errors = vec![];

        let solved_decls = self
            .nir
            .decls()
            .to_vec()
            .into_iter()
            .map(|d| self.infer_decl(&d))
            .filter_map(|r| r.map_err(|e| errors.push(e)).ok())
            .collect_vec();

        for c in self.constraints.iter() {
            match c {
                Constraint::Equal(t1, t2) => {
                    self.sub = t1
                        .apply_subst(&self.sub)
                        .unify(&t2.apply_subst(&self.sub))
                        .map(|s| s.compose(&self.sub))
                        .unwrap_or_else(|e| {
                            errors.push(e);
                            self.sub.clone()
                        });
                }
            }
        }

        (
            if solved_decls.is_empty() {
                None
            } else {
                Some(Root::new(solved_decls, self.nir.span()).apply_subst(&self.sub))
            },
            errors,
        )
    }

    fn infer_decl(&mut self, decl: &nir::Decl) -> InferResult<Decl> {
        match decl.kind() {
            nir::DeclKind::DataType(dt) => todo!(),
            // match dt.kind() {
            //     nir::DataTypeKind::Record { fields } => {
            //         let mut inferred_fields = vec![];
            //         for (field, hint) in fields {
            //             let var = Type::Var(TyVar::fresh());
            //             let field_type = match hint.kind() {
            //                 nir::TypeHintKind::Int => Type::Int,
            //                 nir::TypeHintKind::Bool => Type::Bool,
            //                 nir::TypeHintKind::String => Type::String,
            //                 nir::TypeHintKind::Ident(ident) => {
            //                     if let Some(scm) = self.ctx.get(ident.id()) {
            //                         scm.instantiate()
            //                     } else {
            //                         return Err(TypeError::from(format!(
            //                             "unbound type: {:?} - \"{}\"",
            //                             ident,
            //                             self.src[*ident.span()].to_string()
            //                         )));
            //                     }
            //                 }
            //                 nir::TypeHintKind::List(_) => todo!(),
            //                 nir::TypeHintKind::Fn(_, _) => todo!(),
            //                 nir::TypeHintKind::Unit => Type::Unit,
            //             };
            //             self.constraints.push(Constraint::Equal(var, field_type));
            //             inferred_fields.push((Ident::new(*field.name(), *field.span()), var));
            //             // tmp_ctx = self.ctx.union(tmp_ctx);
            //             // tmp_ctx.extend(*field.name(), Scheme::new(vec![], field_type.clone()));
            //         }
            //         let ty = Type::Record(
            //             *dt.name().id(),
            //             inferred_fields
            //                 .iter()
            //                 .map(|(k, v)| (*k.name(), v.clone()))
            //                 .collect(),
            //         );

            //         self.ctx
            //             .extend(*dt.name().id(), Scheme::new(vec![], ty.clone()));

            //         Ok(Decl::new(
            //             DeclKind::DataType(DataType::new(
            //                 UniqueIdent::new(*dt.name().id(), *dt.name().span()),
            //                 DataTypeKind::Record {
            //                     fields: solved_fields,
            //                 },
            //                 ty.clone(),
            //                 *decl.span(),
            //             )),
            //             ty,
            //             *decl.span(),
            //         ))
            //     }
            // },
            nir::DeclKind::Let { name, expr } => {
                let solved_expr = self.infer_expr(expr)?;
                let scheme = solved_expr.ty().generalize(&self.ctx);
                self.ctx.extend(name.id(), scheme);

                Ok(Decl::new(
                    DeclKind::Let {
                        name: *name,
                        expr: solved_expr.clone(),
                    },
                    solved_expr.ty(),
                    decl.span(),
                ))
            }
            nir::DeclKind::Fn { name, params, expr } => {
                let mut param_types = vec![];

                for param in params {
                    let param_type = Type::Var(TyVar::fresh());
                    param_types.push(param_type.clone());
                    self.ctx.extend(param.id(), Scheme::new(vec![], param_type));
                }

                let fun_ty_var = Type::Var(TyVar::fresh());

                self.ctx
                    .extend(name.id(), Scheme::new(vec![], fun_ty_var.clone()));
                let solved_expr = self.infer_expr(expr)?;

                self.constraints.push(Constraint::Equal(
                    fun_ty_var.clone(),
                    Type::Lambda(param_types, Box::new(solved_expr.ty())),
                ));

                Ok(Decl::new(
                    DeclKind::Fn {
                        name: *name,
                        params: params.clone(),
                        expr: solved_expr,
                    },
                    fun_ty_var,
                    decl.span(),
                ))
            }
        }
    }

    fn infer_expr(&mut self, expr: &nir::Expr) -> InferResult<Expr> {
        match expr.kind() {
            nir::ExprKind::Lit(lit) => match *lit {
                nir::Lit::Int(n) => Ok(Expr::new(
                    ExprKind::Lit(Lit::Int(n)),
                    Type::Int,
                    expr.span(),
                )),
                nir::Lit::Bool(b) => Ok(Expr::new(
                    ExprKind::Lit(Lit::Bool(b)),
                    Type::Bool,
                    expr.span(),
                )),
                nir::Lit::String(s) => Ok(Expr::new(
                    ExprKind::Lit(Lit::String(s)),
                    Type::String,
                    expr.span(),
                )),
            },
            nir::ExprKind::Ident(name) => {
                if let Some(scm) = self.ctx.get(&name.id()) {
                    let ty = scm.instantiate();
                    Ok(Expr::new(ExprKind::Ident(*name), ty, expr.span()))
                } else {
                    Err(TypeError::from(format!(
                        "unbound variable: {:?} - \"{}\"",
                        expr,
                        self.src[name.span()].to_string()
                    )))
                }
            }
            nir::ExprKind::Apply { fun, args } => {
                let solved_fun = self.infer_expr(fun)?;
                let mut solved_args = vec![];
                let mut arg_types = vec![];

                for arg in args {
                    let solved_arg = self.infer_expr(arg)?;
                    solved_args.push(solved_arg.clone());
                    arg_types.push(solved_arg.ty());
                }

                let ty_ret = Type::Var(TyVar::fresh());
                self.constraints.push(Constraint::Equal(
                    solved_fun.ty(),
                    Type::Lambda(arg_types, Box::new(ty_ret.clone())),
                ));

                Ok(Expr::new(
                    ExprKind::Apply {
                        fun: solved_fun,
                        args: solved_args,
                    },
                    ty_ret,
                    expr.span(),
                ))
            }
            nir::ExprKind::Let { name, expr, body } => {
                let solved_expr = self.infer_expr(expr)?;
                let scheme = solved_expr.ty().generalize(&self.ctx);

                self.ctx.extend(name.id(), scheme);
                let solved_body = self.infer_expr(body)?;
                Ok(Expr::new(
                    ExprKind::Let {
                        name: *name,
                        expr: solved_expr,
                        body: solved_body.clone(),
                    },
                    solved_body.ty(),
                    expr.span(),
                ))
            }
            nir::ExprKind::Fn {
                name,
                params,
                expr,
                body,
            } => {
                let mut param_types = vec![];

                for param in params {
                    let param_type = Type::Var(TyVar::fresh());
                    param_types.push(param_type.clone());
                    self.ctx.extend(param.id(), Scheme::new(vec![], param_type));
                }

                let fun_ty_var = Type::Var(TyVar::fresh());

                self.ctx
                    .extend(name.id(), Scheme::new(vec![], fun_ty_var.clone()));
                let solved_expr = self.infer_expr(expr)?;
                let solved_body = self.infer_expr(body)?;

                self.constraints.push(Constraint::Equal(
                    fun_ty_var.clone(),
                    Type::Lambda(param_types, Box::new(solved_expr.ty())),
                ));

                Ok(Expr::new(
                    ExprKind::Fn {
                        name: *name,
                        params: params.clone(),
                        expr: solved_expr,
                        body: solved_body.clone(),
                    },
                    solved_body.ty(),
                    expr.span(),
                ))
            }
            nir::ExprKind::If { cond, then, else_ } => {
                let solved_cond = self.infer_expr(cond)?;
                let solved_then = self.infer_expr(then)?;
                let solved_else_ = self.infer_expr(else_)?;

                self.constraints
                    .push(Constraint::Equal(solved_cond.ty(), Type::Bool));
                self.constraints
                    .push(Constraint::Equal(solved_then.ty(), solved_else_.ty()));

                Ok(Expr::new(
                    ExprKind::If {
                        cond: solved_cond,
                        then: solved_then.clone(),
                        else_: solved_else_,
                    },
                    solved_then.ty(),
                    expr.span(),
                ))
            }
            nir::ExprKind::Lambda { params, expr } => {
                let mut param_types = vec![];

                for param in params {
                    let param_type = Type::Var(TyVar::fresh());
                    param_types.push(param_type.clone());
                    self.ctx.extend(param.id(), Scheme::new(vec![], param_type));
                }

                let solved_expr = self.infer_expr(expr)?;
                let fun_ty = Type::Lambda(param_types, Box::new(solved_expr.ty()));

                Ok(Expr::new(
                    ExprKind::Lambda {
                        params: params.clone(),
                        expr: solved_expr,
                    },
                    fun_ty,
                    expr.span(),
                ))
            }
            nir::ExprKind::List(elems) => {
                let mut solved_elems = vec![];

                for elem in elems {
                    let solved_elem = self.infer_expr(elem)?;
                    solved_elems.push(solved_elem);
                }

                let list_ty = Type::List(Box::new(Type::Var(TyVar::fresh())));

                for elem in solved_elems.iter() {
                    self.constraints
                        .push(Constraint::Equal(list_ty.clone(), elem.ty()));
                }

                Ok(Expr::new(
                    ExprKind::List(List::from(solved_elems)),
                    list_ty,
                    expr.span(),
                ))
            }
            nir::ExprKind::Record { name, fields } => {
                // if let Some(ident) = name {
                //     if let Some(scm) = self.ctx.get(ident.id()) {
                //         todo!()
                //     } else {
                //         return Err(TypeError::from(format!(
                //             "unbound type: {:?} - \"{}\"",
                //             ident,
                //             self.src[*ident.span()].to_string()
                //         )));
                //     }
                // }
                todo!()
            }
            nir::ExprKind::Unit => Ok(Expr::new(ExprKind::Unit, Type::Unit, expr.span())),
            _ => todo!(),
        }
    }
}
