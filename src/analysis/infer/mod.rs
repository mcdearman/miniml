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
    utils::{
        ident::ScopedIdent, intern::InternedString, list::List, scoped_intern::ScopedInterner,
        span::Span, unique_id::UniqueId,
    },
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

#[derive(Debug, Clone)]
pub struct TypeSolver<'src> {
    src: &'src str,
    nir: nir::Root,
    ctx: Context,
    reg: Registry,
    builtins: HashMap<UniqueId, InternedString>,
    constraints: Vec<Constraint>,
    sub: Substitution,
    scoped_interner: ScopedInterner,
}

impl<'src> TypeSolver<'src> {
    pub fn new(
        src: &'src str,
        nir: nir::Root,
        builtins: HashMap<UniqueId, InternedString>,
        scoped_interner: ScopedInterner,
    ) -> Self {
        Self {
            src,
            nir,
            ctx: Context::from_builtins(&builtins),
            reg: Registry::new(),
            builtins,
            constraints: vec![],
            sub: Substitution::new(),
            scoped_interner,
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
                    println!("unify {:?} and {:?}", t1, t2);
                    self.sub = t1
                        .apply_subst(&self.sub)
                        .unify(&t2.apply_subst(&self.sub))
                        .map(|s| s.compose(&self.sub))
                        .unwrap_or_else(|e| {
                            errors.push(e);
                            self.sub.clone()
                        });

                    // println!("sub: {:#?}", self.sub);

                    self.ctx = self.ctx.apply_subst(&self.sub);
                }
                _ => todo!(),
            }
        }

        // println!("constraints: {:#?}", self.constraints);
        // println!("sub: {:#?}", self.sub);
        self.pretty_print_ctx();

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
            nir::DeclKind::DataType(dt) => match dt.kind() {
                nir::DataTypeKind::Record { fields } => {
                    let mut field_types = vec![];

                    for (name, hint) in fields {
                        let ty = self.hint_to_type(hint.clone())?;
                        field_types.push((name.clone(), ty));
                    }

                    let kind = DataTypeKind::Record {
                        fields: field_types.clone(),
                    };

                    let ty = Type::Record(
                        dt.name().id(),
                        field_types
                            .iter()
                            .map(|(name, ty)| (name.key(), ty.clone()))
                            .collect(),
                    );

                    self.reg.insert(dt.name().id(), ty.generalize(&self.ctx));

                    Ok(Decl::new(
                        DeclKind::DataType(DataType::new(dt.name(), kind, ty.clone(), dt.span())),
                        ty,
                        decl.span(),
                    ))
                }
            },
            nir::DeclKind::Let { name, expr } => {
                self.ctx.push();
                let solved_expr = self.infer_expr(expr)?;
                let scheme = solved_expr.ty().generalize(&self.ctx);
                self.ctx.pop();
                self.ctx.insert(name.id(), scheme);

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
                let fun_ty_var = TyVar::fresh();
                let fun_ty = Type::Var(fun_ty_var);
                self.ctx
                    .insert(name.id(), Scheme::new(vec![fun_ty_var], fun_ty.clone()));

                let mut param_types = vec![];
                self.ctx.push();

                for param in params {
                    let var = TyVar::fresh();
                    let param_type = Type::Var(var);
                    param_types.push(param_type.clone());
                    self.ctx
                        .insert(param.id(), Scheme::new(vec![var], param_type));
                }

                let solved_expr = self.infer_expr(expr)?;
                self.ctx.pop();

                self.constraints.push(Constraint::Equal(
                    fun_ty.clone(),
                    Type::Lambda(param_types, Box::new(solved_expr.ty())),
                ));

                Ok(Decl::new(
                    DeclKind::Fn {
                        name: *name,
                        params: params.clone(),
                        expr: solved_expr,
                    },
                    fun_ty,
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
                nir::Lit::Rational(n) => Ok(Expr::new(
                    ExprKind::Lit(Lit::Rational(n)),
                    Type::Rational,
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

                // println!("constraints: {:#?}", self.constraints);
                // println!("ctx: {:#?}", self.ctx);

                Ok(Expr::new(
                    ExprKind::Apply {
                        fun: solved_fun,
                        args: solved_args,
                    },
                    ty_ret,
                    expr.span(),
                ))
            }
            nir::ExprKind::Or { lhs, rhs } => {
                let solved_lhs = self.infer_expr(lhs)?;
                let solved_rhs = self.infer_expr(rhs)?;

                self.constraints
                    .push(Constraint::Equal(solved_lhs.ty(), Type::Bool));
                self.constraints
                    .push(Constraint::Equal(solved_rhs.ty(), Type::Bool));

                Ok(Expr::new(
                    ExprKind::Or {
                        lhs: solved_lhs,
                        rhs: solved_rhs,
                    },
                    Type::Bool,
                    expr.span(),
                ))
            }
            nir::ExprKind::And { lhs, rhs } => {
                let solved_lhs = self.infer_expr(lhs)?;
                let solved_rhs = self.infer_expr(rhs)?;

                self.constraints
                    .push(Constraint::Equal(solved_lhs.ty(), Type::Bool));
                self.constraints
                    .push(Constraint::Equal(solved_rhs.ty(), Type::Bool));

                Ok(Expr::new(
                    ExprKind::And {
                        lhs: solved_lhs,
                        rhs: solved_rhs,
                    },
                    Type::Bool,
                    expr.span(),
                ))
            }
            nir::ExprKind::Let { name, expr, body } => {
                self.ctx.push();
                let solved_expr = self.infer_expr(expr)?;
                self.ctx.pop();

                self.ctx.push();
                let scheme = solved_expr.ty().generalize(&self.ctx);
                // let scheme = Scheme::new(vec![], solved_expr.ty());
                self.ctx.insert(name.id(), scheme);
                let solved_body = self.infer_expr(body)?;
                self.ctx.pop();

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
                self.ctx.push();
                let fun_ty_var = TyVar::fresh();
                let fun_ty = Type::Var(fun_ty_var.clone());
                self.ctx
                    .insert(name.id(), Scheme::new(vec![], fun_ty.clone()));

                let mut param_types = vec![];

                for param in params {
                    let param_type = Type::Var(TyVar::fresh());
                    param_types.push(param_type.clone());
                    self.ctx.insert(param.id(), Scheme::new(vec![], param_type));
                }

                let solved_expr = self.infer_expr(expr)?;
                let solved_body = self.infer_expr(body)?;
                self.ctx.pop();

                self.constraints.push(Constraint::Equal(
                    fun_ty.clone(),
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
                self.ctx.push();
                let mut param_types = vec![];

                for param in params {
                    let param_type = Type::Var(TyVar::fresh());
                    param_types.push(param_type.clone());
                    self.ctx.insert(param.id(), Scheme::new(vec![], param_type));
                }

                let solved_expr = self.infer_expr(expr)?;
                let fun_ty = Type::Lambda(param_types, Box::new(solved_expr.ty()));
                self.ctx.pop();

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
                if let Some(ident) = name {
                    if let Some(scm) = self.reg.get(&ident.id()) {
                        let ty = scm.instantiate();
                        let mut solved_fields = vec![];

                        for (name, expr) in fields {
                            let solved_expr = self.infer_expr(expr)?;
                            solved_fields.push((name.clone(), solved_expr));
                        }

                        Ok(Expr::new(
                            ExprKind::Record {
                                name: *ident,
                                fields: solved_fields.clone(),
                            },
                            ty,
                            expr.span(),
                        ))
                    } else {
                        Err(TypeError::from(format!(
                            "unbound type: {:?} - \"{}\"",
                            ident,
                            self.src[ident.span()].to_string()
                        )))
                    }
                } else {
                    // infer record type
                    let solved_fields = fields
                        .iter()
                        .map(|(name, expr)| Ok((name.clone(), self.infer_expr(expr)?)))
                        .collect::<InferResult<Vec<_>>>()?;

                    let mut possible_types = vec![];
                    for (id, scm) in self.reg.iter() {
                        if let Type::Record(_, fields) = scm.instantiate() {
                            if fields
                                .iter()
                                .map(|(k, v)| (k.clone(), v.clone()))
                                .collect::<Vec<_>>()
                                == solved_fields
                                    .iter()
                                    .map(|(k, v)| (k.key(), v.ty()))
                                    .collect::<Vec<_>>()
                            {
                                possible_types.push((id, scm.instantiate()));
                            }
                        }
                    }

                    if possible_types.is_empty() {
                        Err(TypeError::from(format!(
                            "no matching record type for fields: {:?}",
                            solved_fields
                        )))
                    } else if possible_types.len() > 1 {
                        Err(TypeError::from(format!(
                            "ambiguous record type for fields: {:?}",
                            solved_fields
                        )))
                    } else {
                        Ok(Expr::new(
                            ExprKind::Record {
                                name: ScopedIdent::new(
                                    possible_types[0].0.clone(),
                                    *self.scoped_interner.get(*possible_types[0].0).ok_or(
                                        TypeError::from(format!(
                                            "unbound type: {:?}",
                                            possible_types[0].0
                                        )),
                                    )?,
                                    Span::new(expr.span().start(), expr.span().start()),
                                ),
                                fields: solved_fields,
                            },
                            possible_types[0].1.clone(),
                            expr.span(),
                        ))
                    }
                }
            }
            nir::ExprKind::Dot { expr, field } => {
                let solved_expr = self.infer_expr(expr)?;

                if let Type::Record(_, fields) = solved_expr.ty() {
                    if let Some(ty) = fields
                        .iter()
                        .find_or_first(|(k, _)| *k == field.key())
                        .map(|(_, v)| v)
                    {
                        Ok(Expr::new(
                            ExprKind::Dot {
                                expr: solved_expr,
                                field: *field,
                            },
                            ty.clone(),
                            expr.span(),
                        ))
                    } else {
                        Err(TypeError::from(format!(
                            "field not found: {:?} - \"{}\"",
                            field,
                            self.src[field.span()].to_string()
                        )))
                    }
                } else {
                    Err(TypeError::from(format!(
                        "expected record type, found: {:?}",
                        solved_expr.ty()
                    )))
                }
            }
            nir::ExprKind::Unit => Ok(Expr::new(ExprKind::Unit, Type::Unit, expr.span())),
        }
    }

    fn hint_to_type(&self, hint: nir::TypeHint) -> InferResult<Type> {
        match hint.kind() {
            nir::TypeHintKind::Int => Ok(Type::Int),
            nir::TypeHintKind::Bool => Ok(Type::Bool),
            nir::TypeHintKind::String => Ok(Type::String),
            nir::TypeHintKind::Ident(name) => {
                if let Some(scm) = self.reg.get(&name.id()) {
                    Ok(scm.instantiate())
                } else {
                    return Err(TypeError::from(format!(
                        "unbound type: {:?} - \"{}\"",
                        name,
                        self.src[name.span()].to_string()
                    )));
                }
            }
            nir::TypeHintKind::List(list_hint) => self.hint_to_type(list_hint.clone()),
            nir::TypeHintKind::Fn(args, ret) => {
                let mut arg_types = vec![];
                for arg in args {
                    arg_types.push(self.hint_to_type(arg.clone())?);
                }
                Ok(Type::Lambda(
                    arg_types,
                    Box::new(self.hint_to_type(ret.clone())?),
                ))
            }
            nir::TypeHintKind::Unit => Ok(Type::Unit),
        }
    }

    fn pretty_print_ctx(&self) {
        let mut ctx = HashMap::new();
        for (uid, scm) in self.ctx.iter() {
            let name = self.builtins.get(uid).map(|s| s.to_string());
            ctx.insert(name.unwrap_or_else(|| format!("{:?}", uid)), scm.clone());
        }

        println!("ctx: {:#?}", ctx);
    }
}
