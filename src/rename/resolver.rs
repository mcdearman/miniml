use super::{
    env::Env,
    error::{ResError, ResErrorKind, ResResult},
    nir::*,
};
use crate::{
    parse::ast,
    utils::{
        ident::{Ident, ScopedIdent},
        intern::InternedString,
        unique_id::UniqueId,
    },
};
use log::trace;
use std::collections::HashMap;

#[derive(Debug)]
pub struct Resolver {
    builtins: HashMap<UniqueId, InternedString>,
    env: Env,
}

impl Resolver {
    pub fn new() -> Self {
        #[rustfmt::skip]
        let names = vec![
            "not", "neg", "add", "sub", 
            "mul", "div", "rem", "pow",
            "eq", "neq", "lt", "lte", 
            "gt", "gte", "and", "or",
            "println",
        ];

        let mut builtins = HashMap::new();
        for n in names {
            let id = UniqueId::gen();
            builtins.insert(id, InternedString::from(n));
        }

        Self {
            builtins: builtins.clone(),
            env: Env::new_with_builtins(builtins),
        }
    }

    // pub fn set_src(&mut self, src: &'src str) {
    //     self.src = src;
    // }

    pub fn builtins(&self) -> &HashMap<UniqueId, InternedString> {
        &self.builtins
    }

    fn get_builtin(&mut self, name: InternedString) -> Option<UniqueId> {
        for (id, n) in &self.builtins {
            if n == &name {
                return Some(*id);
            }
        }
        None
    }

    pub fn env(&self) -> &Env {
        &self.env
    }

    pub fn resolve(&mut self, ast: ast::Root) -> (Option<Root>, Vec<ResError>) {
        let mut errors = vec![];
        let mut decls = vec![];

        for decl in ast.decls() {
            match self.resolve_decl(&decl) {
                Ok(d) => {
                    trace!("env: {:#?}", self.env);
                    decls.push(d);
                }
                Err(err) => {
                    trace!("env: {:#?}", self.env);
                    errors.push(err)
                }
            }
        }

        if decls.is_empty() {
            (None, errors)
        } else {
            (Some(Root::new(decls, ast.span())), errors)
        }
    }

    fn resolve_decl(&mut self, decl: &ast::Decl) -> ResResult<Decl> {
        match decl.kind() {
            ast::DeclKind::DataType(dt) => {
                let res_name = ScopedIdent::new(self.env.define(dt.ident().key()), dt.span());
                match dt.kind() {
                    ast::DataTypeKind::Record { fields } => {
                        let res_fields = fields
                            .iter()
                            .map(|(name, hint)| {
                                self.resolve_type(hint).map(|ty| (name.clone(), ty))
                            })
                            .collect::<ResResult<Vec<(Ident, TypeHint)>>>()?;
                        Ok(Decl::new(
                            DeclKind::DataType(DataType::new(
                                res_name,
                                DataTypeKind::Record { fields: res_fields },
                                decl.span(),
                            )),
                            decl.span(),
                        ))
                    }
                }
            }
            ast::DeclKind::Let { name, expr } => {
                self.env.push();
                let expr = self.resolve_expr(&expr)?;
                self.env.pop();
                let name = ScopedIdent::new(self.env.define(name.key()), name.span());
                Ok(Decl::new(DeclKind::Let { name, expr }, decl.span()))
            }
            ast::DeclKind::Fn { name, params, expr } => {
                let name = ScopedIdent::new(self.env.define(name.key()), name.span());
                self.env.push();
                let params = params
                    .iter()
                    .map(|p| ScopedIdent::new(self.env.define(p.key()), p.span()))
                    .collect();
                let expr = self.resolve_expr(&expr)?;
                self.env.pop();
                Ok(Decl::new(DeclKind::Fn { name, params, expr }, decl.span()))
            }
        }
    }

    fn resolve_expr(&mut self, expr: &ast::Expr) -> ResResult<Expr> {
        match expr.kind() {
            ast::ExprKind::Lit(l) => match l {
                ast::Lit::Int(n) => Ok(Expr::new(ExprKind::Lit(Lit::Int(n.clone())), expr.span())),
                ast::Lit::Bool(b) => {
                    Ok(Expr::new(ExprKind::Lit(Lit::Bool(*b)), expr.span().clone()))
                }
                ast::Lit::String(s) => Ok(Expr::new(
                    ExprKind::Lit(Lit::String(s.clone())),
                    expr.span().clone(),
                )),
            },
            ast::ExprKind::Ident(ident) => {
                if let Some(name) = self.env.find(&ident.key()) {
                    Ok(Expr::new(
                        ExprKind::Ident(ScopedIdent::new(name, expr.span().clone())),
                        expr.span().clone(),
                    ))
                } else {
                    Err(ResError::new(
                        ResErrorKind::UnboundName(ident.key()),
                        expr.span(),
                    ))
                }
            }
            ast::ExprKind::Apply { fun, args } => Ok(Expr::new(
                ExprKind::Apply {
                    fun: self.resolve_expr(&fun)?,
                    args: args
                        .iter()
                        .map(|arg| self.resolve_expr(&arg))
                        .collect::<ResResult<Vec<Expr>>>()?,
                },
                expr.span(),
            )),
            ast::ExprKind::Unary { op, expr } => {
                let id = self
                    .get_builtin(InternedString::from(*op))
                    .ok_or(ResError::new(
                        ResErrorKind::UnboundBuiltIn(InternedString::from(*op)),
                        op.span(),
                    ))?;
                let ident = ScopedIdent::new(id, op.span());
                Ok(Expr::new(
                    ExprKind::Apply {
                        fun: Expr::new(ExprKind::Ident(ident.clone()), op.span()),
                        args: vec![self.resolve_expr(&expr)?],
                    },
                    expr.span(),
                ))
            }
            ast::ExprKind::Binary { op, lhs, rhs } => {
                let id = self
                    .get_builtin(InternedString::from(*op))
                    .ok_or(ResError::new(
                        ResErrorKind::UnboundBuiltIn(InternedString::from(*op)),
                        op.span(),
                    ))?;
                let ident = ScopedIdent::new(id, op.span());
                Ok(Expr::new(
                    ExprKind::Apply {
                        fun: Expr::new(ExprKind::Ident(ident.clone()), op.span()),
                        args: vec![self.resolve_expr(&lhs)?, self.resolve_expr(&rhs)?],
                    },
                    op.span(),
                ))
            }
            ast::ExprKind::If { cond, then, else_ } => Ok(Expr::new(
                ExprKind::If {
                    cond: self.resolve_expr(&cond)?,
                    then: self.resolve_expr(&then)?,
                    else_: self.resolve_expr(&else_)?,
                },
                expr.span(),
            )),
            ast::ExprKind::Let { name, expr, body } => {
                self.env.push();
                let res_expr = self.resolve_expr(&expr)?;
                let res_name = ScopedIdent::new(self.env.define(name.key()), name.span());
                let res_body = self.resolve_expr(&body)?;
                self.env.pop();
                Ok(Expr::new(
                    ExprKind::Let {
                        name: res_name,
                        expr: res_expr,
                        body: res_body,
                    },
                    expr.span(),
                ))
            }
            ast::ExprKind::Fn {
                name,
                params,
                expr: fn_expr,
                body,
            } => {
                let res_name = ScopedIdent::new(self.env.push_and_define(name.key()), name.span());
                let res_params = params
                    .iter()
                    .map(|p| ScopedIdent::new(self.env.define(p.key()), p.span()))
                    .collect();

                let res_expr = self.resolve_expr(&fn_expr)?;
                let res_body = self.resolve_expr(&body)?;
                self.env.pop();

                Ok(Expr::new(
                    ExprKind::Fn {
                        name: res_name,
                        params: res_params,
                        expr: res_expr,
                        body: res_body,
                    },
                    fn_expr.span(),
                ))
            }
            ast::ExprKind::Lambda { params, expr } => {
                self.env.push();
                let res_params = params
                    .iter()
                    .map(|p| ScopedIdent::new(self.env.define(p.key()), p.span()))
                    .collect();
                let res_expr = self.resolve_expr(&expr)?;
                self.env.pop();

                Ok(Expr::new(
                    ExprKind::Lambda {
                        params: res_params,
                        expr: res_expr,
                    },
                    expr.span(),
                ))
            }
            ast::ExprKind::List(exprs) => Ok(Expr::new(
                ExprKind::List(
                    exprs
                        .iter()
                        .map(|e| self.resolve_expr(e))
                        .collect::<ResResult<Vec<Expr>>>()?,
                ),
                expr.span(),
            )),
            ast::ExprKind::Record { name, fields } => {
                let fields = fields
                    .iter()
                    .map(|(ident, expr)| {
                        let expr = self.resolve_expr(expr)?;
                        Ok((ident.clone(), expr))
                    })
                    .collect::<ResResult<Vec<(Ident, Expr)>>>()?;

                match name {
                    Some(name) => {
                        let name = ScopedIdent::new(
                            self.env.find(&name.key()).ok_or(ResError::new(
                                ResErrorKind::UnboundName(name.key()),
                                name.span(),
                            ))?,
                            name.span(),
                        );
                        Ok(Expr::new(
                            ExprKind::Record {
                                name: Some(name),
                                fields,
                            },
                            expr.span(),
                        ))
                    }
                    None => Ok(Expr::new(
                        ExprKind::Record { name: None, fields },
                        expr.span(),
                    )),
                }
            }
            ast::ExprKind::Unit => Ok(Expr::new(ExprKind::Unit, expr.span())),
        }
    }

    fn resolve_type(&mut self, ty: &ast::TypeHint) -> ResResult<TypeHint> {
        match ty.kind() {
            ast::TypeHintKind::Int => Ok(TypeHint::new(TypeHintKind::Int, ty.span())),
            ast::TypeHintKind::Bool => Ok(TypeHint::new(TypeHintKind::Bool, ty.span())),
            ast::TypeHintKind::String => Ok(TypeHint::new(TypeHintKind::String, ty.span())),
            ast::TypeHintKind::Ident(ident) => {
                if let Some(name) = self.env.find(&ident.key()) {
                    Ok(TypeHint::new(
                        TypeHintKind::Ident(ScopedIdent::new(name, ident.span())),
                        ty.span(),
                    ))
                } else {
                    Err(ResError::new(
                        ResErrorKind::UnboundName(ident.key()),
                        ident.span(),
                    ))
                }
            }
            ast::TypeHintKind::List(ty) => Ok(TypeHint::new(
                TypeHintKind::List(self.resolve_type(ty)?),
                ty.span(),
            )),
            ast::TypeHintKind::Fn(args, ret) => Ok(TypeHint::new(
                TypeHintKind::Fn(
                    args.iter()
                        .map(|arg_ty| self.resolve_type(arg_ty))
                        .collect::<ResResult<Vec<TypeHint>>>()?,
                    self.resolve_type(ret)?,
                ),
                ty.span(),
            )),
            ast::TypeHintKind::Unit => Ok(TypeHint::new(TypeHintKind::Unit, ty.span())),
        }
    }
}

mod tests {
    // use crate::{ast::parse, db::Database};
    // use std::{cell::RefCell, rc::Rc};

    // fn test_helper(src: &str) -> super::Root {
    //     if let (Some(ast), errors) = parse(src) {
    //         let db = Rc::new(RefCell::new(Database::new()));
    //         let mut resolver = Resolver::new(db.clone());
    //         let (res, errors) = resolver.resolve(Env::new(), &ast);
    //         if !errors.is_empty() {
    //             panic!("resolve error: {:?}", errors);
    //         }
    //         res.unwrap()
    //     } else {
    //         panic!("parse error");
    //     }
    // }

    // #[test]
    // fn res_let() {
    //     insta::assert_debug_snapshot!(test_helper("let x = 1"));
    // }

    // #[test]
    // fn res_let_error() {
    //     let src = "let x = x";
    //     if let (Some(ast), errors) = parse(src) {
    //         let db = Rc::new(RefCell::new(Database::new()));
    //         let mut r = Resolver::new(db.clone());
    //         let (_, errors) = r.resolve(Env::new(), &ast);
    //         assert!(!errors.is_empty());
    //     } else {
    //         panic!("parse error");
    //     }
    // }

    // #[test]
    // fn res_nested() {
    //     insta::assert_debug_snapshot!(test_helper("let a = let x = 1 in let y = 2 in x + y"));
    // }

    // #[test]
    // fn res_fn_def() {
    //     let (ast, errors) = crate::syntax::ast::parse("add x y = x + y");
    //     if !errors.is_empty() {
    //         panic!("parse error: {:?}", errors);
    //     }
    //     let (res, errors) = super::resolve(&ast.unwrap());
    //     if !errors.is_empty() {
    //         panic!("resolve error: {:?}", errors);
    //     }
    //     insta::assert_debug_snapshot!(res);
    // }

    // #[test]
    // fn res_rec_fn_def() {
    //     let (ast, errors) = crate::syntax::ast::parse("f x = f x");
    //     if !errors.is_empty() {
    //         panic!("parse error: {:?}", errors);
    //     }
    //     let (res, errors) = super::resolve(&ast.unwrap());
    //     if !errors.is_empty() {
    //         panic!("resolve error: {:?}", errors);
    //     }
    //     insta::assert_debug_snapshot!(res);
    // }

    // #[test]
    // fn res_rec_let() {
    //     let (ast, errors) = crate::syntax::ast::parse("let f x = f x in f 1");
    //     if !errors.is_empty() {
    //         panic!("parse error: {:?}", errors);
    //     }
    //     let (res, errors) = super::resolve(&ast.unwrap());
    //     if !errors.is_empty() {
    //         panic!("resolve error: {:?}", errors);
    //     }
    //     insta::assert_debug_snapshot!(res);
    // }
}
