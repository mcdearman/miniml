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
use itertools::Itertools;
use log::trace;
use std::{cell::RefCell, collections::HashMap, rc::Rc};

#[derive(Debug)]
pub struct Resolver {
    builtins: HashMap<UniqueId, InternedString>,
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
        Self { builtins }
    }

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

    pub fn resolve(
        &mut self,
        env: Rc<RefCell<Env>>,
        root: &ast::Root,
    ) -> (Option<Root>, Vec<ResError>) {
        let mut errors = vec![];
        let mut decls = vec![];
        for decl in root.decls() {
            match self.resolve_decl(env.clone(), decl) {
                Ok(d) => {
                    trace!("env: {:#?}", env.borrow());
                    decls.push(d);
                    // println!("decls: {:#?}", decls);
                }
                Err(err) => {
                    trace!("env: {:#?}", env.borrow());
                    errors.push(err)
                }
            }
        }
        if decls.is_empty() {
            (None, errors)
        } else {
            (Some(Root::new(decls, root.span().clone())), errors)
        }
    }

    fn resolve_decl(&mut self, env: Rc<RefCell<Env>>, decl: &ast::Decl) -> ResResult<Decl> {
        trace!("decl env: {:#?}", env.borrow());
        match decl.kind() {
            ast::DeclKind::DataType(dt) => {
                let name = ScopedIdent::new(
                    env.borrow_mut().define(dt.name().name().clone()),
                    *dt.span(),
                );
                match dt.kind() {
                    ast::DataTypeKind::Record { fields } => Ok(Decl::new(
                        DeclKind::DataType(DataType::new(
                            name,
                            DataTypeKind::Record {
                                fields: fields
                                    .into_iter()
                                    .map(|(name, hint)| {
                                        self.resolve_type(env.clone(), hint).map(|ty| {
                                            (
                                                Ident::new(
                                                    name.name().clone(),
                                                    name.span().clone(),
                                                ),
                                                ty,
                                            )
                                        })
                                    })
                                    .try_collect()?,
                            },
                            decl.span().clone(),
                        )),
                        decl.span().clone(),
                    )),
                }
            }
            ast::DeclKind::Let { name, expr } => {
                let let_env = Env::new_with_parent(env.clone());
                let expr = self.resolve_expr(let_env.clone(), expr)?;
                let name =
                    ScopedIdent::new(env.borrow_mut().define(name.name().clone()), name.span());
                Ok(Decl::new(DeclKind::Let { name, expr }, decl.span().clone()))
            }
            ast::DeclKind::Fn { name, params, expr } => {
                let fn_env = Env::new_with_parent(env.clone());
                let name =
                    ScopedIdent::new(env.borrow_mut().define(name.name().clone()), name.span());
                let params = params
                    .iter()
                    .map(|p| {
                        ScopedIdent::new(fn_env.borrow_mut().define(p.name().clone()), p.span())
                    })
                    .collect();
                let expr = self.resolve_expr(fn_env.clone(), expr)?;
                Ok(Decl::new(
                    DeclKind::Fn { name, params, expr },
                    decl.span().clone(),
                ))
            }
        }
    }

    fn resolve_expr(&mut self, env: Rc<RefCell<Env>>, expr: &ast::Expr) -> ResResult<Expr> {
        match expr.kind() {
            ast::ExprKind::Lit(l) => match l {
                ast::Lit::Int(n) => Ok(Expr::new(ExprKind::Lit(Lit::Int(n.clone())), *expr.span())),
                ast::Lit::Bool(b) => {
                    Ok(Expr::new(ExprKind::Lit(Lit::Bool(*b)), expr.span().clone()))
                }
                ast::Lit::String(s) => Ok(Expr::new(
                    ExprKind::Lit(Lit::String(s.clone())),
                    expr.span().clone(),
                )),
            },
            ast::ExprKind::Ident(ident) => {
                if let Some(name) = env.borrow().find(ident.name()) {
                    Ok(Expr::new(
                        ExprKind::Ident(ScopedIdent::new(name, expr.span().clone())),
                        expr.span().clone(),
                    ))
                } else {
                    Err(ResError::new(
                        ResErrorKind::UnboundName(*ident.name()),
                        *expr.span(),
                    ))
                }
            }
            ast::ExprKind::Apply { fun, args } => Ok(Expr::new(
                ExprKind::Apply {
                    fun: self.resolve_expr(env.clone(), fun)?,
                    args: args
                        .iter()
                        .map(|a| self.resolve_expr(env.clone(), a))
                        .collect::<ResResult<Vec<Expr>>>()?,
                },
                *expr.span(),
            )),
            ast::ExprKind::Unary { op, expr } => {
                let id = self
                    .get_builtin(InternedString::from(*op))
                    .ok_or(ResError::new(
                        ResErrorKind::UnboundBuiltIn(InternedString::from(*op)),
                        *op.span(),
                    ))?;
                let ident = ScopedIdent::new(id, *op.span());
                Ok(Expr::new(
                    ExprKind::Apply {
                        fun: Expr::new(ExprKind::Ident(ident.clone()), *op.span()),
                        args: vec![self.resolve_expr(env.clone(), expr)?],
                    },
                    *expr.span(),
                ))
            }
            ast::ExprKind::Binary { op, lhs, rhs } => {
                let id = self
                    .get_builtin(InternedString::from(*op))
                    .ok_or(ResError::new(
                        ResErrorKind::UnboundBuiltIn(InternedString::from(*op)),
                        *op.span(),
                    ))?;
                let ident = ScopedIdent::new(id, *op.span());
                Ok(Expr::new(
                    ExprKind::Apply {
                        fun: Expr::new(ExprKind::Ident(ident.clone()), *op.span()),
                        args: vec![
                            self.resolve_expr(env.clone(), lhs)?,
                            self.resolve_expr(env.clone(), rhs)?,
                        ],
                    },
                    *op.span(),
                ))
            }
            ast::ExprKind::If { cond, then, else_ } => Ok(Expr::new(
                ExprKind::If {
                    cond: self.resolve_expr(env.clone(), cond)?,
                    then: self.resolve_expr(env.clone(), then)?,
                    else_: self.resolve_expr(env.clone(), else_)?,
                },
                *expr.span(),
            )),
            ast::ExprKind::Let { name, expr, body } => {
                let let_env = Env::new_with_parent(env.clone());
                let res_expr = self.resolve_expr(let_env.clone(), expr)?;
                let name =
                    ScopedIdent::new(env.borrow_mut().define(name.name().clone()), *name.span());
                let body = self.resolve_expr(let_env.clone(), body)?;
                Ok(Expr::new(
                    ExprKind::Let {
                        name,
                        expr: res_expr,
                        body,
                    },
                    *expr.span(),
                ))
            }
            ast::ExprKind::Fn {
                name,
                params,
                expr: fn_expr,
                body,
            } => {
                let fn_env = Env::new_with_parent(env.clone());
                let name = ScopedIdent::new(
                    fn_env.borrow_mut().define(name.name().clone()),
                    *name.span(),
                );
                let params = params
                    .iter()
                    .map(|p| {
                        ScopedIdent::new(fn_env.borrow_mut().define(p.name().clone()), *p.span())
                    })
                    .collect();
                let expr = self.resolve_expr(fn_env.clone(), fn_expr)?;
                let body = self.resolve_expr(fn_env.clone(), body)?;
                Ok(Expr::new(
                    ExprKind::Fn {
                        name,
                        params,
                        expr,
                        body,
                    },
                    *fn_expr.span(),
                ))
            }
            ast::ExprKind::Lambda { params, expr } => {
                let lam_env = Env::new_with_parent(env.clone());
                let params = params
                    .iter()
                    .map(|p| {
                        ScopedIdent::new(lam_env.borrow_mut().define(p.name().clone()), *p.span())
                    })
                    .collect();
                Ok(Expr::new(
                    ExprKind::Lambda {
                        params,
                        expr: self.resolve_expr(lam_env.clone(), expr)?,
                    },
                    *expr.span(),
                ))
            }
            ast::ExprKind::List(exprs) => Ok(Expr::new(
                ExprKind::List(
                    exprs
                        .iter()
                        .map(|e| self.resolve_expr(env.clone(), e))
                        .collect::<ResResult<Vec<Expr>>>()?,
                ),
                *expr.span(),
            )),
            ast::ExprKind::Record { name, fields } => {
                let fields = fields
                    .iter()
                    .map(|(ident, expr)| {
                        let new_ident = Ident::new(ident.name().clone(), *ident.span());
                        let expr = self.resolve_expr(env.clone(), expr)?;
                        Ok((new_ident, expr))
                    })
                    .collect::<ResResult<Vec<(Ident, Expr)>>>()?;

                match name {
                    Some(name) => {
                        let name = ScopedIdent::new(
                            env.borrow_mut().find(name.name()).ok_or(ResError::new(
                                ResErrorKind::UnboundName(*name.name()),
                                *name.span(),
                            ))?,
                            *name.span(),
                        );
                        Ok(Expr::new(
                            ExprKind::Record {
                                name: Some(name),
                                fields,
                            },
                            *expr.span(),
                        ))
                    }
                    None => Ok(Expr::new(
                        ExprKind::Record { name: None, fields },
                        *expr.span(),
                    )),
                }
            }
            ast::ExprKind::Unit => Ok(Expr::new(ExprKind::Unit, *expr.span())),
        }
    }

    fn resolve_type(&mut self, env: Rc<RefCell<Env>>, ty: &ast::TypeHint) -> ResResult<TypeHint> {
        match ty.kind() {
            ast::TypeHintKind::Int => Ok(TypeHint::new(TypeHintKind::Int, *ty.span())),
            ast::TypeHintKind::Bool => Ok(TypeHint::new(TypeHintKind::Bool, *ty.span())),
            ast::TypeHintKind::String => Ok(TypeHint::new(TypeHintKind::String, *ty.span())),
            ast::TypeHintKind::Ident(ident) => {
                if let Some(name) = env.borrow().find(ident.name()) {
                    Ok(TypeHint::new(
                        TypeHintKind::Ident(ScopedIdent::new(name, *ident.span())),
                        *ty.span(),
                    ))
                } else {
                    Err(ResError::new(
                        ResErrorKind::UnboundName(*ident.name()),
                        *ident.span(),
                    ))
                }
            }
            ast::TypeHintKind::List(ty) => Ok(TypeHint::new(
                TypeHintKind::List(self.resolve_type(env.clone(), ty)?),
                *ty.span(),
            )),
            ast::TypeHintKind::Fn(args, ret) => Ok(TypeHint::new(
                TypeHintKind::Fn(
                    args.iter()
                        .map(|a| self.resolve_type(env.clone(), a))
                        .collect::<ResResult<Vec<TypeHint>>>()?,
                    self.resolve_type(env.clone(), ret)?,
                ),
                *ty.span(),
            )),
            ast::TypeHintKind::Unit => Ok(TypeHint::new(TypeHintKind::Unit, *ty.span())),
        }
    }
}

mod tests {
    // use super::{Env, Resolver};
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
