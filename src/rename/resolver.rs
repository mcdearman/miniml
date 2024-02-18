use super::{
    env::Env,
    error::{ResError, ResErrorKind, ResResult},
    nir::*,
};
use crate::{
    parse::ast,
    utils::{intern::InternedString, unique_id::UniqueId},
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
            ast::DeclKind::Let { name, expr } => match expr.kind() {
                ast::ExprKind::Lambda { .. } => {
                    let name =
                        Ident::new(env.borrow_mut().define(name.name().clone()), *name.span());
                    let let_env = Env::new_with_parent(env.clone());
                    let expr = self.resolve_expr(let_env.clone(), expr)?;
                    Ok(Decl::new(DeclKind::Let { name, expr }, decl.span().clone()))
                }
                _ => {
                    let let_env = Env::new_with_parent(env.clone());
                    let expr = self.resolve_expr(let_env.clone(), expr)?;
                    let name =
                        Ident::new(env.borrow_mut().define(name.name().clone()), *name.span());
                    Ok(Decl::new(DeclKind::Let { name, expr }, decl.span().clone()))
                }
            },
        }
    }

    fn resolve_expr(&mut self, env: Rc<RefCell<Env>>, expr: &ast::Expr) -> ResResult<Expr> {
        match expr.kind() {
            ast::ExprKind::Lit(l) => match l {
                ast::Lit::Int(n) => Ok(Expr::new(ExprKind::Lit(Lit::Int(n.clone())), *expr.span())),
                ast::Lit::Bool(b) => {
                    Ok(Expr::new(ExprKind::Lit(Lit::Bool(*b)), expr.span().clone()))
                }
            },
            ast::ExprKind::Ident(ident) => {
                if let Some(name) = env.borrow().find(ident.name()) {
                    Ok(Expr::new(
                        ExprKind::Ident(Ident::new(name, expr.span().clone())),
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
                let ident = Ident::new(id, *op.span());
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
                let ident = Ident::new(id, *op.span());
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
            ast::ExprKind::Let { name, expr, body } => match expr.kind() {
                ast::ExprKind::Lambda { .. } => {
                    let name =
                        Ident::new(env.borrow_mut().define(name.name().clone()), *name.span());
                    let let_env = Env::new_with_parent(env.clone());
                    let res_expr = self.resolve_expr(let_env.clone(), &expr)?;
                    let body = self.resolve_expr(let_env.clone(), &body)?;
                    Ok(Expr::new(
                        ExprKind::Let {
                            name,
                            expr: res_expr,
                            body,
                        },
                        *expr.span(),
                    ))
                }
                _ => {
                    let let_env = Env::new_with_parent(env.clone());
                    let res_expr = self.resolve_expr(let_env.clone(), expr)?;
                    let name =
                        Ident::new(env.borrow_mut().define(name.name().clone()), *name.span());
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
            },
            ast::ExprKind::Lambda { params, expr } => {
                let lam_env = Env::new_with_parent(env.clone());
                let params = params
                    .iter()
                    .map(|p| Ident::new(lam_env.borrow_mut().define(p.name().clone()), *p.span()))
                    .collect();
                Ok(Expr::new(
                    ExprKind::Lambda {
                        params,
                        expr: self.resolve_expr(lam_env.clone(), expr)?,
                    },
                    *expr.span(),
                ))
            }
            ast::ExprKind::Unit => Ok(Expr::new(ExprKind::Unit, *expr.span())),
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