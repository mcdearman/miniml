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
            "gt", "gte", "println",
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

    pub fn resolve(&mut self, ast: &ast::Root) -> (Option<Root>, Vec<ResError>) {
        let mut decls = Vec::new();
        let mut errors = Vec::new();
        for decl in ast.decls {
            match self.resolve_decl(&decl) {
                Ok(decl) => decls.push(decl),
                Err(e) => errors.push(e),
            }
        }
        if decls.is_empty() {
            (None, errors)
        } else {
            (
                Some(Root {
                    decls,
                    span: ast.span,
                }),
                errors,
            )
        }
    }

    fn resolve_decl(&mut self, decl: &ast::Decl) -> ResResult<Decl> {
        match decl.kind {
            ast::DeclKind::Let(ident, expr) => {
                self.env.push();
                let res_name =
                    ScopedIdent::new(self.env.define(ident.key()), ident.key(), ident.span());
                let res_expr = self.resolve_expr(&expr)?;
                self.env.pop();
                Ok(Decl {
                    kind: DeclKind::Let(res_name, res_expr),
                    span: decl.span,
                })
            }
            ast::DeclKind::Fn(ident, params, fn_expr) => {
                self.env.push();
                let res_name = self.env.define(ident.key());
                let res_params = params
                    .iter()
                    .map(|p| ScopedIdent::new(self.env.define(p.key()), p.key(), p.span()))
                    .collect();
                let res_expr = self.resolve_expr(&fn_expr)?;
                self.env.pop();
                Ok(Decl {
                    kind: DeclKind::Fn(
                        ScopedIdent::new(res_name, ident.key(), ident.span()),
                        res_params,
                        res_expr,
                    ),
                    span: decl.span,
                })
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
            },
            ast::ExprKind::Var(ident) => {
                if let Some(name) = self.env.find(&ident.key()) {
                    Ok(Expr::new(
                        ExprKind::Var(ScopedIdent::new(name, ident.key(), expr.span().clone())),
                        expr.span().clone(),
                    ))
                } else {
                    Err(ResError::new(
                        ResErrorKind::UnboundName(ident.key()),
                        expr.span(),
                    ))
                }
            }
            ast::ExprKind::Lambda(params, fn_expr) => {
                self.env.push();

                let res_params = params
                    .iter()
                    .map(|p| ScopedIdent::new(self.env.define(p.key()), p.key(), p.span()))
                    .collect();

                let res_expr = self.resolve_expr(fn_expr)?;
                self.env.pop();

                Ok(Expr::new(
                    ExprKind::Lambda(res_params, res_expr),
                    expr.span(),
                ))
            }
            ast::ExprKind::Apply(fun, args) => Ok(Expr::new(
                ExprKind::Apply(
                    self.resolve_expr(fun)?,
                    args.iter()
                        .map(|arg| self.resolve_expr(arg))
                        .collect::<ResResult<Vec<Expr>>>()?,
                ),
                expr.span(),
            )),
            ast::ExprKind::UnaryOp(op, op_expr) => {
                let name = InternedString::from(*op);
                let id = self
                    .get_builtin(name)
                    .ok_or(ResError::new(ResErrorKind::UnboundBuiltIn(name), op.span()))?;
                let ident = ScopedIdent::new(id, name, op.span());
                Ok(Expr::new(
                    ExprKind::Apply(
                        Expr::new(ExprKind::Var(ident.clone()), op.span()),
                        vec![self.resolve_expr(op_expr)?],
                    ),
                    expr.span(),
                ))
            }
            ast::ExprKind::BinaryOp(op, lhs, rhs) => {
                let name = InternedString::from(*op);
                let id = self
                    .get_builtin(name)
                    .ok_or(ResError::new(ResErrorKind::UnboundBuiltIn(name), op.span()))?;
                let ident = ScopedIdent::new(id, name, op.span());
                Ok(Expr::new(
                    ExprKind::Apply(
                        Expr::new(ExprKind::Var(ident.clone()), op.span()),
                        vec![self.resolve_expr(lhs)?, self.resolve_expr(rhs)?],
                    ),
                    expr.span(),
                ))
            }
            ast::ExprKind::Or(lhs, rhs) => Ok(Expr::new(
                ExprKind::Or(self.resolve_expr(lhs)?, self.resolve_expr(rhs)?),
                expr.span(),
            )),
            ast::ExprKind::And(lhs, rhs) => Ok(Expr::new(
                ExprKind::And(self.resolve_expr(lhs)?, self.resolve_expr(rhs)?),
                expr.span(),
            )),
            // ast::ExprKind::If { cond, then, else_ } => Ok(Expr::new(
            //     ExprKind::If {
            //         cond: self.resolve_expr(&cond)?,
            //         then: self.resolve_expr(&then)?,
            //         else_: self.resolve_expr(&else_)?,
            //     },
            //     expr.span(),
            // )),
            ast::ExprKind::Let(name, let_expr, body) => {
                let res_expr = self.resolve_expr(&let_expr)?;
                self.env.push();
                let res_name =
                    ScopedIdent::new(self.env.define(name.key()), name.key(), name.span());
                let res_body = self.resolve_expr(&body)?;
                self.env.pop();

                Ok(Expr::new(
                    ExprKind::Let(res_name, res_expr, res_body),
                    expr.span(),
                ))
            }
            ast::ExprKind::Fn(name, params, fn_expr, body) => {
                self.env.push();
                let res_name = self.env.define(name.key());
                let res_params = params
                    .iter()
                    .map(|p| ScopedIdent::new(self.env.define(p.key()), p.key(), p.span()))
                    .collect();
                let res_expr = self.resolve_expr(fn_expr)?;
                let res_body = self.resolve_expr(body)?;
                self.env.pop();

                Ok(Expr::new(
                    ExprKind::Fn(
                        ScopedIdent::new(res_name, name.key(), name.span()),
                        res_params,
                        res_expr,
                        res_body,
                    ),
                    expr.span(),
                ))
            }
            ast::ExprKind::Unit => Ok(Expr::new(ExprKind::Unit, expr.span())),
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
