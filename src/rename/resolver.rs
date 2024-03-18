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

    pub fn resolve_expr(&mut self, expr: &ast::Expr) -> ResResult<Expr> {
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
            ast::ExprKind::Abs(param, fn_expr) => {
                self.env.push();
                let res_param =
                    ScopedIdent::new(self.env.define(param.key()), param.key(), param.span());
                let res_expr = self.resolve_expr(fn_expr)?;
                self.env.pop();

                Ok(Expr::new(ExprKind::Abs(res_param, res_expr), expr.span()))
            }
            ast::ExprKind::App(fun, arg) => Ok(Expr::new(
                ExprKind::App(self.resolve_expr(fun)?, self.resolve_expr(arg)?),
                expr.span(),
            )),
            ast::ExprKind::UnaryOp(op, op_expr) => {
                let name = InternedString::from(*op);
                let id = self
                    .get_builtin(name)
                    .ok_or(ResError::new(ResErrorKind::UnboundBuiltIn(name), op.span()))?;
                let ident = ScopedIdent::new(id, name, op.span());
                Ok(Expr::new(
                    ExprKind::App(
                        Expr::new(ExprKind::Var(ident.clone()), op.span()),
                        self.resolve_expr(op_expr)?,
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
                    ExprKind::App(
                        Expr::new(
                            ExprKind::App(
                                Expr::new(ExprKind::Var(ident.clone()), op.span()),
                                self.resolve_expr(&lhs)?,
                            ),
                            lhs.span().extend(rhs.span()),
                        ),
                        self.resolve_expr(rhs)?,
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
            ast::ExprKind::Let(name, rec, let_expr, body) => {
                if *rec {
                    let res_name = ScopedIdent::new(
                        self.env.push_and_define(name.key()),
                        name.key(),
                        name.span(),
                    );

                    let res_expr = self.resolve_expr(let_expr)?;
                    let res_body = self.resolve_expr(body)?;
                    self.env.pop();

                    Ok(Expr::new(
                        ExprKind::Let(res_name, true, res_expr, res_body),
                        expr.span(),
                    ))
                } else {
                    let res_expr = self.resolve_expr(&let_expr)?;
                    self.env.push();
                    let res_name =
                        ScopedIdent::new(self.env.define(name.key()), name.key(), name.span());
                    let res_body = self.resolve_expr(&body)?;
                    self.env.pop();

                    Ok(Expr::new(
                        ExprKind::Let(res_name, false, res_expr, res_body),
                        expr.span(),
                    ))
                }
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
