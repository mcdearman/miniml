use crate::{intern::InternedString, span::Span, unique_id::UniqueId};
use std::{cell::RefCell, collections::HashMap, fmt::Display, rc::Rc};



#[derive(Debug)]
pub struct Resolver {
    builtins: HashMap<UniqueId, InternedString>,
}

impl Resolver {
    pub fn new(db: Rc<RefCell<Database>>) -> Self {
        Self { db }
    }

    pub fn resolve(
        &mut self,
        env: Rc<RefCell<Env>>,
        root: &parse::Root,
    ) -> (Option<Root>, Vec<ResError>) {
        let mut errors = vec![];
        let mut decls = vec![];
        for decl in root.decls() {
            match self.resolve_decl(env.clone(), decl) {
                Ok(d) => {
                    // trace!("env: {:#?}", env.borrow());
                    println!("resolver env: {:#?}", env.borrow());
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
            (
                Some(Root {
                    decls,
                    span: root.span().clone(),
                }),
                errors,
            )
        }
    }

    fn resolve_decl(&mut self, env: Rc<RefCell<Env>>, decl: &parse::Decl) -> ResResult<Decl> {
        trace!("decl env: {:#?}", env.borrow());
        match decl.kind() {
            parse::DeclKind::Let { name, expr } => match expr.kind() {
                parse::ExprKind::Lambda { .. } => {
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

    fn resolve_expr(&mut self, env: Rc<RefCell<Env>>, expr: &parse::Expr) -> ResResult<Expr> {
        match expr.kind() {
            parse::ExprKind::Lit(l) => match l {
                parse::Lit::Num(n) => {
                    Ok(Expr::new(ExprKind::Lit(Lit::Num(n.clone())), *expr.span()))
                }
                parse::Lit::Bool(b) => {
                    Ok(Expr::new(ExprKind::Lit(Lit::Bool(*b)), expr.span().clone()))
                }
                parse::Lit::String(s) => Ok(Expr::new(
                    ExprKind::Lit(Lit::String(s.clone())),
                    *expr.span(),
                )),
            },
            parse::ExprKind::Ident(ident) => {
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
            parse::ExprKind::Apply { fun, arg } => Ok(Expr::new(
                ExprKind::Apply {
                    fun: self.resolve_expr(env.clone(), fun)?,
                    arg: self.resolve_expr(env.clone(), arg)?,
                },
                *expr.span(),
            )),
            parse::ExprKind::Unary { op, expr } => {
                let id = env.borrow_mut().define_if_absent(InternedString::from(*op));
                let ident = Ident::new(id, *op.span());
                self.db
                    .borrow_mut()
                    .insert_or_get(id, InternedString::from(*op));
                Ok(Expr::new(
                    ExprKind::Apply {
                        fun: Expr::new(ExprKind::Ident(ident.clone()), *op.span()),
                        arg: self.resolve_expr(env.clone(), expr)?,
                    },
                    *expr.span(),
                ))
            }
            parse::ExprKind::Binary { op, lhs, rhs } => {
                let id = env.borrow_mut().define_if_absent(InternedString::from(*op));
                let ident = Ident::new(id, *op.span());
                self.db
                    .borrow_mut()
                    .insert_or_get(id, InternedString::from(*op));
                Ok(Expr::new(
                    ExprKind::Apply {
                        fun: Expr::new(
                            ExprKind::Apply {
                                fun: Expr::new(ExprKind::Ident(ident.clone()), *op.span()),
                                arg: self.resolve_expr(env.clone(), lhs)?,
                            },
                            *op.span(),
                        ),
                        arg: self.resolve_expr(env.clone(), rhs)?,
                    },
                    *expr.span(),
                ))
            }
            parse::ExprKind::If { cond, then, else_ } => Ok(Expr::new(
                ExprKind::If {
                    cond: self.resolve_expr(env.clone(), cond)?,
                    then: self.resolve_expr(env.clone(), then)?,
                    else_: self.resolve_expr(env.clone(), else_)?,
                },
                *expr.span(),
            )),
            parse::ExprKind::Let { name, expr, body } => match expr.kind() {
                parse::ExprKind::Lambda { .. } => {
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
            parse::ExprKind::Lambda { param, expr } => {
                let lam_env = Env::new_with_parent(env.clone());
                let param =
                    Ident::new(env.borrow_mut().define(param.name().clone()), *param.span());
                Ok(Expr::new(
                    ExprKind::Lambda {
                        param,
                        expr: self.resolve_expr(lam_env.clone(), expr)?,
                    },
                    *expr.span(),
                ))
            }
            parse::ExprKind::Unit => Ok(Expr::new(ExprKind::Unit, *expr.span())),
        }
    }
}

mod tests {
    use super::{Env, Resolver};
    use crate::{db::Database, parse::parse};
    use std::{cell::RefCell, rc::Rc};

    fn test_helper(src: &str) -> super::Root {
        if let (Some(ast), errors) = parse(src) {
            let db = Rc::new(RefCell::new(Database::new()));
            let mut resolver = Resolver::new(db.clone());
            let (res, errors) = resolver.resolve(Env::new(), &ast);
            if !errors.is_empty() {
                panic!("resolve error: {:?}", errors);
            }
            res.unwrap()
        } else {
            panic!("parse error");
        }
    }

    #[test]
    fn res_let() {
        insta::assert_debug_snapshot!(test_helper("let x = 1"));
    }

    #[test]
    fn res_let_error() {
        let src = "let x = x";
        if let (Some(ast), errors) = parse(src) {
            let db = Rc::new(RefCell::new(Database::new()));
            let mut r = Resolver::new(db.clone());
            let (_, errors) = r.resolve(Env::new(), &ast);
            assert!(!errors.is_empty());
        } else {
            panic!("parse error");
        }
    }

    #[test]
    fn res_nested() {
        insta::assert_debug_snapshot!(test_helper("let a = let x = 1 in let y = 2 in x + y"));
    }

    // #[test]
    // fn res_fn_def() {
    //     let (ast, errors) = crate::syntax::parse::parse("add x y = x + y");
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
    //     let (ast, errors) = crate::syntax::parse::parse("f x = f x");
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
    //     let (ast, errors) = crate::syntax::parse::parse("let f x = f x in f 1");
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
