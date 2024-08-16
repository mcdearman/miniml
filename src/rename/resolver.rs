use super::{
    env::Env,
    error::{ResError, ResErrorKind, ResResult},
    nir::*,
    res_id::ResId,
    scoped_ident::ScopedIdent,
};
use crate::{
    parse::ast,
    utils::{ident::Ident, intern::InternedString},
};
use std::collections::HashMap;

#[derive(Debug)]
pub struct Resolver {
    builtins: HashMap<ResId, InternedString>,
    env: Env,
}

impl Resolver {
    pub fn new() -> Self {
        #[rustfmt::skip]
        const BUILTINS: [&str; 16] = [
            "__println__", 
            "__neg__", "__not__", 

            "__add__", "__sub__", 
            "__mul__", "__div__", 

            "__rem__", "__pow__", 
            "__eq__", "__neq__",

            "__lt__", "__lte__", 
            "__gt__", "__gte__", 

            "__pair__",
        ];

        let mut builtins = HashMap::new();
        for n in BUILTINS {
            let id = ResId::gen();
            builtins.insert(id, InternedString::from(n));
        }

        Self {
            builtins: builtins.clone(),
            env: Env::new_with_builtins(builtins),
        }
    }

    pub fn builtins(&self) -> &HashMap<ResId, InternedString> {
        &self.builtins
    }

    pub fn env(&self) -> &Env {
        &self.env
    }

    pub fn resolve(&mut self, ast: &ast::Root) -> (Option<Root>, Vec<ResError>) {
        let mut decls = Vec::new();
        let mut errors = Vec::new();

        for decl in &ast.decls {
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
        match &decl.kind {
            ast::DeclKind::Def(pattern, expr) => {
                if expr.is_lambda() {
                    match pattern.kind.as_ref() {
                        ast::PatternKind::Ident(ident, _) => {
                            let res_pat = Pattern::new(
                                PatternKind::Ident(
                                    ScopedIdent::new(ident.name, 0, ident.span),
                                    None,
                                ),
                                ident.span,
                            );
                            let res_expr = self.resolve_expr(&expr)?;
                            Ok(Decl {
                                kind: DeclKind::Def(res_pat, true, res_expr),
                                span: decl.span,
                            })
                        }
                        _ => Err(ResError::new(ResErrorKind::InvalidDefPattern, pattern.span)),
                    }
                } else {
                    let res_expr = self.resolve_expr(&expr)?;
                    let (res_pat, _) = self.resolve_pattern(&pattern, true)?;
                    Ok(Decl {
                        kind: DeclKind::Def(res_pat, false, res_expr),
                        span: decl.span,
                    })
                }
            }
            ast::DeclKind::Fn(ident, params, fn_expr) => {
                let mut res_params = vec![];
                let mut idents = vec![];
                for p in params {
                    match self.resolve_pattern(p, false) {
                        Ok((p, i)) => {
                            res_params.push(p);
                            idents.extend(i);
                        }
                        Err(e) => {
                            for ident in idents {
                                self.env.pop(ident.name);
                            }
                            return Err(e);
                        }
                    }
                }

                let res_expr = self.resolve_expr(&fn_expr)?;
                for ident in idents {
                    self.env.pop(ident.name);
                }

                // fold params into lambda expr
                let lam = res_params.into_iter().rev().fold(res_expr, |acc, p| {
                    Expr::new(ExprKind::Lambda(p, acc), decl.span)
                });

                Ok(Decl::new(
                    DeclKind::Def(
                        Pattern::new(
                            PatternKind::Ident(ScopedIdent::new(ident.name, 0, ident.span), None),
                            ident.span,
                        ),
                        true,
                        lam,
                    ),
                    decl.span,
                ))
            }
            ast::DeclKind::FnMatch(arms) => {
                // if arms.is_empty() {
                //     return Err(ResError::new(ResErrorKind::EmptyFnMatch, decl.span));
                // }

                // let name = arms[0].0;
                // let res_name = self.env.define(name.str);

                // let param_len = arms[0].1.len();

                // // let mut res_params = vec![];
                // let mut res_arms = vec![];
                // for (n, params, body) in arms {
                //     if *n != name {
                //         return Err(ResError::new(ResErrorKind::TooManyFnNames, decl.span));
                //     }
                //     if params.len() != param_len {
                //         return Err(ResError::new(ResErrorKind::FnArmParamMismatch, decl.span));
                //     }
                //     self.env.push();
                //     let mut res_params = vec![];
                //     for p in params {
                //         match self.resolve_pattern(p) {
                //             Ok(p) => res_params.push(p),
                //             Err(e) => {
                //                 self.env.pop();
                //                 return Err(e);
                //             }
                //         }
                //     }
                //     let res_body = self.resolve_expr(body)?;
                //     self.env.pop();
                //     res_arms.push((res_params, res_body));
                // }

                // Ok(Decl {
                //     kind: DeclKind::Fn(res_name,
                // })
                todo!()
            }
        }
    }

    fn resolve_expr(&mut self, expr: &ast::Expr) -> ResResult<Expr> {
        match expr.kind.as_ref() {
            ast::ExprKind::Lit(l) => match l {
                ast::Lit::Byte(b) => Ok(Expr::new(ExprKind::Lit(Lit::Byte(*b)), expr.span)),
                ast::Lit::Int(n) => Ok(Expr::new(ExprKind::Lit(Lit::Int(*n)), expr.span)),
                ast::Lit::Rational(r) => Ok(Expr::new(ExprKind::Lit(Lit::Rational(*r)), expr.span)),
                ast::Lit::Real(r) => Ok(Expr::new(ExprKind::Lit(Lit::Real(*r)), expr.span)),
                ast::Lit::Bool(b) => Ok(Expr::new(ExprKind::Lit(Lit::Bool(*b)), expr.span)),
                ast::Lit::String(s) => Ok(Expr::new(ExprKind::Lit(Lit::String(*s)), expr.span)),
                ast::Lit::Char(c) => Ok(Expr::new(ExprKind::Lit(Lit::Char(*c)), expr.span)),
            },
            ast::ExprKind::Var(ident) => {
                if let Some(level) = self.env.find(&ident.name) {
                    Ok(Expr::new(
                        ExprKind::Var(ScopedIdent::new(ident.name, level, expr.span)),
                        expr.span.clone(),
                    ))
                } else {
                    Err(ResError::new(
                        ResErrorKind::UnboundName(ident.name),
                        expr.span,
                    ))
                }
            }
            ast::ExprKind::Lambda(params, fn_expr) => {
                let mut res_params = vec![];
                let mut idents = vec![];
                for p in params {
                    match self.resolve_pattern(p, false) {
                        Ok((p, i)) => {
                            res_params.push(p);
                            idents.extend(i);
                        }
                        Err(e) => {
                            for ident in idents {
                                self.env.pop(ident.name);
                            }
                            return Err(e);
                        }
                    }
                }

                let res_expr = self.resolve_expr(fn_expr)?;
                for ident in idents {
                    self.env.pop(ident.name);
                }

                Ok(res_params.into_iter().rev().fold(res_expr, |acc, p| {
                    Expr::new(ExprKind::Lambda(p, acc), expr.span)
                }))
            }
            ast::ExprKind::Apply(fun, args) => {
                let res_fun = self.resolve_expr(fun)?;
                let res_args = args
                    .iter()
                    .map(|arg| self.resolve_expr(arg))
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(Expr::new(
                    res_args
                        .into_iter()
                        .fold(res_fun, |acc, arg| {
                            Expr::new(ExprKind::Apply(acc, arg), expr.span)
                        })
                        .kind
                        .as_ref()
                        .clone(),
                    expr.span,
                ))
            }
            ast::ExprKind::UnaryOp(op, op_expr) => {
                let name = InternedString::from(*op);
                let ident = ScopedIdent::new(name, 0, op.span);
                Ok(Expr::new(
                    ExprKind::Apply(
                        Expr::new(ExprKind::Var(ident), op.span),
                        self.resolve_expr(op_expr)?,
                    ),
                    expr.span,
                ))
            }
            ast::ExprKind::BinaryOp(op, lhs, rhs) => {
                let name = InternedString::from(*op);
                let ident = ScopedIdent::new(name, 0, op.span);
                let res_lhs = self.resolve_expr(lhs)?;
                Ok(Expr::new(
                    ExprKind::Apply(
                        Expr::new(
                            ExprKind::Apply(
                                Expr::new(ExprKind::Var(ident), op.span),
                                res_lhs.clone(),
                            ),
                            op.span.extend(res_lhs.span),
                        ),
                        self.resolve_expr(rhs)?,
                    ),
                    expr.span,
                ))
            }
            ast::ExprKind::Or(lhs, rhs) => Ok(Expr::new(
                ExprKind::Or(self.resolve_expr(lhs)?, self.resolve_expr(rhs)?),
                expr.span,
            )),
            ast::ExprKind::And(lhs, rhs) => Ok(Expr::new(
                ExprKind::And(self.resolve_expr(lhs)?, self.resolve_expr(rhs)?),
                expr.span,
            )),
            ast::ExprKind::Let(pat, let_expr, body) => {
                if let_expr.is_lambda() {
                    match pat.kind.as_ref() {
                        ast::PatternKind::Ident(ident, _) => {
                            let level = self.env.push(ident.name);
                            let res_pat = Pattern::new(
                                PatternKind::Ident(
                                    ScopedIdent::new(ident.name, level, ident.span),
                                    None,
                                ),
                                ident.span,
                            );
                            let res_expr = self.resolve_expr(&let_expr)?;
                            Ok(Expr::new(
                                ExprKind::Let(res_pat, true, res_expr, self.resolve_expr(&body)?),
                                expr.span,
                            ))
                        }
                        _ => Err(ResError::new(ResErrorKind::InvalidLetPattern, pat.span)),
                    }
                } else {
                    let res_expr = self.resolve_expr(&let_expr)?;
                    let (res_pat, idents) = self.resolve_pattern(pat, false)?;
                    let res_body = self.resolve_expr(&body)?;
                    for ident in idents {
                        self.env.pop(ident.name);
                    }

                    Ok(Expr::new(
                        ExprKind::Let(res_pat, false, res_expr, res_body),
                        expr.span,
                    ))
                }
            }
            ast::ExprKind::Fn(ident, params, fn_expr, body) => {
                let level = self.env.push(ident.name);

                let mut res_params = vec![];
                let mut idents = vec![];
                for p in params {
                    match self.resolve_pattern(p, false) {
                        Ok((p, i)) => {
                            res_params.push(p);
                            idents.extend(i);
                        }
                        Err(e) => {
                            for ident in idents {
                                self.env.pop(ident.name);
                            }
                            return Err(e);
                        }
                    }
                }

                let res_expr = self.resolve_expr(fn_expr)?;
                for ident in idents {
                    self.env.pop(ident.name);
                }

                let res_body = self.resolve_expr(body)?;
                self.env.pop(ident.name);

                let lam = res_params.into_iter().rev().fold(res_expr, |acc, p| {
                    Expr::new(ExprKind::Lambda(p, acc), expr.span)
                });

                Ok(Expr::new(
                    ExprKind::Let(
                        Pattern::new(
                            PatternKind::Ident(
                                ScopedIdent::new(ident.name, level, ident.span),
                                None,
                            ),
                            ident.span,
                        ),
                        true,
                        lam,
                        res_body,
                    ),
                    expr.span,
                ))
            }
            ast::ExprKind::If(cond, then, else_) => Ok(Expr::new(
                ExprKind::If(
                    self.resolve_expr(&cond)?,
                    self.resolve_expr(&then)?,
                    self.resolve_expr(&else_)?,
                ),
                expr.span,
            )),
            ast::ExprKind::Match(expr, arms) => {
                let res_expr = self.resolve_expr(&expr)?;
                let mut res_arms = vec![];
                for (pat, body) in arms {
                    let (res_pat, idents) = self.resolve_pattern(pat, false)?;
                    let res_body = self.resolve_expr(body)?;
                    for ident in idents {
                        self.env.pop(ident.name);
                    }
                    res_arms.push((res_pat, res_body));
                }
                Ok(Expr::new(ExprKind::Match(res_expr, res_arms), expr.span))
            }
            ast::ExprKind::List(exprs) => Ok(Expr::new(
                ExprKind::List(
                    exprs
                        .iter()
                        .map(|e| self.resolve_expr(e))
                        .collect::<ResResult<Vec<Expr>>>()?,
                ),
                expr.span,
            )),
            ast::ExprKind::Unit => Ok(Expr::new(ExprKind::Unit, expr.span)),
        }
    }

    fn resolve_pattern(
        &mut self,
        pat: &ast::Pattern,
        overwrite: bool,
    ) -> ResResult<(Pattern, Vec<Ident>)> {
        match pat.kind.as_ref() {
            ast::PatternKind::Ident(ident, hint) => {
                let level = if overwrite {
                    self.env.overwrite(ident.name);
                    0
                } else {
                    self.env.push(ident.name)
                };

                Ok((
                    Pattern::new(
                        PatternKind::Ident(ScopedIdent::new(ident.name, level, ident.span), {
                            if let Some(hint) = hint {
                                Some(self.resolve_hint(hint)?)
                            } else {
                                None
                            }
                        }),
                        pat.span,
                    ),
                    vec![*ident],
                ))
            }
            ast::PatternKind::Wildcard => {
                Ok((Pattern::new(PatternKind::Wildcard, pat.span), vec![]))
            }
            ast::PatternKind::List(pats) => {
                let mut solved_pats = vec![];
                let mut idents = vec![];
                for p in pats {
                    let (res_pat, res_idents) = self.resolve_pattern(p, false)?;
                    idents.extend(res_idents);
                    solved_pats.push(res_pat);
                }
                Ok((
                    Pattern::new(PatternKind::List(solved_pats), pat.span),
                    idents,
                ))
            }
            ast::PatternKind::Pair(head, tail) => {
                let (res_head, head_idents) = self.resolve_pattern(head, false)?;
                let (res_tail, tail_idents) = self.resolve_pattern(tail, false)?;
                Ok((
                    Pattern::new(PatternKind::Pair(res_head, res_tail), pat.span),
                    head_idents
                        .into_iter()
                        .chain(tail_idents)
                        .collect::<Vec<_>>(),
                ))
            }
            ast::PatternKind::Lit(lit) => Ok((
                Pattern::new(
                    PatternKind::Lit(match lit {
                        ast::Lit::Byte(b) => Lit::Byte(*b),
                        ast::Lit::Int(n) => Lit::Int(*n),
                        ast::Lit::Rational(r) => Lit::Rational(*r),
                        ast::Lit::Real(r) => Lit::Real(*r),
                        ast::Lit::Bool(b) => Lit::Bool(*b),
                        ast::Lit::String(s) => Lit::String(*s),
                        ast::Lit::Char(c) => Lit::Char(*c),
                    }),
                    pat.span,
                ),
                vec![],
            )),
            ast::PatternKind::Unit => Ok((Pattern::new(PatternKind::Unit, pat.span), vec![])),
        }
    }

    fn resolve_hint(&mut self, hint: &ast::TypeHint) -> ResResult<TypeHint> {
        use ast::TypeHintKind as Atk;
        match hint.kind.as_ref() {
            ast::TypeHintKind::Ident(ident) => {
                todo!()
                // if let Some(name) = self.env.find(&ident.name) {
                //     Ok(TypeHint::new(
                //         TypeHintKind::Ident(Ident::new(name, ident.name, ident.span)),
                //         hint.span,
                //     ))
                // } else {
                //     Err(ResError::new(
                //         ResErrorKind::UnboundName(ident.name),
                //         ident.span,
                //     ))
                // }
            }
            Atk::Byte => Ok(TypeHint::new(TypeHintKind::Byte, hint.span)),
            Atk::Int => Ok(TypeHint::new(TypeHintKind::Int, hint.span)),
            Atk::Rational => Ok(TypeHint::new(TypeHintKind::Rational, hint.span)),
            Atk::Real => Ok(TypeHint::new(TypeHintKind::Real, hint.span)),
            Atk::Bool => Ok(TypeHint::new(TypeHintKind::Bool, hint.span)),
            Atk::String => Ok(TypeHint::new(TypeHintKind::String, hint.span)),
            Atk::Char => Ok(TypeHint::new(TypeHintKind::Char, hint.span)),
            Atk::Fn(params, ret) => Ok(TypeHint::new(
                TypeHintKind::Fn(
                    params
                        .iter()
                        .map(|p| self.resolve_hint(p))
                        .collect::<ResResult<Vec<TypeHint>>>()?,
                    self.resolve_hint(ret)?,
                ),
                hint.span,
            )),
            ast::TypeHintKind::List(hint) => Ok(TypeHint::new(
                TypeHintKind::List(self.resolve_hint(hint)?),
                hint.span,
            )),
            ast::TypeHintKind::Unit => Ok(TypeHint::new(TypeHintKind::Unit, hint.span)),
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
