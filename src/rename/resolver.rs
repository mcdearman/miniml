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
            "gt", "gte", "println", "pair",
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
            ast::DeclKind::Let(ident, expr) => {
                let res_pat = self.resolve_pattern(&ident)?;
                let res_expr = self.resolve_expr(&expr)?;
                Ok(Decl {
                    kind: DeclKind::Let(res_pat, res_expr),
                    span: decl.span,
                })
            }
            ast::DeclKind::Fn(ident, params, fn_expr) => {
                let res_name = self.env.define(ident.key);

                self.env.push();
                let mut res_params = vec![];
                for p in params {
                    match self.resolve_pattern(p) {
                        Ok(p) => res_params.push(p),
                        Err(e) => {
                            self.env.pop();
                            return Err(e);
                        }
                    }
                }

                let res_expr = self.resolve_expr(&fn_expr)?;
                self.env.pop();

                Ok(Decl {
                    kind: DeclKind::Fn(
                        ScopedIdent::new(res_name, ident.key, ident.span),
                        res_params,
                        res_expr,
                    ),
                    span: decl.span,
                })
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
                if let Some(name) = self.env.find(&ident.key) {
                    Ok(Expr::new(
                        ExprKind::Var(ScopedIdent::new(name, ident.key, expr.span)),
                        expr.span.clone(),
                    ))
                } else {
                    Err(ResError::new(
                        ResErrorKind::UnboundName(ident.key),
                        expr.span,
                    ))
                }
            }
            ast::ExprKind::Lambda(params, fn_expr) => {
                self.env.push();

                let mut res_params = vec![];
                for p in params {
                    match self.resolve_pattern(p) {
                        Ok(p) => res_params.push(p),
                        Err(e) => {
                            self.env.pop();
                            return Err(e);
                        }
                    }
                }

                let res_expr = self.resolve_expr(fn_expr)?;
                self.env.pop();

                Ok(Expr::new(ExprKind::Lambda(res_params, res_expr), expr.span))
            }
            ast::ExprKind::Apply(fun, args) => Ok(Expr::new(
                ExprKind::Apply(
                    self.resolve_expr(fun)?,
                    args.iter()
                        .map(|arg| self.resolve_expr(arg))
                        .collect::<ResResult<Vec<Expr>>>()?,
                ),
                expr.span,
            )),
            ast::ExprKind::UnaryOp(op, op_expr) => {
                let name = InternedString::from(*op);
                let id = self
                    .get_builtin(name)
                    .ok_or(ResError::new(ResErrorKind::UnboundBuiltIn(name), op.span))?;
                let ident = ScopedIdent::new(id, name, op.span);
                Ok(Expr::new(
                    ExprKind::Apply(
                        Expr::new(ExprKind::Var(ident.clone()), op.span),
                        vec![self.resolve_expr(op_expr)?],
                    ),
                    expr.span,
                ))
            }
            ast::ExprKind::BinaryOp(op, lhs, rhs) => {
                let name = InternedString::from(*op);
                let id = self
                    .get_builtin(name)
                    .ok_or(ResError::new(ResErrorKind::UnboundBuiltIn(name), op.span))?;
                let ident = ScopedIdent::new(id, name, op.span);
                Ok(Expr::new(
                    ExprKind::Apply(
                        Expr::new(ExprKind::Var(ident.clone()), op.span),
                        vec![self.resolve_expr(lhs)?, self.resolve_expr(rhs)?],
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
            ast::ExprKind::Let(name, let_expr, body) => {
                let res_expr = self.resolve_expr(&let_expr)?;
                self.env.push();
                let res_pat = self.resolve_pattern(name)?;
                let res_body = self.resolve_expr(&body)?;
                self.env.pop();

                Ok(Expr::new(
                    ExprKind::Let(res_pat, res_expr, res_body),
                    expr.span,
                ))
            }
            ast::ExprKind::Fn(name, params, fn_expr, body) => {
                self.env.push();
                let res_name = self.env.define(name.key);
                self.env.push();
                let mut res_params = vec![];
                for p in params {
                    match self.resolve_pattern(p) {
                        Ok(p) => res_params.push(p),
                        Err(e) => {
                            self.env.pop();
                            return Err(e);
                        }
                    }
                }
                let res_expr = self.resolve_expr(fn_expr)?;
                self.env.pop();
                let res_body = self.resolve_expr(body)?;
                self.env.pop();

                Ok(Expr::new(
                    ExprKind::Fn(
                        ScopedIdent::new(res_name, name.key, name.span),
                        res_params,
                        res_expr,
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
                    self.env.push();
                    let res_pat = self.resolve_pattern(pat)?;
                    let res_body = self.resolve_expr(body)?;
                    self.env.pop();
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

    fn resolve_pattern(&mut self, pat: &ast::Pattern) -> ResResult<Pattern> {
        match pat.kind.as_ref() {
            ast::PatternKind::Ident(ident, hint) => {
                let id = self.env.define(ident.key);
                Ok(Pattern::new(
                    PatternKind::Ident(ScopedIdent::new(id, ident.key, ident.span), {
                        if let Some(hint) = hint {
                            Some(self.resolve_hint(hint)?)
                        } else {
                            None
                        }
                    }),
                    pat.span,
                ))
                // if let Some(name) = self.env.find(&ident.key) {
                //     Ok(Pattern::new(
                //         PatternKind::Ident(ScopedIdent::new(name, ident.key, ident.span), {
                //             if let Some(hint) = hint {
                //                 Some(self.resolve_hint(hint)?)
                //             } else {
                //                 None
                //             }
                //         }),
                //         pat.span,
                //     ))
                // } else {
                //     Err(ResError::new(
                //         ResErrorKind::UnboundName(ident.key),
                //         ident.span,
                //     ))
                // }
            }
            ast::PatternKind::Wildcard => Ok(Pattern::new(PatternKind::Wildcard, pat.span)),
            // ast::PatternKind::Tuple(pats) => Ok(Pattern::new(
            //     PatternKind::Tuple(
            //         pats.iter()
            //             .map(|p| self.resolve_pattern(p))
            //             .collect::<ResResult<Vec<Pattern>>>()?,
            //     ),
            //     pat.span,
            // )),
            ast::PatternKind::List(pats) => Ok(Pattern::new(
                PatternKind::List(
                    pats.iter()
                        .map(|p| self.resolve_pattern(p))
                        .collect::<ResResult<Vec<Pattern>>>()?,
                ),
                pat.span,
            )),
            ast::PatternKind::Pair(head, tail) => Ok(Pattern::new(
                PatternKind::Pair(self.resolve_pattern(head)?, self.resolve_pattern(tail)?),
                pat.span,
            )),
            ast::PatternKind::Lit(lit) => Ok(Pattern::new(
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
            )),
            ast::PatternKind::Unit => Ok(Pattern::new(PatternKind::Unit, pat.span)),
        }
    }

    fn resolve_hint(&mut self, hint: &ast::TypeHint) -> ResResult<TypeHint> {
        match hint.kind.as_ref() {
            ast::TypeHintKind::Ident(ident) => {
                if let Some(name) = self.env.find(&ident.key) {
                    Ok(TypeHint::new(
                        TypeHintKind::Ident(ScopedIdent::new(name, ident.key, ident.span)),
                        hint.span,
                    ))
                } else {
                    Err(ResError::new(
                        ResErrorKind::UnboundName(ident.key),
                        ident.span,
                    ))
                }
            }
            ast::TypeHintKind::Byte => Ok(TypeHint::new(TypeHintKind::Byte, hint.span)),
            ast::TypeHintKind::Int => Ok(TypeHint::new(TypeHintKind::Int, hint.span)),
            ast::TypeHintKind::Rational => Ok(TypeHint::new(TypeHintKind::Rational, hint.span)),
            ast::TypeHintKind::Real => Ok(TypeHint::new(TypeHintKind::Real, hint.span)),
            ast::TypeHintKind::Bool => Ok(TypeHint::new(TypeHintKind::Bool, hint.span)),
            ast::TypeHintKind::String => Ok(TypeHint::new(TypeHintKind::String, hint.span)),
            ast::TypeHintKind::Char => Ok(TypeHint::new(TypeHintKind::Char, hint.span)),
            ast::TypeHintKind::Fn(params, ret) => Ok(TypeHint::new(
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
