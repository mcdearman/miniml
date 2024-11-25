use super::{
    env::Env,
    error::{ResError, ResErrorKind, ResResult},
    nir::*,
    res_id::ResId,
    scoped_ident::ScopedIdent,
};
use miniml_ast::{self as ast, SynNode};
use miniml_utils::intern::InternedString;
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

    fn get_builtin(&mut self, name: InternedString) -> Option<ResId> {
        for (id, n) in &self.builtins {
            if n == &name {
                return Some(*id);
            }
        }
        None
    }

    pub fn resolve(&mut self, prog: &ast::Prog) -> (Option<Prog>, Vec<ResError>) {
        let mut decls = Vec::new();
        let mut errors = Vec::new();

        for decl in &prog.value.decls {
            match self.resolve_decl(decl) {
                Ok(decl) => decls.push(decl),
                Err(e) => errors.push(e),
            }
        }

        let mut res_imports = vec![vec![]];
        for i in &prog.value.imports {
            match self.resolve_path(i) {
                Ok(p) => res_imports.push(p),
                Err(e) => errors.push(e),
            }
        }

        if decls.is_empty() {
            (None, errors)
        } else {
            (
                Some(SynNode::new(
                    Module {
                        name: SynNode::new(
                            ScopedIdent::new(
                                self.env.define(prog.value.name.value),
                                prog.value.name.value,
                            ),
                            prog.meta,
                        ),
                        imports: res_imports,
                        decls,
                    },
                    prog.meta,
                )),
                errors,
            )
        }
    }

    fn resolve_path(&mut self, path: &ast::Path) -> ResResult<Path> {
        let mut res_path = Vec::new();
        for ident in path {
            let id = self.env.define(ident.value);
            res_path.push(SynNode::new(ScopedIdent::new(id, ident.value), ident.meta));
        }
        Ok(res_path)
    }

    fn resolve_decl(&mut self, decl: &ast::Decl) -> ResResult<Decl> {
        match &decl.value {
            ast::DeclKind::Def(pattern, expr) => {
                if matches!(expr.value.as_ref(), &ast::ExprKind::Lambda(_, _)) {
                    match pattern.value.as_ref() {
                        ast::PatternKind::Ident(ident, _) => {
                            let res_name = SynNode::new(
                                ScopedIdent::new(self.env.define(ident.value), ident.value),
                                ident.meta,
                            );
                            let res_expr = self.resolve_expr(&expr)?;

                            Ok(SynNode::new(
                                DeclKind::Def(DefGroup::Rec(vec![DefRec {
                                    ident: res_name,
                                    anno: None,
                                    body: res_expr,
                                }])),
                                decl.meta,
                            ))
                        }
                        _ => Err(ResError::new(ResErrorKind::InvalidDefPattern, pattern.meta)),
                    }
                } else {
                    let res_expr = self.resolve_expr(&expr)?;
                    let res_pat = self.resolve_pattern(&pattern)?;

                    Ok(SynNode::new(
                        DeclKind::Def(DefGroup::NonRec(Def {
                            pat: res_pat,
                            body: res_expr,
                        })),
                        decl.meta,
                    ))
                }
            }
            ast::DeclKind::Fn(ident, params, fn_expr) => {
                let res_name = SynNode::new(
                    ScopedIdent::new(self.env.define(ident.value), ident.value),
                    ident.meta,
                );

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

                let res_lam = res_params.into_iter().rev().fold(res_expr, |acc, p| {
                    Expr::new(ExprKind::Lambda(p, acc), fn_expr.meta)
                });

                Ok(SynNode::new(
                    DeclKind::Def(DefGroup::Rec(vec![DefRec {
                        ident: res_name,
                        anno: None,
                        body: res_lam,
                    }])),
                    decl.meta,
                ))
            }
            ast::DeclKind::FnMatch(name, arms) => {
                // if arms.is_empty() {
                //     return Err(ResError::new(ResErrorKind::EmptyFnMatch, decl.meta));
                // }

                // let name = arms[0].0;
                // let res_name = self.env.define(name.str);

                // let param_len = arms[0].1.len();

                // // let mut res_params = vec![];
                // let mut res_arms = vec![];
                // for (n, params, body) in arms {
                //     if *n != name {
                //         return Err(ResError::new(ResErrorKind::TooManyFnNames, decl.meta));
                //     }
                //     if params.len() != param_len {
                //         return Err(ResError::new(ResErrorKind::FnArmParamMismatch, decl.meta));
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
        match expr.value.as_ref() {
            ast::ExprKind::Lit(l) => match l {
                ast::Lit::Byte(b) => Ok(Expr::new(ExprKind::Lit(Lit::Byte(*b)), expr.meta)),
                ast::Lit::Int(n) => Ok(Expr::new(ExprKind::Lit(Lit::Int(*n)), expr.meta)),
                ast::Lit::Rational(r) => Ok(Expr::new(ExprKind::Lit(Lit::Rational(*r)), expr.meta)),
                ast::Lit::Real(r) => Ok(Expr::new(ExprKind::Lit(Lit::Real(*r)), expr.meta)),
                ast::Lit::Bool(b) => Ok(Expr::new(ExprKind::Lit(Lit::Bool(*b)), expr.meta)),
                ast::Lit::String(s) => Ok(Expr::new(ExprKind::Lit(Lit::String(*s)), expr.meta)),
                ast::Lit::Char(c) => Ok(Expr::new(ExprKind::Lit(Lit::Char(*c)), expr.meta)),
            },
            ast::ExprKind::Var(ident) => {
                if let Some(id) = self.env.find(&ident.value) {
                    Ok(Expr::new(
                        ExprKind::Var(SynNode::new(ScopedIdent::new(id, ident.value), ident.meta)),
                        expr.meta.clone(),
                    ))
                } else {
                    Err(ResError::new(
                        ResErrorKind::UnboundName(ident.value),
                        expr.meta,
                    ))
                }
            }
            ast::ExprKind::Lambda(params, fn_expr) => {
                let mut res_params = vec![];
                for p in params {
                    match self.resolve_pattern(p) {
                        Ok(p) => {
                            res_params.push(p);
                        }
                        Err(e) => {
                            self.env.pop();
                            return Err(e);
                        }
                    }
                }

                let res_expr = self.resolve_expr(fn_expr)?;
                self.env.pop();

                Ok(res_params.into_iter().rev().fold(res_expr, |acc, p| {
                    Expr::new(ExprKind::Lambda(p, acc), expr.meta)
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
                            Expr::new(ExprKind::Apply(acc, arg), expr.meta)
                        })
                        .value
                        .as_ref()
                        .clone(),
                    expr.meta,
                ))
            }
            ast::ExprKind::UnaryOp(op, op_expr) => {
                let name = InternedString::from(op.value);
                let id = self
                    .get_builtin(name)
                    .ok_or(ResError::new(ResErrorKind::UnboundBuiltIn(name), op.meta))?;
                let ident = SynNode::new(ScopedIdent::new(id, name), op.meta);
                Ok(Expr::new(
                    ExprKind::Apply(
                        Expr::new(ExprKind::Var(ident), op.meta),
                        self.resolve_expr(op_expr)?,
                    ),
                    expr.meta,
                ))
            }
            ast::ExprKind::BinaryOp(op, lhs, rhs) => {
                let name = InternedString::from(op.value);
                let id = self
                    .get_builtin(name)
                    .ok_or(ResError::new(ResErrorKind::UnboundBuiltIn(name), op.meta))?;
                let ident = SynNode::new(ScopedIdent::new(id, name), op.meta);
                let res_lhs = self.resolve_expr(lhs)?;
                Ok(Expr::new(
                    ExprKind::Apply(
                        Expr::new(
                            ExprKind::Apply(
                                Expr::new(ExprKind::Var(ident), op.meta),
                                res_lhs.clone(),
                            ),
                            op.meta.extend(res_lhs.meta),
                        ),
                        self.resolve_expr(rhs)?,
                    ),
                    expr.meta,
                ))
            }
            ast::ExprKind::Or(lhs, rhs) => Ok(Expr::new(
                ExprKind::Or(self.resolve_expr(lhs)?, self.resolve_expr(rhs)?),
                expr.meta,
            )),
            ast::ExprKind::And(lhs, rhs) => Ok(Expr::new(
                ExprKind::And(self.resolve_expr(lhs)?, self.resolve_expr(rhs)?),
                expr.meta,
            )),
            ast::ExprKind::Let(pat, let_expr, body) => {
                if matches!(let_expr.value.as_ref(), &ast::ExprKind::Lambda(_, _)) {
                    match pat.value.as_ref() {
                        ast::PatternKind::Ident(ident, _) => {
                            self.env.push();
                            let res_pat = Pattern::new(
                                PatternKind::Ident(
                                    SynNode::new(
                                        ScopedIdent::new(self.env.define(ident.value), ident.value),
                                        ident.meta,
                                    ),
                                    None,
                                ),
                                ident.meta,
                            );
                            let res_expr = self.resolve_expr(&let_expr)?;
                            Ok(Expr::new(
                                ExprKind::Let(res_pat, true, res_expr, self.resolve_expr(&body)?),
                                expr.meta,
                            ))
                        }
                        _ => Err(ResError::new(ResErrorKind::InvalidLetPattern, pat.meta)),
                    }
                } else {
                    let res_expr = self.resolve_expr(&let_expr)?;
                    self.env.push();
                    let res_pat = self.resolve_pattern(pat)?;
                    let res_body = self.resolve_expr(&body)?;
                    self.env.pop();

                    Ok(Expr::new(
                        ExprKind::Let(res_pat, false, res_expr, res_body),
                        expr.meta,
                    ))
                }
            }
            ast::ExprKind::Fn(ident, params, fn_expr, body) => {
                self.env.push();
                let res_name = self.env.define(ident.value);
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

                let res_pat = Pattern::new(
                    PatternKind::Ident(
                        SynNode::new(ScopedIdent::new(res_name, ident.value), ident.meta),
                        None,
                    ),
                    ident.meta,
                );

                let res_lam = res_params.into_iter().rev().fold(res_expr, |acc, p| {
                    Expr::new(ExprKind::Lambda(p, acc), fn_expr.meta)
                });

                Ok(Expr::new(
                    ExprKind::Let(res_pat, true, res_lam, res_body),
                    expr.meta,
                ))
            }
            ast::ExprKind::If(cond, then, else_) => Ok(Expr::new(
                ExprKind::If(
                    self.resolve_expr(&cond)?,
                    self.resolve_expr(&then)?,
                    self.resolve_expr(&else_)?,
                ),
                expr.meta,
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
                Ok(Expr::new(ExprKind::Match(res_expr, res_arms), expr.meta))
            }
            ast::ExprKind::List(exprs) => Ok(Expr::new(
                ExprKind::List(
                    exprs
                        .iter()
                        .map(|e| self.resolve_expr(e))
                        .collect::<ResResult<Vec<Expr>>>()?,
                ),
                expr.meta,
            )),
            ast::ExprKind::Unit => Ok(Expr::new(ExprKind::Unit, expr.meta)),
        }
    }

    fn resolve_pattern(&mut self, pat: &ast::Pattern) -> ResResult<Pattern> {
        match pat.value.as_ref() {
            ast::PatternKind::Ident(ident, hint) => {
                let id = self.env.define(ident.value);
                Ok(Pattern::new(
                    PatternKind::Ident(
                        SynNode::new(ScopedIdent::new(id, ident.value), ident.meta),
                        {
                            if let Some(hint) = hint {
                                Some(self.resolve_hint(hint)?)
                            } else {
                                None
                            }
                        },
                    ),
                    pat.meta,
                ))
            }
            ast::PatternKind::Wildcard => Ok(Pattern::new(PatternKind::Wildcard, pat.meta)),
            ast::PatternKind::List(pats) => Ok(Pattern::new(
                PatternKind::List(
                    pats.iter()
                        .map(|p| self.resolve_pattern(p))
                        .collect::<ResResult<Vec<Pattern>>>()?,
                ),
                pat.meta,
            )),
            ast::PatternKind::Pair(head, tail) => Ok(Pattern::new(
                PatternKind::Pair(self.resolve_pattern(head)?, self.resolve_pattern(tail)?),
                pat.meta,
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
                pat.meta,
            )),
            ast::PatternKind::Unit => Ok(Pattern::new(PatternKind::Unit, pat.meta)),
        }
    }

    fn resolve_hint(&mut self, hint: &ast::TypeAnno) -> ResResult<TypeAnno> {
        use ast::TypeAnnoKind as Atk;
        match hint.value.as_ref() {
            ast::TypeAnnoKind::Ident(ident) => {
                todo!()
                // if let Some(name) = self.env.find(&ident.name) {
                //     Ok(TypeHint::new(
                //         TypeHintKind::Ident(Ident::new(name, ident.name, ident.meta)),
                //         hint.meta,
                //     ))
                // } else {
                //     Err(ResError::new(
                //         ResErrorKind::UnboundName(ident.name),
                //         ident.meta,
                //     ))
                // }
            }
            Atk::Byte => Ok(TypeAnno::new(TypeAnnoKind::Byte, hint.meta)),
            Atk::Int => Ok(TypeAnno::new(TypeAnnoKind::Int, hint.meta)),
            Atk::Rational => Ok(TypeAnno::new(TypeAnnoKind::Rational, hint.meta)),
            Atk::Real => Ok(TypeAnno::new(TypeAnnoKind::Real, hint.meta)),
            Atk::Bool => Ok(TypeAnno::new(TypeAnnoKind::Bool, hint.meta)),
            Atk::String => Ok(TypeAnno::new(TypeAnnoKind::String, hint.meta)),
            Atk::Char => Ok(TypeAnno::new(TypeAnnoKind::Char, hint.meta)),
            Atk::Fn(params, ret) => Ok(TypeAnno::new(
                TypeAnnoKind::Fn(
                    params
                        .iter()
                        .map(|p| self.resolve_hint(p))
                        .collect::<ResResult<Vec<TypeAnno>>>()?,
                    self.resolve_hint(ret)?,
                ),
                hint.meta,
            )),
            ast::TypeAnnoKind::List(hint) => Ok(TypeAnno::new(
                TypeAnnoKind::List(self.resolve_hint(hint)?),
                hint.meta,
            )),
            ast::TypeAnnoKind::Unit => Ok(TypeAnno::new(TypeAnnoKind::Unit, hint.meta)),
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
