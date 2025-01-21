use super::{
    env::Env,
    error::{ResError, ResErrorKind, ResResult},
};
use miniml_ast::{self as ast, SynNode};
use miniml_nir::{res_id::ResId, *};
use miniml_utils::intern::InternedString;
use scoped_ident::ScopedIdent;
use std::{collections::HashMap, vec};

#[derive(Debug)]
pub struct Resolver {
    builtins: HashMap<ResId, InternedString>,
    env: Env,
    errors: Vec<ResError>,
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
            errors: vec![],
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

    pub fn clear_errors(&mut self) {
        self.errors.clear();
    }

    pub fn resolve(&mut self, prog: &ast::Prog) -> (Option<Prog>, Vec<ResError>) {
        let mut decls = Vec::new();

        self.define_top_level(prog);

        for decl in &prog.inner.decls {
            match self.resolve_decl(decl) {
                Ok(decl) => decls.push(decl),
                Err(e) => self.errors.push(e),
            }
        }

        let mut res_imports = vec![vec![]];
        for i in &prog.inner.imports {
            match self.resolve_path(i) {
                Ok(p) => res_imports.push(p),
                Err(e) => self.errors.push(e),
            }
        }

        if decls.is_empty() {
            (None, self.errors.clone())
        } else {
            (
                Some(SynNode::new(
                    Module {
                        name: SynNode::new(
                            ScopedIdent::new(
                                self.env.define(prog.inner.name.inner),
                                prog.inner.name.inner,
                            ),
                            prog.meta,
                        ),
                        imports: res_imports,
                        decls,
                    },
                    prog.meta,
                )),
                self.errors.clone(),
            )
        }
    }

    /// First pass to define top-level names
    fn define_top_level(&mut self, prog: &ast::Prog) -> Vec<ResError> {
        let mut names = vec![];

        for decl in &prog.inner.decls {
            match &decl.inner {
                ast::DeclKind::Def(pat, expr) => match expr.inner.as_ref() {
                    &ast::ExprKind::Lambda(_, _) => match pat.inner.as_ref() {
                        ast::PatternKind::Ident(ident, _) => {
                            if names.contains(&ident.inner) {
                                self.errors.push(ResError::new(
                                    ResErrorKind::DuplicateName(ident.inner),
                                    ident.meta,
                                ));
                            } else {
                                self.env.define(ident.inner);
                                names.push(ident.inner);
                            }
                        }
                        _ => continue,
                    },
                    _ => continue,
                },
                ast::DeclKind::Fn(name, _, _) | ast::DeclKind::FnMatch(name, _) => {
                    if names.contains(&name.inner) {
                        self.errors.push(ResError::new(
                            ResErrorKind::DuplicateName(name.inner),
                            name.meta,
                        ));
                    } else {
                        self.env.define(name.inner);
                        names.push(name.inner);
                    }
                }
                _ => todo!(),
            }
        }

        self.errors.clone()
    }

    fn resolve_path(&mut self, path: &ast::Path) -> ResResult<Path> {
        let mut res_path = Vec::new();
        for ident in path {
            let id = self.env.define(ident.inner);
            res_path.push(SynNode::new(ScopedIdent::new(id, ident.inner), ident.meta));
        }
        Ok(res_path)
    }

    fn resolve_decl(&mut self, decl: &ast::Decl) -> ResResult<Decl> {
        match &decl.inner {
            ast::DeclKind::Def(pattern, expr) => {
                if matches!(expr.inner.as_ref(), &ast::ExprKind::Lambda(_, _)) {
                    match pattern.inner.as_ref() {
                        ast::PatternKind::Ident(ident, _) => {
                            let res_name = SynNode::new(
                                ScopedIdent::new(
                                    self.env.find(&ident.inner).ok_or(ResError::new(
                                        ResErrorKind::UnboundName(ident.inner),
                                        ident.meta,
                                    ))?,
                                    ident.inner,
                                ),
                                ident.meta,
                            );
                            let res_expr = self.resolve_expr(&expr)?;

                            Ok(SynNode::new(
                                DeclKind::Def(Def::new(
                                    DefKind::Rec {
                                        ident: res_name,
                                        anno: None,
                                        body: res_expr,
                                    },
                                    expr.meta,
                                )),
                                decl.meta,
                            ))
                        }
                        _ => Err(ResError::new(ResErrorKind::InvalidDefPattern, pattern.meta)),
                    }
                } else {
                    let res_expr = self.resolve_expr(&expr)?;
                    let res_pat = self.resolve_pattern(&pattern)?;

                    Ok(SynNode::new(
                        DeclKind::Def(Def::new(
                            DefKind::NonRec {
                                pat: res_pat,
                                body: res_expr,
                            },
                            expr.meta,
                        )),
                        decl.meta,
                    ))
                }
            }
            ast::DeclKind::Fn(ident, params, fn_expr) => {
                let res_name = SynNode::new(
                    ScopedIdent::new(
                        self.env.find(&ident.inner).ok_or(ResError::new(
                            ResErrorKind::UnboundName(ident.inner),
                            ident.meta,
                        ))?,
                        ident.inner,
                    ),
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
                    DeclKind::Def(Def::new(
                        DefKind::Rec {
                            ident: res_name,
                            anno: None,
                            body: res_lam,
                        },
                        decl.meta,
                    )),
                    decl.meta,
                ))
            }
            ast::DeclKind::FnMatch(ident, arms) => {
                let res_name = SynNode::new(
                    ScopedIdent::new(
                        self.env.find(&ident.inner).ok_or(ResError::new(
                            ResErrorKind::UnboundName(ident.inner),
                            ident.meta,
                        ))?,
                        ident.inner,
                    ),
                    ident.meta,
                );

                let mut res_arms = vec![];
                for (params, body) in arms {
                    self.env.push();
                    let res_params = params
                        .iter()
                        .map(|p| self.resolve_pattern(p))
                        .collect::<ResResult<Vec<Pattern>>>()?;
                    let res_body = self.resolve_expr(body)?;
                    self.env.pop();
                    res_arms.push((res_params, res_body));
                }

                // Ok(SynNode::new(
                //     DeclKind::Def(DefGroup::NonRec(Def::FnMatch {
                //         ident: res_name,
                //         arms: res_arms,
                //     })),
                //     decl.meta,
                // ))
                todo!()
            }
            _ => todo!(),
        }
    }

    fn resolve_expr(&mut self, expr: &ast::Expr) -> ResResult<Expr> {
        match expr.inner.as_ref() {
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
                if let Some(id) = self.env.find(&ident.inner) {
                    Ok(Expr::new(
                        ExprKind::Var(SynNode::new(ScopedIdent::new(id, ident.inner), ident.meta)),
                        expr.meta.clone(),
                    ))
                } else {
                    Err(ResError::new(
                        ResErrorKind::UnboundName(ident.inner),
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
                        .inner
                        .as_ref()
                        .clone(),
                    expr.meta,
                ))
            }
            ast::ExprKind::UnaryOp(op, op_expr) => {
                let name = InternedString::from(op.inner);
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
                let name = InternedString::from(op.inner);
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
                if matches!(let_expr.inner.as_ref(), &ast::ExprKind::Lambda(_, _)) {
                    match pat.inner.as_ref() {
                        ast::PatternKind::Ident(ident, _) => {
                            self.env.push();
                            let res_pat = Pattern::new(
                                PatternKind::Ident(
                                    SynNode::new(
                                        ScopedIdent::new(self.env.define(ident.inner), ident.inner),
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
                let res_name = self.env.define(ident.inner);
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
                        SynNode::new(ScopedIdent::new(res_name, ident.inner), ident.meta),
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
            ast::ExprKind::Match(expr, arms) => self.resolve_match(expr, arms),
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

    fn resolve_match(
        &mut self,
        expr: &ast::Expr,
        arms: &Vec<(ast::Pattern, ast::Expr)>,
    ) -> ResResult<Expr> {
        let res_expr = self.resolve_expr(expr)?;
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

    fn resolve_pattern(&mut self, pat: &ast::Pattern) -> ResResult<Pattern> {
        match pat.inner.as_ref() {
            ast::PatternKind::Ident(ident, hint) => {
                let id = self.env.define(ident.inner);
                Ok(Pattern::new(
                    PatternKind::Ident(
                        SynNode::new(ScopedIdent::new(id, ident.inner), ident.meta),
                        {
                            if let Some(hint) = hint {
                                Some(self.resolve_type_anno(hint)?)
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

    fn resolve_type_anno(&mut self, anno: &ast::TypeAnno) -> ResResult<TypeAnno> {
        use ast::TypeAnnoKind as Atk;
        match anno.inner.as_ref() {
            Atk::Ident(ident) => {
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
            Atk::Byte => Ok(TypeAnno::new(TypeAnnoKind::Byte, anno.meta)),
            Atk::Int => Ok(TypeAnno::new(TypeAnnoKind::Int, anno.meta)),
            Atk::Rational => Ok(TypeAnno::new(TypeAnnoKind::Rational, anno.meta)),
            Atk::Real => Ok(TypeAnno::new(TypeAnnoKind::Real, anno.meta)),
            Atk::Bool => Ok(TypeAnno::new(TypeAnnoKind::Bool, anno.meta)),
            Atk::String => Ok(TypeAnno::new(TypeAnnoKind::String, anno.meta)),
            Atk::Char => Ok(TypeAnno::new(TypeAnnoKind::Char, anno.meta)),
            Atk::Fn(param, ret) => Ok(TypeAnno::new(
                TypeAnnoKind::Fn(self.resolve_type_anno(param)?, self.resolve_type_anno(ret)?),
                anno.meta,
            )),
            ast::TypeAnnoKind::List(hint) => Ok(TypeAnno::new(
                TypeAnnoKind::List(self.resolve_type_anno(hint)?),
                hint.meta,
            )),
            ast::TypeAnnoKind::Unit => Ok(TypeAnno::new(TypeAnnoKind::Unit, anno.meta)),
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
