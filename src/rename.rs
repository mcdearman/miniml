/*
 * This module resolves names in the AST and produces an IR similar
 * to the AST but with all names resolved to their unique IDs. Names
 * that shadow names from an outer scope are given a new unique ID.
 */
use crate::{
    parse,
    utils::{InternedString, Span, UniqueId},
};
use log::trace;
use num_rational::Rational64;
use std::{cell::RefCell, collections::HashMap, rc::Rc};

#[derive(Debug, Clone, PartialEq)]
pub struct ResError {
    pub msg: InternedString,
    pub span: Span,
}

pub type ResResult<T> = Result<T, ResError>;

#[derive(Debug, Clone, PartialEq)]
pub struct Env {
    parent: Option<Rc<RefCell<Env>>>,
    data: HashMap<InternedString, UniqueId>,
}

impl Env {
    pub fn new() -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(Self {
            parent: None,
            data: HashMap::new(),
        }))
    }

    pub fn new_with_parent(parent: Rc<RefCell<Self>>) -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(Self {
            parent: Some(parent),
            data: HashMap::new(),
        }))
    }

    fn find(&self, name: &InternedString) -> Option<UniqueId> {
        if let Some(id) = self.data.get(name) {
            Some(*id)
        } else if let Some(parent) = &self.parent {
            parent.borrow().find(name)
        } else {
            None
        }
    }

    fn find_in_scope(&self, name: &InternedString) -> Option<UniqueId> {
        self.data.get(name).copied()
    }

    fn find_in_parent(&self, name: &InternedString) -> Option<UniqueId> {
        if let Some(parent) = &self.parent {
            parent.borrow().find(name)
        } else {
            None
        }
    }

    pub fn define(&mut self, name: InternedString) -> UniqueId {
        let id = UniqueId::gen();
        self.data.insert(name, id);
        id
    }

    // pub fn insert(&mut self, name: InternedString, id: UniqueId) {
    //     self.data.insert(name, id);
    // }

    fn define_if_absent(&mut self, name: InternedString) -> UniqueId {
        if let Some(id) = self.data.get(&name) {
            *id
        } else {
            let id = UniqueId::gen();
            self.data.insert(name, id);
            id
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Root {
    decls: Vec<Decl>,
    span: Span,
}

impl Root {
    pub fn new(decls: Vec<Decl>, span: Span) -> Self {
        Self { decls, span }
    }

    pub fn decls(&self) -> &[Decl] {
        &self.decls
    }

    pub fn span(&self) -> &Span {
        &self.span
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Item {
    kind: ItemKind,
    span: Span,
}

impl Item {
    pub fn new(kind: ItemKind, span: Span) -> Self {
        Self { kind, span }
    }

    pub fn kind(&self) -> &ItemKind {
        &self.kind
    }

    pub fn span(&self) -> &Span {
        &self.span
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ItemKind {
    Expr(Expr),
    Decl(Decl),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expr {
    kind: Box<ExprKind>,
    span: Span,
}

impl Expr {
    pub fn new(kind: ExprKind, span: Span) -> Self {
        Self {
            kind: Box::new(kind),
            span,
        }
    }

    pub fn kind(&self) -> &ExprKind {
        &self.kind
    }

    pub fn span(&self) -> &Span {
        &self.span
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind {
    Lit(Lit),
    Ident(Ident),
    Apply { fun: Expr, arg: Expr },
    If { cond: Expr, then: Expr, else_: Expr },
    Let { name: Ident, expr: Expr, body: Expr },
    Lambda { param: Ident, expr: Expr },
    Unit,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Ident {
    name: UniqueId,
    span: Span,
}

impl Ident {
    pub fn new(name: UniqueId, span: Span) -> Self {
        Self { name, span }
    }

    pub fn name(&self) -> &UniqueId {
        &self.name
    }

    pub fn span(&self) -> &Span {
        &self.span
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Lit {
    Num(Rational64),
    Bool(bool),
    String(InternedString),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Decl {
    kind: DeclKind,
    span: Span,
}

impl Decl {
    pub fn new(kind: DeclKind, span: Span) -> Self {
        Self { kind, span }
    }

    pub fn kind(&self) -> &DeclKind {
        &self.kind
    }

    pub fn span(&self) -> &Span {
        &self.span
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum DeclKind {
    Let { name: Ident, expr: Expr },
}

pub fn resolve(env: Rc<RefCell<Env>>, root: &parse::Root) -> (Option<Root>, Vec<ResError>) {
    let mut errors = vec![];
    let mut decls = vec![];
    for decl in root.decls() {
        match resolve_decl(env.clone(), decl) {
            Ok(d) => {
                // trace!("env: {:#?}", env.borrow());
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

fn resolve_decl(env: Rc<RefCell<Env>>, decl: &parse::Decl) -> ResResult<Decl> {
    trace!("decl env: {:#?}", env.borrow());
    match decl.kind() {
        parse::DeclKind::Let { name, expr } => match expr.kind() {
            parse::ExprKind::Lambda { .. } => {
                let name = Ident::new(env.borrow_mut().define(name.name().clone()), *name.span());
                let let_env = Env::new_with_parent(env.clone());
                let expr = resolve_expr(let_env.clone(), expr)?;
                Ok(Decl::new(DeclKind::Let { name, expr }, decl.span().clone()))
            }
            _ => {
                let let_env = Env::new_with_parent(env.clone());
                let expr = resolve_expr(let_env.clone(), expr)?;
                let name = Ident::new(env.borrow_mut().define(name.name().clone()), *name.span());
                Ok(Decl::new(DeclKind::Let { name, expr }, decl.span().clone()))
            }
        },
    }
}

fn resolve_expr(env: Rc<RefCell<Env>>, expr: &parse::Expr) -> ResResult<Expr> {
    match expr.kind() {
        parse::ExprKind::Lit(l) => match l {
            parse::Lit::Num(n) => Ok(Expr::new(ExprKind::Lit(Lit::Num(n.clone())), *expr.span())),
            parse::Lit::Bool(b) => Ok(Expr::new(ExprKind::Lit(Lit::Bool(*b)), expr.span().clone())),
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
                Err(ResError {
                    msg: InternedString::from(&*format!(
                        "name '{:?}' is not defined",
                        ident.name()
                    )),
                    span: expr.span().clone(),
                })
            }
        }
        parse::ExprKind::Apply { fun, arg } => Ok(Expr::new(
            ExprKind::Apply {
                fun: resolve_expr(env.clone(), fun)?,
                arg: resolve_expr(env.clone(), arg)?,
            },
            *expr.span(),
        )),
        parse::ExprKind::Unary { op, expr } => {
            let op = Ident::new(
                env.borrow()
                    .find(&op.clone().into())
                    .expect("operator id not present"),
                *op.span(),
            );
            Ok(Expr::new(
                ExprKind::Apply {
                    fun: Expr::new(ExprKind::Ident(op.clone()), *op.span()),
                    arg: resolve_expr(env.clone(), expr)?,
                },
                *expr.span(),
            ))
        }
        parse::ExprKind::Binary { op, lhs, rhs } => {
            let op = Ident::new(
                env.borrow()
                    .find(&op.clone().into())
                    .expect("operator id not present"),
                *op.span(),
            );
            Ok(Expr::new(
                ExprKind::Apply {
                    fun: Expr::new(ExprKind::Ident(op.clone()), *op.span()),
                    arg: Expr::new(
                        ExprKind::Apply {
                            fun: resolve_expr(env.clone(), lhs)?,
                            arg: resolve_expr(env.clone(), rhs)?,
                        },
                        *expr.span(),
                    ),
                },
                *expr.span(),
            ))
        }
        parse::ExprKind::If { cond, then, else_ } => Ok(Expr::new(
            ExprKind::If {
                cond: resolve_expr(env.clone(), cond)?,
                then: resolve_expr(env.clone(), then)?,
                else_: resolve_expr(env.clone(), else_)?,
            },
            *expr.span(),
        )),
        parse::ExprKind::Let { name, expr, body } => match expr.kind() {
            parse::ExprKind::Lambda { .. } => {
                let name = Ident::new(env.borrow_mut().define(name.name().clone()), *name.span());
                let let_env = Env::new_with_parent(env.clone());
                let res_expr = resolve_expr(let_env.clone(), &expr)?;
                let body = resolve_expr(let_env.clone(), &body)?;
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
                let res_expr = resolve_expr(let_env.clone(), expr)?;
                let name = Ident::new(env.borrow_mut().define(name.name().clone()), *name.span());
                let body = resolve_expr(env.clone(), body)?;
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
            let lambda_env = Env::new_with_parent(env.clone());
            let param = Ident::new(
                lambda_env.borrow_mut().define(param.name().clone()),
                *param.span(),
            );
            Ok(Expr::new(
                ExprKind::Lambda {
                    param,
                    expr: resolve_expr(lambda_env.clone(), expr)?,
                },
                *expr.span(),
            ))
        }
        parse::ExprKind::Unit => Ok(Expr::new(ExprKind::Unit, *expr.span())),
    }
}

mod tests {
    use crate::{parse::parse, rename::resolve};

    use super::Env;

    fn test_helper(src: &str) -> super::Root {
        let ast = parse(src).expect("parse errors");
        let (res, errors) = resolve(Env::new(), &ast);
        if !errors.is_empty() {
            panic!("resolve error: {:?}", errors);
        }
        res.unwrap()
    }

    #[test]
    fn res_let() {
        insta::assert_debug_snapshot!(test_helper("let x = 1"));
    }

    #[test]
    fn res_let_error() {
        let ast = parse("let x = x").expect("parse errors");
        let (_, errors) = resolve(Env::new(), &ast);
        assert!(!errors.is_empty());
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
