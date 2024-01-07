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

pub type ResResult<T> = Result<T, Vec<ResError>>;

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

    pub fn find(&self, name: &InternedString) -> Option<UniqueId> {
        if let Some(id) = self.data.get(name) {
            Some(*id)
        } else if let Some(parent) = &self.parent {
            parent.borrow().find(name)
        } else {
            None
        }
    }

    pub fn find_in_scope(&self, name: &InternedString) -> Option<UniqueId> {
        self.data.get(name).copied()
    }

    pub fn find_in_parent(&self, name: &InternedString) -> Option<UniqueId> {
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

    pub fn insert(&mut self, name: InternedString, id: UniqueId) {
        self.data.insert(name, id);
    }

    pub fn define_if_absent(&mut self, name: InternedString) -> UniqueId {
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

pub fn resolve(env: Rc<RefCell<Env>>, root: &parse::Root) -> , Vec<ResError>) {
    let mut errors = vec![];
    let mut items = vec![];
    for item in root.inner().clone().items {
        match resolve_item(env.clone(), item) {
            Ok(i) => {
                // trace!("env: {:#?}", env.borrow());
                items.push(i);
            }
            Err(err) => {
                trace!("env: {:#?}", env.borrow());
                errors.push(err)
            }
        }
    }
    if items.is_empty() {
        (None, errors)
    } else {
        (Some(Node::new(Root { items }, root.span())), errors)
    }
}

fn resolve_decl(env: Rc<RefCell<Env>>, decl: parse::Decl) -> ResResult<Decl> {
    trace!("decl env: {:#?}", env.borrow());
    match decl.kind() {
        parse::Item::Def { pat, expr } => {
            // let def_env = Env::new_with_parent(env.clone());
            let expr = resolve_expr(env.clone(), expr)?;
            let pat = resolve_pattern(env.clone(), pat)?;
            trace!("def env: {:#?}", env.borrow());
            Ok(Node::new(Item::Def { pat, expr }, item.span()))
        }
        ast::Item::Fn { name, params, body } => {
            let name = Node::new(env.borrow_mut().define(name.inner().clone()), name.span());
            let mut new_params = vec![];
            let fn_env = Env::new_with_parent(env.clone());
            for p in params {
                let pat = resolve_pattern(fn_env.clone(), &p)?;
                new_params.push(pat);
            }
            let body = resolve_expr(fn_env.clone(), &body)?;
            Ok(Node::new(
                Item::Fn {
                    name,
                    params: new_params,
                    body,
                },
                item.span(),
            ))
        }
        ast::Item::Expr(expr) => {
            let expr = resolve_expr(env.clone(), &Node::new(expr.clone(), item.span()))?;
            Ok(Node::new(Item::Expr(expr.inner().clone()), item.span()))
        }
    }
}

fn resolve_expr(env: Rc<RefCell<Env>>, expr: &Node<ast::Expr>) -> ResResult<Node<Expr>> {
    match expr.inner() {
        ast::Expr::Ident(ident) => {
            if let Some(name) = env.borrow().find(ident) {
                Ok(Node::new(
                    Expr::Ident(Node::new(name, expr.span())),
                    expr.span(),
                ))
            } else {
                Err(ResError {
                    msg: InternedString::from(&*format!(
                        "name '{:?}' is not defined",
                        expr.inner()
                    )),
                    span: expr.span(),
                })
            }
        }
        ast::Expr::Lit(l) => Ok(Node::new(
            Expr::Lit(Node::new(l.clone().into(), expr.span())),
            expr.span(),
        )),
        ast::Expr::Let { pat, expr, body } => {
            let let_env = Env::new_with_parent(env.clone());
            let expr = resolve_expr(let_env.clone(), &expr.clone())?;
            let pat = resolve_pattern(let_env.clone(), pat)?;
            trace!("let env: {:#?}", let_env.borrow());
            let body = resolve_expr(let_env.clone(), body)?;
            Ok(Node::new(
                Expr::Let {
                    pat,
                    expr: expr.clone(),
                    body,
                },
                expr.span(),
            ))
        }
        ast::Expr::Fn {
            name,
            params,
            expr,
            body,
        } => {
            let name = Node::new(env.borrow_mut().define(name.inner().clone()), name.span());
            let mut new_params = vec![];
            let fn_env = Env::new_with_parent(env.clone());
            for p in params {
                let pat = resolve_pattern(fn_env.clone(), p)?;
                new_params.push(pat);
            }
            let expr = resolve_expr(fn_env.clone(), expr)?;
            let body = resolve_expr(fn_env, body)?;
            Ok(Node::new(
                Expr::Fn {
                    name,
                    params: new_params,
                    expr: expr.clone(),
                    body,
                },
                expr.span(),
            ))
        }
        ast::Expr::Apply { fun, args } => {
            let fun = resolve_expr(env.clone(), fun)?;
            let args = args
                .iter()
                .map(|arg| resolve_expr(env.clone(), arg))
                .collect::<ResResult<Vec<_>>>()?;
            Ok(Node::new(
                Expr::Apply {
                    fun: fun.clone(),
                    args,
                },
                expr.span(),
            ))
        }
        ast::Expr::Lambda { params, body } => {
            let mut new_params = vec![];
            let lambda_env = Env::new_with_parent(env.clone());
            for p in params {
                let pat = resolve_pattern(lambda_env.clone(), p)?;
                new_params.push(pat);
            }
            let body = resolve_expr(lambda_env, body)?;
            Ok(Node::new(
                Expr::Lambda {
                    params: new_params,
                    body,
                },
                expr.span(),
            ))
        }
        ast::Expr::Match { expr, cases } => {
            let expr = resolve_expr(env.clone(), expr)?;
            let cases = cases
                .iter()
                .map(|case| {
                    let pattern = resolve_pattern(env.clone(), &case.pattern)?;
                    let expr = resolve_expr(env.clone(), &case.expr)?;
                    Ok(Node::new(
                        MatchCase {
                            pattern: pattern.clone(),
                            expr: expr.clone(),
                        },
                        case.span(),
                    ))
                })
                .collect::<ResResult<Vec<_>>>()?;
            Ok(Node::new(
                Expr::Match {
                    expr: expr.clone(),
                    cases,
                },
                expr.span(),
            ))
        }
        ast::Expr::If { cond, then, else_ } => {
            let cond = resolve_expr(env.clone(), cond)?;
            let then = resolve_expr(env.clone(), then)?;
            let else_ = resolve_expr(env, else_)?;
            Ok(Node::new(
                Expr::If {
                    cond: cond.clone(),
                    then: then.clone(),
                    else_: else_.clone(),
                },
                expr.span(),
            ))
        }
        ast::Expr::Prefix { op, expr } => {
            let op_id = Node::new(
                env.borrow()
                    .find(&InternedString::from(op.to_string()))
                    .unwrap(),
                op.span(),
            );
            let expr = resolve_expr(env, expr)?;
            Ok(Node::new(
                Expr::Apply {
                    fun: Node::new(Expr::Ident(op_id), op.span()),
                    args: vec![expr.clone()],
                },
                expr.span(),
            ))
        }
        ast::Expr::Infix { op, lhs, rhs } => {
            let op_id = Node::new(
                env.borrow()
                    .find(&InternedString::from(op.to_string()))
                    .unwrap(),
                op.span(),
            );
            let lhs = resolve_expr(env.clone(), lhs)?;
            let rhs = resolve_expr(env, rhs)?;
            Ok(Node::new(
                Expr::Apply {
                    fun: Node::new(Expr::Ident(op_id), op.span()),
                    args: vec![lhs.clone(), rhs.clone()],
                },
                expr.span(),
            ))
        }
        ast::Expr::Unit => Ok(Node::new(Expr::Unit, expr.span())),
    }
}

fn resolve_pattern(
    env: Rc<RefCell<Env>>,
    pattern: &Node<ast::Pattern>,
) -> ResResult<Node<Pattern>> {
    match pattern.inner() {
        ast::Pattern::Lit(l) => Ok(Node::new(Pattern::Lit(l.clone().into()), pattern.span())),
        ast::Pattern::Ident(ident) => {
            let name = env.borrow_mut().define(ident.clone());
            Ok(Node::new(Pattern::Ident(name), pattern.span()))
        }
        ast::Pattern::Wildcard => Ok(Node::new(Pattern::Wildcard, pattern.span())),
        ast::Pattern::Unit => Ok(Node::new(Pattern::Unit, pattern.span())),
    }
}

mod tests {
    fn test_helper(src: &str) -> common::node::Node<super::Root> {
        let (ast, errors) = syntax::chumsky_parser::parse(src);
        if !errors.is_empty() {
            panic!("parse error: {:?}", errors);
        }
        let (res, errors) = super::resolve(super::Env::new(), &ast.unwrap());
        if !errors.is_empty() {
            panic!("resolve error: {:?}", errors);
        }
        res.unwrap()
    }

    #[test]
    fn res_let() {
        insta::assert_debug_snapshot!(test_helper("let x = 1 in x"));
    }

    #[test]
    fn res_def() {
        insta::assert_debug_snapshot!(test_helper("x = 1"));
    }

    #[test]
    fn res_def_error() {
        let (ast, errors) = syntax::chumsky_parser::parse("x = x");
        if !errors.is_empty() {
            panic!("parse error: {:?}", errors);
        }
        let (_, errors) = super::resolve(super::Env::new(), &ast.unwrap());
        assert!(!errors.is_empty());
    }

    #[test]
    fn res_nested() {
        insta::assert_debug_snapshot!(test_helper("let x = 1 in let y = 2 in x + y"));
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
