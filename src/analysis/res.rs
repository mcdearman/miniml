/*
 * This module resolves names in the AST and produces an IR similar
 * to the AST but with all names resolved to their unique IDs. Names
 * that shadow names from an outer scope are given a new unique ID.
 */
use crate::{
    syntax::ast,
    util::{intern::InternedString, node::Node, span::Span, unique_id::UniqueId},
};
use num_rational::Rational64;
use std::{cell::RefCell, collections::HashMap, hash::Hash, rc::Rc};

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
    pub items: Vec<Node<Item>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Item {
    Def {
        name: Node<UniqueId>,
        expr: Node<Expr>,
    },
    Fn {
        name: Node<UniqueId>,
        params: Vec<Node<UniqueId>>,
        body: Node<Expr>,
    },
    Expr(Expr),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Lit(Node<Lit>),
    Ident(Node<UniqueId>),
    Lambda {
        params: Vec<Node<UniqueId>>,
        body: Node<Self>,
    },
    Apply {
        fun: Node<Self>,
        args: Vec<Node<Self>>,
    },
    Let {
        name: Node<UniqueId>,
        expr: Node<Self>,
        body: Node<Self>,
    },
    Fn {
        name: Node<UniqueId>,
        params: Vec<Node<UniqueId>>,
        expr: Node<Self>,
        body: Node<Self>,
    },
    Match {
        expr: Node<Self>,
        cases: Vec<Node<MatchCase>>,
    },
    If {
        cond: Node<Self>,
        then: Node<Self>,
        else_: Node<Self>,
    },
    Unit,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MatchCase {
    pub pattern: Node<Pattern>,
    pub expr: Node<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Lit {
    Num(Rational64),
    Bool(bool),
    String(InternedString),
}

impl From<ast::Lit> for Lit {
    fn from(lit: ast::Lit) -> Self {
        match lit {
            ast::Lit::Num(num) => Self::Num(num),
            ast::Lit::Bool(b) => Self::Bool(b),
            ast::Lit::String(s) => Self::String(s),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    Lit(Lit),
    Ident(UniqueId),
    Wildcard,
    Unit,
}

pub fn resolve(
    env: Rc<RefCell<Env>>,
    root: &Node<ast::Root>,
) -> (Option<Node<Root>>, Vec<ResError>) {
    let mut errors = vec![];
    let mut items = vec![];
    for item in root.inner().clone().items {
        match resolve_item(env.clone(), item) {
            Ok(i) => items.push(i),
            Err(err) => errors.push(err),
        }
    }
    if items.is_empty() {
        (None, errors)
    } else {
        (Some(Node::new(Root { items }, root.span())), errors)
    }
}

fn resolve_item(env: Rc<RefCell<Env>>, item: Node<ast::Item>) -> ResResult<Node<Item>> {
    match &*item {
        ast::Item::Def { name, expr } => {
            let name = Node::new(env.borrow_mut().define(name.inner().clone()), name.span());
            let expr = resolve_expr(env.clone(), expr, false)?;
            Ok(Node::new(Item::Def { name, expr }, item.span()))
        }
        // ast::Item::Def { name, expr } => Ok(item.try_map(|i| {
        //     Ok(Item::Def {
        //         name: name.map(|n| env.borrow_mut().define(n)),
        //         expr: expr
        //             .try_map(|e| resolve_expr(env.clone(), expr, false).map(|n| n.into_inner()))?,
        //     })
        // })?),
        ast::Item::Fn { name, params, body } => {
            let name = Node::new(env.borrow_mut().define(name.inner().clone()), name.span());
            let mut params = params.clone();
            let mut param_names = vec![];
            let fn_env = Env::new_with_parent(env.clone());
            for param in &mut params {
                let name = fn_env.borrow_mut().define(param.inner().clone());
                param_names.push(Node::new(name, param.span()));
            }
            let body = resolve_expr(fn_env, &body, true)?;
            Ok(Node::new(
                Item::Fn {
                    name,
                    params: param_names,
                    body,
                },
                item.span(),
            ))
        }
        ast::Item::Expr(expr) => {
            let expr = resolve_expr(env.clone(), &Node::new(expr.clone(), item.span()), false)?;
            Ok(Node::new(Item::Expr(expr.inner().clone()), item.span()))
        }
    }
}

fn resolve_expr(env: Rc<RefCell<Env>>, expr: &Node<ast::Expr>, rec: bool) -> ResResult<Node<Expr>> {
    match expr.inner() {
        ast::Expr::Ident(ident) => {
            if rec {
                if let Some(name) = env.borrow().find(ident) {
                    Ok(Node::new(
                        Expr::Ident(Node::new(name, expr.span())),
                        expr.span(),
                    ))
                // } else if &**ident == "_" {
                //     Ok(Node::new(
                //         Expr::Ident(Node::new(env.borrow_mut().define(*ident), expr.span())),
                //         expr.span(),
                //     ))
                } else {
                    Err(ResError {
                        msg: InternedString::from(&*format!(
                            "name '{:?}' is not defined",
                            expr.inner()
                        )),
                        span: expr.span(),
                    })
                }
            } else {
                if let Some(name) = env.borrow().find_in_scope(ident) {
                    Ok(Node::new(
                        Expr::Ident(Node::new(name, expr.span())),
                        expr.span(),
                    ))
                // } else if &**ident == "_" {
                //     Ok(Node::new(
                //         Expr::Ident(Node::new(env.borrow_mut().define(*ident), expr.span())),
                //         expr.span(),
                //     ))
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
        }
        ast::Expr::Lit(l) => Ok(Node::new(
            Expr::Lit(Node::new(l.clone().into(), expr.span())),
            expr.span(),
        )),
        ast::Expr::Let { pat, expr, body } => {
            let name = Node::new(env.borrow_mut().define(name.inner().clone()), name.span());
            let expr = resolve_expr(Env::new_with_parent(env.clone()), expr, rec)?;
            let body = resolve_expr(env, body, rec)?;
            Ok(Node::new(
                Expr::Let {
                    name,
                    expr: expr.clone(),
                    body: body.clone(),
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
            let mut params = params.clone();
            let mut param_names = vec![];
            let fn_env = Env::new_with_parent(env.clone());
            for param in &mut params {
                let name = fn_env.borrow_mut().define(param.inner().clone());
                param_names.push(Node::new(name, param.span()));
            }
            let expr = resolve_expr(fn_env.clone(), expr, true)?;
            let body = resolve_expr(fn_env, body, true)?;
            Ok(Node::new(
                Expr::Fn {
                    name,
                    params: param_names,
                    expr: expr.clone(),
                    body: body.clone(),
                },
                expr.span(),
            ))
        }
        ast::Expr::Apply { fun, args } => {
            let fun = resolve_expr(env.clone(), fun, rec)?;
            let args = args
                .iter()
                .map(|arg| resolve_expr(env.clone(), arg, rec))
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
            let mut params = params.clone();
            let mut param_names = vec![];
            let lambda_env = Env::new_with_parent(env.clone());
            for param in &mut params {
                let name = lambda_env.borrow_mut().define(param.inner().clone());
                param_names.push(Node::new(name, param.span()));
            }
            let body = resolve_expr(lambda_env, body, true)?;
            Ok(Node::new(
                Expr::Lambda {
                    params: param_names,
                    body,
                },
                expr.span(),
            ))
        }
        ast::Expr::Match { expr, cases } => {
            let expr = resolve_expr(env.clone(), expr, rec)?;
            let cases = cases
                .iter()
                .map(|case| {
                    let pattern = resolve_pattern(env.clone(), &case.pattern, rec)?;
                    let expr = resolve_expr(env.clone(), &case.expr, rec)?;
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
            let cond = resolve_expr(env.clone(), cond, rec)?;
            let then = resolve_expr(env.clone(), then, rec)?;
            let else_ = resolve_expr(env, else_, rec)?;
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
            let expr = resolve_expr(env, expr, rec)?;
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
            let lhs = resolve_expr(env.clone(), lhs, rec)?;
            let rhs = resolve_expr(env, rhs, rec)?;
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
    rec: bool,
) -> ResResult<Node<Pattern>> {
    match pattern.inner() {
        ast::Pattern::Lit(l) => Ok(Node::new(Pattern::Lit(l.clone().into()), pattern.span())),
        ast::Pattern::Ident(ident) => {
            if rec {
                if let Some(name) = env.borrow().find(ident) {
                    Ok(Node::new(Pattern::Ident(name), pattern.span()))
                } else {
                    Err(ResError {
                        msg: InternedString::from(&*format!(
                            "name '{:?}' is not defined",
                            pattern.inner()
                        )),
                        span: pattern.span(),
                    })
                }
            } else {
                if let Some(name) = env.borrow().find_in_scope(ident) {
                    Ok(Node::new(Pattern::Ident(name), pattern.span()))
                } else {
                    Err(ResError {
                        msg: InternedString::from(&*format!(
                            "name '{:?}' is not defined",
                            pattern.inner()
                        )),
                        span: pattern.span(),
                    })
                }
            }
        }
        ast::Pattern::Wildcard => Ok(Node::new(Pattern::Wildcard, pattern.span())),
        ast::Pattern::Unit => Ok(Node::new(Pattern::Unit, pattern.span())),
    }
}

mod tests {
    // #[test]
    // fn res_nested() {
    //     let (ast, errors) = crate::syntax::parse::parse("let x = 1 in let x = 2 in x + 1");
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
    // fn res_def() {
    //     let (ast, errors) = crate::syntax::parse::parse("x = 1");
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
    // fn res_def_error() {
    //     let (ast, errors) = crate::syntax::parse::parse("x = x");
    //     if !errors.is_empty() {
    //         panic!("parse error: {:?}", errors);
    //     }
    //     let (_, errors) = super::resolve(&ast.unwrap());
    //     assert!(!errors.is_empty());
    // }

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
    // fn res_let() {
    //     let (ast, errors) = crate::syntax::parse::parse("let x = 1 in x");
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
