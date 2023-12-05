/*
 * This module resolves names in the AST and produces an IR similar
 * to the AST but with all names resolved to their unique IDs. Names
 * that shadow names from an outer scope are given a new unique ID.
 */
use common::{intern::InternedString, node::Node, span::Span, unique_id::UniqueId};
use log::trace;
use num_rational::Rational64;
use std::{cell::RefCell, collections::HashMap, rc::Rc};
use syntax::ast;

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
        pat: Node<Pattern>,
        expr: Node<Expr>,
    },
    Fn {
        name: Node<UniqueId>,
        params: Vec<Node<Pattern>>,
        body: Node<Expr>,
    },
    Expr(Expr),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Lit(Node<Lit>),
    Ident(Node<UniqueId>),
    Lambda {
        params: Vec<Node<Pattern>>,
        body: Node<Self>,
    },
    Apply {
        fun: Node<Self>,
        args: Vec<Node<Self>>,
    },
    Let {
        pat: Node<Pattern>,
        expr: Node<Self>,
        body: Node<Self>,
    },
    Fn {
        name: Node<UniqueId>,
        params: Vec<Node<Pattern>>,
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

fn resolve_item(env: Rc<RefCell<Env>>, item: Node<ast::Item>) -> ResResult<Node<Item>> {
    trace!("item env: {:#?}", env.borrow());
    match &*item {
        ast::Item::Def { pat, expr } => {
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
    // rec: bool,
    // def: bool,
) -> ResResult<Node<Pattern>> {
    match pattern.inner() {
        ast::Pattern::Lit(l) => Ok(Node::new(Pattern::Lit(l.clone().into()), pattern.span())),
        ast::Pattern::Ident(ident) => {
            // if def {
            // if let Some(name) = env.borrow().find(ident) {
            //     return Err(ResError {
            //         msg: InternedString::from(&*format!(
            //             "name '{:?}' is already defined as {:?}",
            //             pattern.inner(),
            //             name
            //         )),
            //         span: pattern.span(),
            //     });
            // }
            let name = env.borrow_mut().define(ident.clone());
            Ok(Node::new(Pattern::Ident(name), pattern.span()))
            // } else {
            //     if rec {
            //         if let Some(name) = env.borrow().find(ident) {
            //             Ok(Node::new(Pattern::Ident(name), pattern.span()))
            //         } else {
            //             Err(ResError {
            //                 msg: InternedString::from(&*format!(
            //                     "name '{:?}' is not defined",
            //                     pattern.inner()
            //                 )),
            //                 span: pattern.span(),
            //             })
            //         }
            //     } else {
            //         if let Some(name) = env.borrow().find_in_scope(ident) {
            //             Ok(Node::new(Pattern::Ident(name), pattern.span()))
            //         } else {
            //             Err(ResError {
            //                 msg: InternedString::from(&*format!(
            //                     "name '{:?}' is not defined",
            //                     pattern.inner()
            //                 )),
            //                 span: pattern.span(),
            //             })
            //         }
            //     }
            // }
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
