/*
 * This module resolves names in the AST and produces an IR similar
 * to the AST but with all names resolved to their unique IDs. Names
 * that shadow names from an outer scope are given a new unique ID.
 */
use crate::{
    syntax::ast,
    util::{intern::InternedString, node::SrcNode, span::Span, unique_id::UniqueId},
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
    pub items: Vec<SrcNode<Item>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Item {
    Def {
        name: SrcNode<UniqueId>,
        expr: SrcNode<Expr>,
    },
    Expr(Expr),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Lit(SrcNode<Lit>),
    Ident(SrcNode<UniqueId>),
    Lambda {
        params: Vec<SrcNode<UniqueId>>,
        body: SrcNode<Self>,
    },
    Apply {
        fun: SrcNode<Self>,
        args: Vec<SrcNode<Self>>,
    },
    Let {
        name: SrcNode<UniqueId>,
        expr: SrcNode<Self>,
        body: SrcNode<Self>,
    },
    If {
        cond: SrcNode<Self>,
        then: SrcNode<Self>,
        else_: SrcNode<Self>,
    },
    Prefix {
        op: SrcNode<PrefixOp>,
        expr: SrcNode<Self>,
    },
    Infix {
        op: SrcNode<InfixOp>,
        lhs: SrcNode<Self>,
        rhs: SrcNode<Self>,
    },
    Unit,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PrefixOp {
    Neg,
    Not,
}

impl From<ast::PrefixOp> for PrefixOp {
    fn from(op: ast::PrefixOp) -> Self {
        match op {
            ast::PrefixOp::Neg => Self::Neg,
            ast::PrefixOp::Not => Self::Not,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum InfixOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    // Pow,
    Eq,
    Neq,
    Lt,
    Gt,
    Leq,
    Geq,
    // And,
    // Or,
    // Pipe,
    // Stmt,
}

impl From<ast::InfixOp> for InfixOp {
    fn from(op: ast::InfixOp) -> Self {
        match op {
            ast::InfixOp::Add => Self::Add,
            ast::InfixOp::Sub => Self::Sub,
            ast::InfixOp::Mul => Self::Mul,
            ast::InfixOp::Div => Self::Div,
            ast::InfixOp::Mod => Self::Mod,
            // ast::InfixOp::Pow => Self::Pow,
            ast::InfixOp::Eq => Self::Eq,
            ast::InfixOp::Neq => Self::Neq,
            ast::InfixOp::Lt => Self::Lt,
            ast::InfixOp::Gt => Self::Gt,
            ast::InfixOp::Leq => Self::Leq,
            ast::InfixOp::Geq => Self::Geq,
            // ast::InfixOp::And => Self::And,
            // ast::InfixOp::Or => Self::Or,
            // ast::InfixOp::Pipe => Self::Pipe,
            // ast::InfixOp::Stmt => Self::Stmt,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Lit {
    Num(Rational64),
    Bool(bool),
}

impl From<ast::Lit> for Lit {
    fn from(lit: ast::Lit) -> Self {
        match lit {
            ast::Lit::Num(num) => Self::Num(num),
            ast::Lit::Bool(b) => Self::Bool(b),
        }
    }
}

pub fn resolve(
    env: Rc<RefCell<Env>>,
    root: &SrcNode<ast::Root>,
) -> (Option<SrcNode<Root>>, Vec<ResError>) {
    let mut errors = vec![];
    let mut items = vec![];
    for item in root.inner().clone().items {
        match resolve_item(env.clone(), &item) {
            Ok(i) => items.push(i),
            Err(err) => errors.push(err),
        }
    }
    if items.is_empty() {
        (None, errors)
    } else {
        (Some(SrcNode::new(Root { items }, root.span())), errors)
    }
}

fn resolve_item(env: Rc<RefCell<Env>>, item: &SrcNode<ast::Item>) -> ResResult<SrcNode<Item>> {
    match item.inner() {
        ast::Item::Def { name, expr } => {
            let name = SrcNode::new(env.borrow_mut().define(name.inner().clone()), name.span());
            let expr = resolve_expr(env.clone(), expr, false)?;
            Ok(SrcNode::new(
                Item::Def {
                    name,
                    expr: expr.clone(),
                },
                item.span(),
            ))
        }
        ast::Item::Expr(expr) => {
            let expr = resolve_expr(env.clone(), &SrcNode::new(expr.clone(), item.span()), false)?;
            Ok(SrcNode::new(Item::Expr(expr.inner().clone()), item.span()))
        }
    }
}

fn resolve_expr(
    env: Rc<RefCell<Env>>,
    expr: &SrcNode<ast::Expr>,
    rec: bool,
) -> ResResult<SrcNode<Expr>> {
    match expr.inner() {
        ast::Expr::Ident(ident) => {
            if rec {
                if let Some(name) = env.borrow().find(ident) {
                    Ok(SrcNode::new(
                        Expr::Ident(SrcNode::new(name, expr.span())),
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
            } else {
                if let Some(name) = env.borrow().find_in_scope(ident) {
                    Ok(SrcNode::new(
                        Expr::Ident(SrcNode::new(name, expr.span())),
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
        }
        ast::Expr::Lit(l) => Ok(SrcNode::new(
            Expr::Lit(SrcNode::new(l.clone().into(), expr.span())),
            expr.span(),
        )),
        ast::Expr::Let { name, expr, body } => {
            let name = SrcNode::new(env.borrow_mut().define(name.inner().clone()), name.span());
            let expr = resolve_expr(Env::new_with_parent(env.clone()), expr, rec)?;
            let body = resolve_expr(env, body, rec)?;
            Ok(SrcNode::new(
                Expr::Let {
                    name,
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
            Ok(SrcNode::new(
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
                param_names.push(SrcNode::new(name, param.span()));
            }
            let body = resolve_expr(lambda_env, body, true)?;
            Ok(SrcNode::new(
                Expr::Lambda {
                    params: param_names,
                    body,
                },
                expr.span(),
            ))
        }
        ast::Expr::If { cond, then, else_ } => {
            let cond = resolve_expr(env.clone(), cond, rec)?;
            let then = resolve_expr(env.clone(), then, rec)?;
            let else_ = resolve_expr(env, else_, rec)?;
            Ok(SrcNode::new(
                Expr::If {
                    cond: cond.clone(),
                    then: then.clone(),
                    else_: else_.clone(),
                },
                expr.span(),
            ))
        }
        ast::Expr::Prefix { op, expr } => {
            let op = SrcNode::new(op.inner().clone().into(), op.span());
            let expr = resolve_expr(env, expr, rec)?;
            Ok(SrcNode::new(
                Expr::Prefix {
                    op,
                    expr: expr.clone(),
                },
                expr.span(),
            ))
        }
        ast::Expr::Infix { op, lhs, rhs } => {
            let op = SrcNode::new(op.inner().clone().into(), op.span());
            let lhs = resolve_expr(env.clone(), lhs, rec)?;
            let rhs = resolve_expr(env, rhs, rec)?;
            Ok(SrcNode::new(
                Expr::Infix {
                    op,
                    lhs: lhs.clone(),
                    rhs: rhs.clone(),
                },
                expr.span(),
            ))
        }
        ast::Expr::Unit => Ok(SrcNode::new(Expr::Unit, expr.span())),
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
