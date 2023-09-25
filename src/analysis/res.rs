/*
 * This module resolves names in the AST and produces an IR similar
 * to the AST but with all names resolved to their unique IDs. Names
 * that shadow names from an outer scope are given a new unique ID.
 */

use crate::{
    syntax::ast,
    util::{intern::InternedString, node::SrcNode, span::Span, unique_id::UniqueId},
};
use num_complex::Complex64;
use num_rational::Rational64;
use std::{collections::HashMap, hash::Hash};

#[derive(Debug, Clone, PartialEq)]
pub struct ResError {
    pub msg: InternedString,
    pub span: Span,
}

pub type ResResult<T> = Result<T, ResError>;

#[derive(Debug, Clone, PartialEq)]
pub struct Env {
    parent: Option<Box<Env>>,
    data: HashMap<InternedString, UniqueId>,
}

impl Env {
    pub fn new() -> Box<Self> {
        Box::new(Self {
            parent: None,
            data: HashMap::new(),
        })
    }

    pub fn new_with_parent(parent: Box<Self>) -> Box<Self> {
        Box::new(Self {
            parent: Some(parent),
            data: HashMap::new(),
        })
    }

    pub fn find(&self, name: &InternedString) -> Option<UniqueId> {
        if let Some(id) = self.data.get(name) {
            Some(*id)
        } else if let Some(parent) = &self.parent {
            parent.find(name)
        } else {
            None
        }
    }

    pub fn find_in_scope(&self, name: &InternedString) -> Option<UniqueId> {
        self.data.get(name).copied()
    }

    pub fn find_in_parent(&self, name: &InternedString) -> Option<UniqueId> {
        if let Some(parent) = &self.parent {
            parent.find(name)
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
    pub decls: Vec<SrcNode<Decl>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Decl {
    Let {
        name: SrcNode<UniqueId>,
        expr: SrcNode<Expr>,
    },
    Fn {
        name: SrcNode<UniqueId>,
        params: Vec<SrcNode<UniqueId>>,
        expr: SrcNode<Expr>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Ident(SrcNode<UniqueId>),
    Lit(SrcNode<Lit>),
    Prefix {
        op: SrcNode<PrefixOp>,
        expr: SrcNode<Self>,
    },
    Infix {
        op: SrcNode<InfixOp>,
        lhs: SrcNode<Self>,
        rhs: SrcNode<Self>,
    },
    Let {
        name: SrcNode<UniqueId>,
        expr: SrcNode<Self>,
        body: SrcNode<Self>,
    },
    Fn {
        name: SrcNode<UniqueId>,
        params: Vec<SrcNode<UniqueId>>,
        expr: SrcNode<Self>,
        body: SrcNode<Self>,
    },
    Apply {
        fun: SrcNode<Self>,
        args: Vec<SrcNode<Self>>,
    },
    If {
        cond: SrcNode<Self>,
        then: SrcNode<Self>,
        elifs: Vec<(SrcNode<Self>, SrcNode<Self>)>,
        else_: SrcNode<Self>,
    },
    Match {
        expr: SrcNode<Self>,
        cases: Vec<SrcNode<MatchCase>>,
    },
    Lambda {
        params: Vec<SrcNode<UniqueId>>,
        body: SrcNode<Self>,
    },
    Unit,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MatchCase {
    pub pattern: SrcNode<Pattern>,
    pub body: SrcNode<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    Ident(SrcNode<UniqueId>),
    Lit(SrcNode<Lit>),
    List {
        items: Vec<SrcNode<Self>>,
    },
    Cons {
        head: SrcNode<Self>,
        tail: SrcNode<Self>,
    },
    Wildcard,
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
    Rem,
    Pow,
    Eq,
    Neq,
    Lt,
    Gt,
    Leq,
    Geq,
    And,
    Or,
    Pipe,
    Stmt,
}

impl From<ast::InfixOp> for InfixOp {
    fn from(op: ast::InfixOp) -> Self {
        match op {
            ast::InfixOp::Add => Self::Add,
            ast::InfixOp::Sub => Self::Sub,
            ast::InfixOp::Mul => Self::Mul,
            ast::InfixOp::Div => Self::Div,
            ast::InfixOp::Rem => Self::Rem,
            ast::InfixOp::Pow => Self::Pow,
            ast::InfixOp::Eq => Self::Eq,
            ast::InfixOp::Neq => Self::Neq,
            ast::InfixOp::Lt => Self::Lt,
            ast::InfixOp::Gt => Self::Gt,
            ast::InfixOp::Leq => Self::Leq,
            ast::InfixOp::Geq => Self::Geq,
            ast::InfixOp::And => Self::And,
            ast::InfixOp::Or => Self::Or,
            ast::InfixOp::Pipe => Self::Pipe,
            ast::InfixOp::Stmt => Self::Stmt,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Lit {
    Nat(u64),
    Int(i64),
    Rational(Rational64),
    Real(f64),
    Complex(Complex64),
    Char(char),
    String(InternedString),
}

impl From<ast::Lit> for Lit {
    fn from(lit: ast::Lit) -> Self {
        match lit {
            ast::Lit::Nat(n) => Self::Nat(n.0),
            ast::Lit::Int(n) => Self::Int(n.0),
            ast::Lit::Rational(n) => Self::Rational(n),
            ast::Lit::Real(n) => Self::Real(n),
            ast::Lit::Complex(n) => Self::Complex(n),
            ast::Lit::Char(n) => Self::Char(n),
            ast::Lit::String(n) => Self::String(n),
        }
    }
}

pub fn resolve(root: &ast::Root) -> (Option<Root>, Vec<ResError>) {
    let env = Env::new();
    let mut errors = vec![];
    let mut decls = vec![];
    for decl in &root.decls {
        match resolve_decl(Env::new_with_parent(env.clone()), decl) {
            Ok(decl) => decls.push(decl),
            Err(err) => errors.push(err),
        }
    }
    if decls.is_empty() {
        (None, errors)
    } else {
        (Some(Root { decls }), errors)
    }
}

fn resolve_decl(mut env: Box<Env>, decl: &SrcNode<ast::Decl>) -> ResResult<SrcNode<Decl>> {
    match decl.inner() {
        ast::Decl::Let { name, expr } => {
            if env.find(name.inner()).is_some() {
                Err(ResError {
                    msg: format!("name '{}' is already defined", name.inner()).into(),
                    span: name.span(),
                })
            } else {
                let name = SrcNode::new(env.define(name.inner().clone()), name.span());
                let expr = resolve_expr(Env::new_with_parent(env.clone()), expr)?;
                Ok(SrcNode::new(Decl::Let { name, expr }, decl.span()))
            }
        }
        ast::Decl::Fn { name, params, expr } => {
            let mut param_names = vec![];
            let mut fn_env = Env::new_with_parent(env.clone());
            for param in params {
                let name = fn_env.define(param.inner().clone());
                param_names.push(SrcNode::new(name, param.span()));
            }
            let name_id = env.define(name.inner().clone());
            fn_env.insert(name.inner().clone(), name_id);
            let name = SrcNode::new(name_id, name.span());
            let expr = resolve_expr(fn_env.clone(), expr)?;
            Ok(SrcNode::new(
                Decl::Fn {
                    name,
                    params: param_names,
                    expr,
                },
                decl.span(),
            ))
        }
    }
}

fn resolve_expr(mut env: Box<Env>, expr: &SrcNode<ast::Expr>) -> ResResult<SrcNode<Expr>> {
    match expr.inner() {
        ast::Expr::Ident(ident) => {
            if let Some(name) = env.find_in_scope(ident.inner()) {
                Ok(SrcNode::new(
                    Expr::Ident(SrcNode::new(name, ident.span())),
                    expr.span(),
                ))
            } else {
                Err(ResError {
                    msg: format!("name '{}' is not defined", ident.inner()).into(),
                    span: ident.span(),
                })
            }
        }
        ast::Expr::Lit(l) => Ok(SrcNode::new(
            Expr::Lit(SrcNode::new(l.inner().clone().into(), l.span())),
            expr.span(),
        )),
        ast::Expr::Prefix { op, expr } => {
            let op = SrcNode::new(op.inner().clone().into(), op.span());
            let expr = resolve_expr(env, expr)?;
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
            let lhs = resolve_expr(env.clone(), lhs)?;
            let rhs = resolve_expr(env, rhs)?;
            Ok(SrcNode::new(
                Expr::Infix {
                    op,
                    lhs: lhs.clone(),
                    rhs: rhs.clone(),
                },
                expr.span(),
            ))
        }
        ast::Expr::Let { name, expr, body } => {
            let name = SrcNode::new(env.define(name.inner().clone()), name.span());
            let expr = resolve_expr(Env::new_with_parent(env.clone()), expr)?;
            let body = resolve_expr(env, body)?;
            Ok(SrcNode::new(
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
            let mut param_names = vec![];
            let mut fn_env = Env::new_with_parent(env.clone());
            for param in params {
                let name = fn_env.define(param.inner().clone());
                param_names.push(SrcNode::new(name, param.span()));
            }
            let name_id = env.define(name.inner().clone());
            fn_env.insert(name.inner().clone(), name_id);
            let name = SrcNode::new(name_id, name.span());
            let expr = resolve_expr(fn_env.clone(), expr)?;
            let body = resolve_expr(fn_env, body)?;
            Ok(SrcNode::new(
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
            let fun = resolve_expr(env.clone(), fun)?;
            let args = args
                .iter()
                .map(|arg| resolve_expr(env.clone(), arg))
                .collect::<ResResult<Vec<_>>>()?;
            Ok(SrcNode::new(
                Expr::Apply {
                    fun: fun.clone(),
                    args,
                },
                expr.span(),
            ))
        }
        ast::Expr::If {
            cond,
            then,
            elifs,
            else_,
        } => {
            let cond = resolve_expr(env.clone(), cond)?;
            let then = resolve_expr(env.clone(), then)?;
            let elifs = elifs
                .iter()
                .map(|(cond, body)| {
                    let cond = resolve_expr(env.clone(), cond)?;
                    let body = resolve_expr(env.clone(), body)?;
                    Ok((cond, body))
                })
                .collect::<ResResult<Vec<_>>>()?;
            let else_ = resolve_expr(env, else_)?;
            Ok(SrcNode::new(
                Expr::If {
                    cond: cond.clone(),
                    then: then.clone(),
                    elifs,
                    else_: else_.clone(),
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
                    let body = resolve_expr(env.clone(), &case.body)?;
                    Ok(SrcNode::new(
                        MatchCase {
                            pattern,
                            body: body.clone(),
                        },
                        case.span(),
                    ))
                })
                .collect::<ResResult<Vec<_>>>()?;
            Ok(SrcNode::new(
                Expr::Match {
                    expr: expr.clone(),
                    cases,
                },
                expr.span(),
            ))
        }
        ast::Expr::Lambda { params, body } => {
            let mut params = params.clone();
            let mut param_names = vec![];
            let mut lambda_env = Env::new_with_parent(env.clone());
            for param in &mut params {
                let name = lambda_env.define(param.inner().clone());
                param_names.push(SrcNode::new(name, param.span()));
            }
            let body = resolve_expr(env, body)?;
            Ok(SrcNode::new(
                Expr::Lambda {
                    params: param_names,
                    body,
                },
                expr.span(),
            ))
        }
        ast::Expr::Unit => Ok(SrcNode::new(Expr::Unit, expr.span())),
    }
}

fn resolve_pattern(mut env: Box<Env>, pat: &SrcNode<ast::Pattern>) -> ResResult<SrcNode<Pattern>> {
    match pat.inner() {
        ast::Pattern::Ident(id) => Ok(SrcNode::new(
            Pattern::Ident(SrcNode::new(
                env.define_if_absent(id.inner().clone()),
                id.span(),
            )),
            pat.span(),
        )),
        ast::Pattern::Lit(lit) => Ok(SrcNode::new(
            Pattern::Lit(SrcNode::new(lit.inner().clone().into(), lit.span())),
            pat.span(),
        )),
        // ast::Pattern::List(items) => Self::List {
        //     items: items.into_iter().map(Self::from).collect(),
        // },
        // ast::Pattern::Cons(head, tail) => Self::Cons {
        //     head: Box::new(Self::from(*head)),
        //     tail: Box::new(Self::from(*tail)),
        // },
        ast::Pattern::Wildcard => Ok(SrcNode::new(Pattern::Wildcard, pat.span())),
        ast::Pattern::Unit => Ok(SrcNode::new(Pattern::Unit, pat.span())),
        _ => todo!(),
    }
}
