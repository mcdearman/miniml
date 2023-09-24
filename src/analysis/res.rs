use crate::{
    compile::error,
    syntax::{ast, node::SrcNode},
    util::{intern::InternedString, span::Span, unique_id::UniqueId},
};
use chumsky::{extra::Err, span::SimpleSpan};
use num_complex::Complex64;
use num_rational::Rational64;
use std::{collections::HashMap, hash::Hash};

/*
 * This module resolves names in the AST and produces an IR similar
 * to the AST but with all names resolved to their unique IDs. Names
 * that shadow names from an outer scope are given a new unique ID.
 */

#[derive(Debug, Clone, PartialEq)]
pub struct ResError {
    pub msg: InternedString,
    pub span: SimpleSpan,
}

pub type ResResult<T> = Result<T, ResError>;

#[derive(Debug, Clone, PartialEq)]
pub struct Names {
    parent: Option<Box<Names>>,
    data: HashMap<InternedString, UniqueId>,
}

impl Names {
    pub fn new() -> Self {
        Self {
            parent: None,
            data: HashMap::new(),
        }
    }

    pub fn new_with_parent(parent: Self) -> Self {
        Self {
            parent: Some(Box::new(parent)),
            data: HashMap::new(),
        }
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
    let mut names = Names::new();
    // let errors = vec![];
    todo!()
    // let decls = root
    //     .decls
    //     .iter()
    //     .map(|decl| resolve_decl(&mut names, decl))
    //     .collect();
    // Root { decls }
}

fn resolve_decl(mut names: Box<Names>, decl: &SrcNode<ast::Decl>) -> ResResult<SrcNode<Decl>> {
    match decl.inner() {
        ast::Decl::Let { name, expr } => {
            if names.find(name.inner()).is_some() {
                Err(ResError {
                    msg: format!("name '{}' is already defined", name.inner()).into(),
                    span: name.span(),
                })
            } else {
                let name = SrcNode::new(names.define(name.inner().clone()), name.span());
                let expr = resolve_expr(names, expr);
                Ok(SrcNode::new(Decl::Let { name, expr }, decl.span()))
            }
        }
    }
}

fn resolve_expr(mut names: Box<Names>, expr: &SrcNode<ast::Expr>) -> SrcNode<Expr> {
    match expr.inner() {
        ast::Expr::Ident(_) => todo!(),
        ast::Expr::Lit(_) => todo!(),
        ast::Expr::Prefix { op, expr } => todo!(),
        ast::Expr::Infix { op, lhs, rhs } => todo!(),
        ast::Expr::Let { name, expr, body } => todo!(),
        ast::Expr::Apply { fun, args } => todo!(),
        ast::Expr::If {
            cond,
            then,
            elifs,
            else_,
        } => todo!(),
        ast::Expr::Match { expr, cases } => todo!(),
        ast::Expr::Lambda { params, body } => todo!(),
        ast::Expr::Unit => todo!(),
    }
}

fn resolve_pattern(mut names: Box<Names>, pat: &SrcNode<ast::Pattern>) -> Pattern {
    match pat.inner() {
        ast::Pattern::Ident(id) => Pattern::Ident(SrcNode::new(
            names.define_if_absent(id.inner().clone()),
            id.span(),
        )),
        ast::Pattern::Lit(lit) => {
            Pattern::Lit(SrcNode::new(lit.inner().clone().into(), lit.span()))
        }
        // ast::Pattern::List(items) => Self::List {
        //     items: items.into_iter().map(Self::from).collect(),
        // },
        // ast::Pattern::Cons(head, tail) => Self::Cons {
        //     head: Box::new(Self::from(*head)),
        //     tail: Box::new(Self::from(*tail)),
        // },
        ast::Pattern::Wildcard => Pattern::Wildcard,
        ast::Pattern::Unit => Pattern::Unit,
        _ => todo!(),
    }
}
