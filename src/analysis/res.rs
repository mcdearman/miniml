use crate::{
    syntax::{ast, node::SrcNode},
    util::{intern::InternedString, unique_id::UniqueId},
};
use num_complex::Complex64;
use num_rational::Rational64;
use std::{collections::HashMap, hash::Hash};

/*
 * This module resolves names in the AST and produces an IR similar
 * to the AST but with all names resolved to their unique IDs.
 */

#[derive(Debug, Clone, PartialEq)]
pub struct Names {
    data: HashMap<InternedString, UniqueId>,
}

impl Names {
    pub fn new() -> Self {
        Self {
            data: HashMap::new(),
        }
    }

    // pub fn get(&self, name: &InternedString) -> Option<UniqueId> {
    //     self.data.get(name).copied()
    // }

    pub fn insert_if_absent(&mut self, name: InternedString) -> UniqueId {
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

fn resolve_expr(names: &mut Names, expr: &ast::Expr) -> Expr {
    match expr {
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

fn resolve_pattern(names: &mut Names, pat: &ast::Pattern) -> Pattern {
    match pat {
        ast::Pattern::Ident(id) => Pattern::Ident(SrcNode::new(
            names.insert_if_absent(id.inner().clone()),
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
