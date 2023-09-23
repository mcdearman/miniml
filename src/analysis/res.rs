use std::sync::atomic::AtomicUsize;
use num_complex::Complex64;
use num_rational::Rational64;

use crate::{syntax::node::SrcNode, util::intern::InternedString};

/*
 * This module resolves names in the AST and produces an IR similar
 * to the AST but with all names resolved to their unique IDs.
 */

pub type Id = AtomicUsize;

#[derive(Debug, Clone, PartialEq)]
pub struct Root {
    pub decls: Vec<SrcNode<Decl>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Decl {
    Let {
        name: SrcNode<InternedString>,
        expr: SrcNode<Expr>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Ident(SrcNode<InternedString>),
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
        name: SrcNode<InternedString>,
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
        params: Vec<SrcNode<InternedString>>,
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
    Ident(SrcNode<InternedString>),
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
