use super::token::Token;
use crate::util::{intern::InternedString, node::SrcNode};
use num_complex::Complex64;
use num_rational::Rational64;
use std::{
    fmt::{Debug, Display},
    num::ParseIntError,
    str::FromStr,
};

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

impl Display for PrefixOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PrefixOp::Neg => write!(f, "-"),
            PrefixOp::Not => write!(f, "!"),
        }
    }
}

impl From<Token> for PrefixOp {
    fn from(kind: Token) -> Self {
        match kind {
            Token::Minus => PrefixOp::Neg,
            Token::Not => PrefixOp::Not,
            _ => panic!("Not a prefix operator: {:?}", kind),
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

impl Display for InfixOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            InfixOp::Add => write!(f, "+"),
            InfixOp::Sub => write!(f, "-"),
            InfixOp::Mul => write!(f, "*"),
            InfixOp::Div => write!(f, "/"),
            InfixOp::Rem => write!(f, "%"),
            InfixOp::Pow => write!(f, "^"),
            InfixOp::Eq => write!(f, "="),
            InfixOp::Neq => write!(f, "!="),
            InfixOp::Lt => write!(f, "<"),
            InfixOp::Gt => write!(f, ">"),
            InfixOp::Leq => write!(f, "<="),
            InfixOp::Geq => write!(f, ">="),
            InfixOp::And => write!(f, "&&"),
            InfixOp::Or => write!(f, "||"),
            InfixOp::Pipe => write!(f, "|>"),
            InfixOp::Stmt => write!(f, ";"),
        }
    }
}

impl From<Token> for InfixOp {
    fn from(kind: Token) -> Self {
        match kind {
            Token::Plus => InfixOp::Add,
            Token::Minus => InfixOp::Sub,
            Token::Star => InfixOp::Mul,
            Token::Slash => InfixOp::Div,
            Token::Percent => InfixOp::Rem,
            Token::Caret => InfixOp::Pow,
            Token::Eq => InfixOp::Eq,
            Token::Neq => InfixOp::Neq,
            Token::Lt => InfixOp::Lt,
            Token::Gt => InfixOp::Gt,
            Token::Leq => InfixOp::Leq,
            Token::Geq => InfixOp::Geq,
            Token::And => InfixOp::And,
            Token::Or => InfixOp::Or,
            Token::Pipe => InfixOp::Pipe,
            _ => panic!("Not an infix operator: {:?}", kind),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Lit {
    Nat(Nat),
    Int(Int),
    Rational(Rational64),
    Real(f64),
    Complex(Complex64),
    Char(char),
    String(InternedString),
}

impl Display for Lit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Lit::Nat(n) => write!(f, "{}", n),
            Lit::Int(i) => write!(f, "{}", i),
            Lit::Rational(r) => write!(f, "{}", r),
            Lit::Real(r) => write!(f, "{}", r),
            Lit::Complex(c) => write!(f, "{}", c),
            Lit::Char(c) => write!(f, "'{}'", c),
            Lit::String(s) => write!(f, "{}", s),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Int(pub i64);

impl Display for Int {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl FromStr for Int {
    type Err = ParseIntError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.starts_with("0x") {
            i64::from_str_radix(&s[2..], 16).map(Int)
        } else if s.starts_with("0o") {
            i64::from_str_radix(&s[2..], 8).map(Int)
        } else if s.starts_with("0b") {
            i64::from_str_radix(&s[2..], 2).map(Int)
        } else {
            i64::from_str(s).map(Int)
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Nat(pub u64);

impl Display for Nat {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl FromStr for Nat {
    type Err = ParseIntError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.starts_with("0x") {
            u64::from_str_radix(&s[2..], 16).map(Nat)
        } else if s.starts_with("0o") {
            u64::from_str_radix(&s[2..], 8).map(Nat)
        } else if s.starts_with("0b") {
            u64::from_str_radix(&s[2..], 2).map(Nat)
        } else {
            u64::from_str(s).map(Nat)
        }
    }
}
