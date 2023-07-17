use super::token::TokenKind;
use crate::intern::InternedString;
use itertools::join;
use num_bigint::BigInt;
use std::fmt::Display;

#[derive(Debug, Clone, PartialEq)]
pub struct File {
    pub items: Vec<Expr>,
}

impl Display for File {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", join(self.clone().items, "\n"))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Ident(InternedString),
    Int(BigInt),
    Prefix {
        op: PrefixOp,
        expr: Box<Self>,
    },
    Infix {
        op: InfixOp,
        lhs: Box<Self>,
        rhs: Box<Self>,
    },
    Let {
        name: InternedString,
        expr: Box<Self>,
        body: Option<Box<Self>>,
    },
    Fn {
        name: InternedString,
        param: InternedString,
        body: Box<Self>,
    },
    Apply {
        fun: Box<Self>,
        arg: Box<Self>,
    },
    If {
        cond: Box<Self>,
        then: Box<Self>,
        else_: Box<Self>,
    },
    Lambda {
        param: InternedString,
        body: Box<Self>,
    },
    Unit,
    Error,
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Ident(i) => write!(f, "{}", i),
            Expr::Prefix { op, expr } => write!(f, "({} {})", op, expr),
            Expr::Infix { op, lhs, rhs } => write!(f, "({} {} {})", lhs, op, rhs),
            Expr::Let { name, expr } => write!(f, "let {} = {}", name, expr),
            Expr::Fn(fun) => write!(f, "{}", fun),
            Expr::Apply(lhs, rhs) => write!(f, "({} {})", lhs, rhs),
            Expr::If { cond, then, else_ } => {
                write!(f, "(if {} then {} else {})", cond, then, else_)
            }
            Expr::Match { expr, arms } => {
                write!(f, "(match {} with ", expr)?;
                for arm in arms {
                    write!(f, "{}", arm)?;
                }
                write!(f, ")")
            }
            Expr::List(l) => write!(f, "{:?}", l.clone()),
            Expr::Tuple(t) => write!(f, "{}", t),
            Expr::Map(m) => write!(f, "{}", m),
            Expr::Record(r) => write!(f, "{}", r),
            Expr::Lambda(l) => write!(f, "{}", l),
            Expr::Unit => write!(f, "()"),
            Expr::Error => write!(f, "error"),
        }
    }
}

impl Display for Lambda {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "\\{} -> {}", self.param, self.body)
    }
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

impl From<TokenKind> for PrefixOp {
    fn from(kind: TokenKind) -> Self {
        match kind {
            TokenKind::Sub => PrefixOp::Neg,
            TokenKind::Not => PrefixOp::Not,
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
    Mod,
    Pow,
    Eq,
    Neq,
    Lss,
    Gtr,
    Leq,
    Geq,
    And,
    Or,
    Pipe,
}

impl Display for InfixOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            InfixOp::Add => write!(f, "+"),
            InfixOp::Sub => write!(f, "-"),
            InfixOp::Mul => write!(f, "*"),
            InfixOp::Div => write!(f, "/"),
            InfixOp::Mod => write!(f, "%"),
            InfixOp::Pow => write!(f, "^"),
            InfixOp::Eq => write!(f, "="),
            InfixOp::Neq => write!(f, "!="),
            InfixOp::Lss => write!(f, "<"),
            InfixOp::Gtr => write!(f, ">"),
            InfixOp::Leq => write!(f, "<="),
            InfixOp::Geq => write!(f, ">="),
            InfixOp::And => write!(f, "&&"),
            InfixOp::Or => write!(f, "||"),
            InfixOp::Pipe => write!(f, "|>"),
        }
    }
}

impl From<TokenKind> for InfixOp {
    fn from(kind: TokenKind) -> Self {
        match kind {
            TokenKind::Add => InfixOp::Add,
            TokenKind::Sub => InfixOp::Sub,
            TokenKind::Mul => InfixOp::Mul,
            TokenKind::Div => InfixOp::Div,
            TokenKind::Rem => InfixOp::Mod,
            TokenKind::Pow => InfixOp::Pow,
            TokenKind::Eql => InfixOp::Eq,
            TokenKind::Neq => InfixOp::Neq,
            TokenKind::Lss => InfixOp::Lss,
            TokenKind::Gtr => InfixOp::Gtr,
            TokenKind::Leq => InfixOp::Leq,
            TokenKind::Geq => InfixOp::Geq,
            TokenKind::And => InfixOp::And,
            TokenKind::Or => InfixOp::Or,
            TokenKind::Pipe => InfixOp::Pipe,
            _ => panic!("Not an infix operator: {:?}", kind),
        }
    }
}
