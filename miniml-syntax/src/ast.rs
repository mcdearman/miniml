use itertools::join;
use miniml_util::{intern::InternedString, span::Spanned};
use std::fmt::Display;

use crate::lex::Token;

#[derive(Debug, Clone, PartialEq)]
pub struct Root {
    pub items: Vec<Spanned<Item>>,
}

impl Display for Root {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            join(self.clone().items.into_iter().map(|i| i.0), "\n")
        )
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Item {
    Expr(Spanned<Expr>),
}

impl Display for Item {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Item::Expr(expr) => write!(f, "{}", expr.0),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Ident(InternedString),
    Int(i64),
    Real(f64),
    String(InternedString),
    Prefix {
        op: PrefixOp,
        expr: Box<Spanned<Self>>,
    },
    // Infix {
    //     op: InfixOp,
    //     lhs: Box<Self>,
    //     rhs: Box<Self>,
    // },
    // Let {
    //     name: InternedString,
    //     expr: Box<Self>,
    //     body: Option<Box<Self>>,
    // },
    // Fn {
    //     name: InternedString,
    //     params: Vec<InternedString>,

    //     body: Box<Self>,
    // },
    // Apply {
    //     fun: Box<Self>,
    //     args: Vec<Self>,
    // },
    // If {
    //     cond: Box<Self>,
    //     then: Box<Self>,
    //     else_: Box<Self>,
    // },
    // Lambda {
    //     param: InternedString,
    //     body: Box<Self>,
    // },
    Unit,
    Error,
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Ident(i) => write!(f, "{}", i),
            Expr::Int(i) => write!(f, "{}", i),
            Expr::Real(r) => write!(f, "{}", r),
            Expr::String(s) => write!(f, "{}", s),
            Expr::Prefix { op, expr } => write!(f, "({}{})", op, expr.0),
            Expr::Unit => write!(f, "()"),
            Expr::Error => write!(f, "error"),
        }
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

impl From<Token> for PrefixOp {
    fn from(kind: Token) -> Self {
        match kind {
            Token::Sub => PrefixOp::Neg,
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

impl From<Token> for InfixOp {
    fn from(kind: Token) -> Self {
        match kind {
            Token::Add => InfixOp::Add,
            Token::Sub => InfixOp::Sub,
            Token::Mul => InfixOp::Mul,
            Token::Div => InfixOp::Div,
            Token::Rem => InfixOp::Mod,
            Token::Pow => InfixOp::Pow,
            Token::Eq => InfixOp::Eq,
            Token::Neq => InfixOp::Neq,
            Token::Lss => InfixOp::Lss,
            Token::Gtr => InfixOp::Gtr,
            Token::Leq => InfixOp::Leq,
            Token::Geq => InfixOp::Geq,
            Token::And => InfixOp::And,
            Token::Or => InfixOp::Or,
            Token::Pipe => InfixOp::Pipe,
            _ => panic!("Not an infix operator: {:?}", kind),
        }
    }
}
