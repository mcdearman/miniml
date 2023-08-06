use itertools::join;
use miniml_util::{
    intern::InternedString,
    span::{Span, Spanned},
};
use std::fmt::Display;

use crate::lex::Token;

#[derive(Debug, Clone, PartialEq)]
pub struct Root {
    pub decls: Vec<Spanned<Decl>>,
}

impl Root {
    pub fn spanned(self, span: Span) -> Spanned<Self> {
        (self, span)
    }
}

impl Display for Root {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            join(self.clone().decls.into_iter().map(|d| d.0), "\n")
        )
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Decl {
    Const {
        name: Spanned<InternedString>,
        expr: Box<Spanned<Expr>>,
    },
    Let {
        name: Spanned<InternedString>,
        expr: Box<Spanned<Expr>>,
    },
    Fn {
        name: Spanned<InternedString>,
        params: Vec<Spanned<InternedString>>,
        body: Box<Spanned<Expr>>,
    },
}

impl Decl {
    pub fn spanned(self, span: Span) -> Spanned<Self> {
        (self, span)
    }
}

impl Display for Decl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Decl::Const { name, expr } => write!(f, "const {} = {}", name.0, expr.0),
            Decl::Let { name, expr } => write!(f, "let {} = {}", name.0, expr.0),
            Decl::Fn { name, params, body } => {
                write!(
                    f,
                    "fn {} {} = {}",
                    name.0,
                    join(params.into_iter().map(|p| p.0), ", "),
                    body.0
                )
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Ident(InternedString),
    Lit(Lit),
    Prefix {
        op: Spanned<PrefixOp>,
        expr: Box<Spanned<Self>>,
    },
    Infix {
        op: Spanned<InfixOp>,
        lhs: Box<Spanned<Self>>,
        rhs: Box<Spanned<Self>>,
    },
    Let {
        name: Spanned<InternedString>,
        expr: Box<Spanned<Self>>,
        body: Box<Spanned<Self>>,
    },
    Apply {
        fun: Box<Spanned<Self>>,
        args: Vec<Spanned<Self>>,
    },
    If {
        cond: Box<Spanned<Self>>,
        then: Box<Spanned<Self>>,
        else_: Box<Spanned<Self>>,
    },
    Lambda {
        params: Vec<Spanned<InternedString>>,
        body: Box<Spanned<Self>>,
    },
    Unit,
    Error,
}

impl Expr {
    pub fn spanned(self, span: Span) -> Spanned<Self> {
        (self, span)
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.clone() {
            Expr::Ident(i) => write!(f, "{}", i),
            Expr::Lit(l) => write!(f, "{}", l),
            Expr::Prefix { op, expr } => write!(f, "{}{}", op.0, expr.0),
            Expr::Infix { op, lhs, rhs } => write!(f, "{} {} {}", lhs.0, op.0, rhs.0),
            Expr::Let { name, expr, body } => {
                write!(f, "let {} = {} in {}", name.0, expr.0, body.0)
            }
            Expr::Apply { fun, args } => {
                write!(f, "({}", fun.0)?;
                for arg in args {
                    write!(f, " {}", arg.0)?;
                }
                write!(f, ")")
            }
            Expr::If { cond, then, else_ } => {
                write!(f, "if {} then {} else {}", cond.0, then.0, else_.0)
            }
            Expr::Lambda { params, body } => write!(
                f,
                "\\{} => {}",
                join(params.into_iter().map(|p| p.0), " "),
                body.0
            ),
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

#[derive(Debug, Clone, PartialEq)]
pub enum Lit {
    Int(i64),
    Real(f64),
    String(InternedString),
}

impl Lit {
    pub fn spanned(self, span: Span) -> Spanned<Self> {
        (self, span)
    }
}

impl Display for Lit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Lit::Int(i) => write!(f, "{}", i),
            Lit::Real(r) => write!(f, "{}", r),
            Lit::String(s) => write!(f, "\"{}\"", s),
        }
    }
}
