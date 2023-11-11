use super::token::Token;
use crate::util::{intern::InternedString, node::SrcNode};
use num_rational::Rational64;

#[derive(Debug, Clone, PartialEq)]
pub struct Root {
    pub items: Vec<SrcNode<Item>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Item {
    Def {
        name: SrcNode<InternedString>,
        expr: SrcNode<Expr>,
    },
    Fn {
        name: SrcNode<InternedString>,
        params: Vec<SrcNode<InternedString>>,
        body: SrcNode<Expr>,
    },
    Expr(Expr),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Lit(Lit),
    Ident(InternedString),
    Lambda {
        params: Vec<SrcNode<InternedString>>,
        body: SrcNode<Self>,
    },
    Apply {
        fun: SrcNode<Self>,
        args: Vec<SrcNode<Self>>,
    },
    Let {
        name: SrcNode<InternedString>,
        expr: SrcNode<Self>,
        body: SrcNode<Self>,
    },
    Fn {
        name: SrcNode<InternedString>,
        params: Vec<SrcNode<InternedString>>,
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

#[derive(Debug, Clone, PartialEq)]
pub enum Lit {
    Num(Rational64),
    Bool(bool),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PrefixOp {
    Neg,
    Not,
}

impl From<Token> for PrefixOp {
    fn from(token: Token) -> Self {
        match token {
            Token::Minus => Self::Neg,
            Token::Bang => Self::Not,
            kind => panic!("Not an prefix operator: {:?}", kind),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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
}

impl From<Token> for InfixOp {
    fn from(token: Token) -> Self {
        match token {
            Token::Plus => Self::Add,
            Token::Minus => Self::Sub,
            Token::Star => Self::Mul,
            Token::Slash => Self::Div,
            Token::Percent => Self::Mod,
            // Token::Caret => Self::Pow,
            Token::Eq => Self::Eq,
            Token::Neq => Self::Neq,
            Token::Lt => Self::Lt,
            Token::Gt => Self::Gt,
            Token::Leq => Self::Leq,
            Token::Geq => Self::Geq,
            // Token::And => Self::And,
            // Token::Or => Self::Or,
            kind => panic!("Not an infix operator: {:?}", kind),
        }
    }
}
