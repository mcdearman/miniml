use super::token::Token;
use crate::util::{intern::InternedString, node::Node};
use num_rational::Rational64;

#[derive(Debug, Clone, PartialEq)]
pub struct Root {
    pub items: Vec<Node<Item>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Item {
    Def {
        name: Node<InternedString>,
        expr: Node<Expr>,
    },
    Fn {
        name: Node<InternedString>,
        params: Vec<Node<InternedString>>,
        body: Node<Expr>,
    },
    Expr(Expr),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Lit(Lit),
    Ident(InternedString),
    Lambda {
        params: Vec<Node<InternedString>>,
        body: Node<Self>,
    },
    Apply {
        fun: Node<Self>,
        args: Vec<Node<Self>>,
    },
    Let {
        name: Node<InternedString>,
        expr: Node<Self>,
        body: Node<Self>,
    },
    Fn {
        name: Node<InternedString>,
        params: Vec<Node<InternedString>>,
        expr: Node<Self>,
        body: Node<Self>,
    },
    If {
        cond: Node<Self>,
        then: Node<Self>,
        else_: Node<Self>,
    },
    Prefix {
        op: Node<PrefixOp>,
        expr: Node<Self>,
    },
    Infix {
        op: Node<InfixOp>,
        lhs: Node<Self>,
        rhs: Node<Self>,
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
