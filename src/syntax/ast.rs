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
        pat: Node<Pattern>,
        expr: Node<Self>,
        body: Node<Self>,
    },
    Fn {
        name: Node<InternedString>,
        params: Vec<Node<Pattern>>,
        expr: Node<Self>,
        body: Node<Self>,
    },
    Match {
        expr: Node<Self>,
        cases: Vec<Node<MatchCase>>,
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
pub struct MatchCase {
    pub pattern: Node<Pattern>,
    pub expr: Node<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Lit {
    Num(Rational64),
    Bool(bool),
    String(InternedString),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    Lit(Lit),
    Ident(InternedString),
    Wildcard,
    Unit,
    // Tuple(Vec<Node<Self>>),
    // Pair {
    //     head: Node<Self>,
    //     tail: Node<Self>,
    // },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PrefixOp {
    Neg,
    Not,
}

impl ToString for PrefixOp {
    fn to_string(&self) -> String {
        match self {
            Self::Neg => "-",
            Self::Not => "!",
        }
        .to_string()
    }
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

impl ToString for InfixOp {
    fn to_string(&self) -> String {
        match self {
            Self::Add => "+",
            Self::Sub => "-",
            Self::Mul => "*",
            Self::Div => "/",
            Self::Mod => "%",
            // Self::Pow => "^",
            Self::Eq => "==",
            Self::Neq => "!=",
            Self::Lt => "<",
            Self::Gt => ">",
            Self::Leq => "<=",
            Self::Geq => ">=",
            // Self::And => "&&",
            // Self::Or => "||",
        }
        .to_string()
    }
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
