use crate::{node::Node, num::Num};

#[derive(Debug, Clone)]
pub enum TokenTree {
    Group {
        delimiter: Node<Delimiter>,
        inner: Vec<Node<Self>>,
    },
    Ident(Node<String>),
    Punct(Node<Punct>),
    Literal(Node<Literal>),
}

#[derive(Debug, Clone)]
pub enum Delimiter {
    Paren,
    Bracket,
    Brace,
    End,
}

#[derive(Debug, Clone)]
pub enum Punct {
    Lambda,
    Arrow,
    Assign,
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Eq,
    Neq,
    Lt,
    Gt,
    Leq,
    Geq,
    Bang,
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBrack,
    RBrack,
}

#[derive(Debug, Clone)]
pub enum Literal {
    Num(Num),
    Bool(bool),
    String(String),
}
