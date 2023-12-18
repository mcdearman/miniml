use crate::{node::Node, num::Num};

#[derive(Debug, Clone, PartialEq)]
pub enum Sexpr {
    Atom(Node<Atom>),
    Pair { head: Node<Self>, tail: Node<Self> },
    // List(Vec<Node<Self>>),
    Vector(Vec<Self>),
    ByteVector(Vec<u8>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Atom {
    Symbol(String),
    String(String),
    Number(Node<Num>),
    Boolean(bool),
    Character(char),
}
