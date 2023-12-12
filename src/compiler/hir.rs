use super::infer::Type;
use common::{node::Node, unique_id::UniqueId};
use num_rational::Rational64;
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub struct Env {
    parent: Option<Box<Env>>,
    bindings: HashMap<UniqueId, Expr>,
}

impl Env {
    pub fn new() -> Self {
        Self {
            parent: None,
            bindings: HashMap::new(),
        }
    }

    pub fn new_with_parent(parent: Env) -> Self {
        Self {
            parent: Some(Box::new(parent)),
            bindings: HashMap::new(),
        }
    }

    pub fn insert(&mut self, id: UniqueId, expr: Expr) {
        self.bindings.insert(id, expr);
    }

    pub fn get(&self, id: &UniqueId) -> Option<&Expr> {
        self.bindings
            .get(id)
            .or(self.parent.as_ref().and_then(|parent| parent.get(id)))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Root {
    pub items: Vec<Node<Item>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Item {
    Expr(Expr),
    Fn {
        name: Node<UniqueId>,
        params: Vec<Node<UniqueId>>,
        body: Node<Expr>,
    },
    Def {
        name: Node<UniqueId>,
        body: Node<Expr>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Lit {
        lit: Lit,
        ty: Type,
    },
    Ident {
        name: Node<UniqueId>,
        ty: Type,
    },
    Fn {
        name: Node<UniqueId>,
        params: Vec<Node<UniqueId>>,
        env: Box<Env>,
        body: Node<Expr>,
        ty: Type,
    },
    Let {
        name: Node<UniqueId>,
        value: Node<Expr>,
        body: Node<Expr>,
        ty: Type,
    },
    Closure {
        fun: Node<Expr>,
        args: Vec<Node<Expr>>,
        env: Box<Env>,
        ty: Type,
    },
    Apply {
        fun: Node<Expr>,
        args: Vec<Node<Expr>>,
        ty: Type,
    },
    Unit,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Lit {
    Num(Rational64),
    Bool(bool),
}
