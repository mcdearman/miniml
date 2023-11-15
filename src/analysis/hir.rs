use super::infer::Type;
use crate::util::{node::SrcNode, unique_id::UniqueId};
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
    pub items: Vec<SrcNode<Item>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Item {
    Expr(Expr),
    Fn {
        name: SrcNode<UniqueId>,
        params: Vec<SrcNode<UniqueId>>,
        env: Box<Env>,
        body: SrcNode<Expr>,
    },
    Def {
        name: SrcNode<UniqueId>,
        body: SrcNode<Expr>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Lit {
        lit: Lit,
        ty: Type,
    },
    Ident {
        name: SrcNode<UniqueId>,
        ty: Type,
    },
    Fn {
        name: SrcNode<UniqueId>,
        params: Vec<SrcNode<UniqueId>>,
        env: Box<Env>,
        body: SrcNode<Expr>,
        ty: Type,
    },
    // Closure {
    //     fun: Box<SrcNode<Expr>>,
    //     captured: HashMap<SrcNode<UniqueId>, SrcNode<Expr>>,
    //     ty: Type,
    // },
    Apply {
        fun: SrcNode<Expr>,
        args: Vec<SrcNode<Expr>>,
        ty: Type,
    },
    Unit,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Lit {
    Num(Rational64),
    Bool(bool),
}
