use super::{ast, node::Node};
use std::fmt::Debug;

pub trait Parser: Debug + Clone {
    fn parse(&self) -> Node<ast::Root>;
}
