use crate::chunk::Chunk;
use crate::num::Num;
use std::fmt::Display;

#[derive(Debug, Clone, PartialEq)]
pub enum Object {
    Num(Num),
    Bool(bool),
    String(String),
    Nil,
}