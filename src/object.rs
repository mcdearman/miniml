use crate::num::Num;

#[derive(Debug, Clone, PartialEq)]
pub enum Object {
    Num(Num),
    Bool(bool),
    String(String),
    Nil,
}
