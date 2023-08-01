use crate::object::Function;

#[derive(Debug, Clone, PartialEq)]
pub struct CallFrame {
    pub fun: Box<Function>,
    pub ip: usize,
    pub base: usize,
}
