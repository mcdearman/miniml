use super::object::Function;
use std::fmt::Display;

#[derive(Debug, Clone, PartialEq)]
pub struct CallFrame {
    pub fun: Box<Function>,
    pub ip: usize,
    pub base: usize,
}

impl CallFrame {
    pub fn new(fun: Box<Function>) -> Self {
        Self {
            fun,
            ip: 0,
            base: 0,
        }
    }
}

impl Display for CallFrame {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.fun)
    }
}
