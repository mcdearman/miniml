use std::fmt::Display;

#[derive(Debug, Clone, PartialEq)]
pub struct RuntimeError(pub String);

impl RuntimeError {
    pub fn new(msg: &str) -> Self {
        Self(msg.to_string())
    }
}

impl Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}

pub type Result<T> = std::result::Result<T, RuntimeError>;
