use crate::util::intern::InternedString;
use std::fmt::Display;

#[derive(Debug, Clone, PartialEq)]
pub struct RuntimeError(InternedString);

impl RuntimeError {
    pub fn new(msg: String) -> Self {
        Self(msg.into())
    }
}

impl Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Runtime Error: {}", self.0)
    }
}

impl From<&str> for RuntimeError {
    fn from(s: &str) -> Self {
        Self::new(s.to_string())
    }
}

pub type EvalResult<T> = std::result::Result<T, RuntimeError>;
