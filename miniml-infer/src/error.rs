use miniml_utils::intern::InternedString;
use std::fmt::Display;

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct TypeError {
    msg: InternedString,
}

impl TypeError {
    pub fn new(msg: &str) -> Self {
        Self {
            msg: InternedString::from(msg),
        }
    }
}

impl Display for TypeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.msg)
    }
}

impl From<String> for TypeError {
    fn from(msg: String) -> Self {
        Self::new(&*msg)
    }
}

pub type InferResult<T> = Result<T, TypeError>;
