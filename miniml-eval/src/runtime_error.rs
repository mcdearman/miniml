use miniml_util::intern::InternedString;

#[derive(Debug, Clone, PartialEq)]
pub struct RuntimeError(InternedString);

impl RuntimeError {
    pub fn new(msg: String) -> Self {
        Self(msg.into())
    }
}

impl From<&str> for RuntimeError {
    fn from(s: &str) -> Self {
        Self::new(s.to_string())
    }
}

pub type EvalResult<T> = std::result::Result<T, RuntimeError>;
