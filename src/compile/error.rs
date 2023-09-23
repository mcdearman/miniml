use crate::util::intern::InternedString;

#[derive(Debug, Clone, PartialEq)]
pub struct CompilerError(pub InternedString);

impl CompilerError {
    pub fn new(msg: &str) -> Self {
        Self(InternedString::from(msg))
    }
}

pub type CompileResult<T> = std::result::Result<T, CompilerError>;
