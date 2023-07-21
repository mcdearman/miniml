#[derive(Debug, Clone, PartialEq)]
pub struct CompilerError(pub String);

impl CompilerError {
    pub fn new(msg: &str) -> Self {
        Self(msg.to_string())
    }
}

pub type CompileResult<T> = std::result::Result<T, CompilerError>;
