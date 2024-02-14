#[derive(Debug, Copy, Clone, PartialEq)]
pub struct TypeError {
    pub msg: InternedString,
}

impl TypeError {
    pub fn new(msg: &str) -> Self {
        Self {
            msg: InternedString::from(msg),
        }
    }
}

impl From<String> for TypeError {
    fn from(msg: String) -> Self {
        Self::new(&*msg)
    }
}

pub type InferResult<T> = Result<T, TypeError>;
