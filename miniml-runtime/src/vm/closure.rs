#[derive(Debug, Clone, PartialEq)]
pub struct Closure {
    name: String,
    arity: usize,
    chunk: Chunk,
    upvalues: Vec<usize>,
}

impl Display for Closure {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<Fn({})>", self.name)
    }
}
