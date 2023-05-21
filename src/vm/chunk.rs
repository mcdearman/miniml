use super::value::Value;

#[derive(Debug, Clone, PartialEq)]
pub struct Chunk {
    pub code: Vec<u8>,
    pub constants: Vec<Value>,
}
