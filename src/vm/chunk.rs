use std::fmt::Display;

use super::{instr::Instr, value::Value};

#[derive(Debug, Clone, PartialEq)]
pub struct Chunk {
    pub code: Vec<Instr>,
    pub constants: Vec<Value>,
}

impl Chunk {
    pub fn new() -> Chunk {
        Chunk {
            code: vec![],
            constants: vec![],
        }
    }

    pub fn write(&mut self, instr: Instr) {
        self.code.push(instr);
    }

    pub fn add_constant(&mut self, constant: Value) -> usize {
        self.constants.push(constant);
        self.constants.len() - 1
    }

    pub fn read_constant(&self, idx: usize) -> &Value {
        &self.constants[idx]
    }

    pub fn emit(&mut self, instr: Instr) {
        self.write(instr)
    }
}

impl Display for Chunk {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}
