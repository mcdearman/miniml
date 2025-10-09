use crate::vm::opcode::Opcode;

pub mod info_table;
pub mod opcode;

#[derive(Debug)]
pub struct StgM {
    code: Vec<Opcode>,
    stack: Vec<usize>,
    heap: Vec<usize>,
    globals: Vec<usize>,
}

pub enum Node {
    Int(i64),
    Ap(usize, usize),
    Global(String, Vec<Opcode>),
}