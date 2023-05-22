use super::{chunk::Chunk, value::Value};
use std::{fmt::Display, io::Write};

#[derive(Debug, Clone, PartialEq)]
pub enum Instr {
    Load(u16),
    Store(Value),
    LoadConst(u16),
    StoreConst(Value),
    Push(Value),
    Pop,
    Dup,
    Swap,
    Drop,
    Neg,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    And,
    Or,
    Xor,
    Not,
    Eq,
    Neq,
    Lt,
    Gt,
    Jump(u16),
    Call(u16),
    Return,
    Jeq(u16),
    Halt,
}

impl Display for Instr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Instr::Load(addr) => write!(f, "LOAD {}", addr),
            Instr::Store(value) => write!(f, "STORE, {}", value),
            Instr::LoadConst(addr) => write!(f, "LOADC {}", addr),
            Instr::StoreConst(value) => write!(f, "STOREC {}", value),
            Instr::Push(value) => write!(f, "PUSH {}", value),
            Instr::Pop => write!(f, "POP"),
            Instr::Dup => write!(f, "DUP"),
            Instr::Swap => write!(f, "SWAP"),
            Instr::Drop => write!(f, "DROP"),
            Instr::Neg => write!(f, "NEG"),
            Instr::Add => write!(f, "ADD"),
            Instr::Sub => write!(f, "SUB"),
            Instr::Mul => write!(f, "MUL"),
            Instr::Div => write!(f, "DIV"),
            Instr::Mod => write!(f, "MOD"),
            Instr::And => write!(f, "AND"),
            Instr::Or => write!(f, "OR"),
            Instr::Xor => write!(f, "XOR"),
            Instr::Not => write!(f, "NOT"),
            Instr::Eq => write!(f, "EQ"),
            Instr::Neq => write!(f, "NEQ"),
            Instr::Lt => write!(f, "LT"),
            Instr::Gt => write!(f, "GT"),
            Instr::Jump(addr) => write!(f, "JMP {}", addr),
            Instr::Call(addr) => write!(f, "CALL {}", addr),
            Instr::Return => write!(f, "RET"),
            Instr::Jeq(addr) => write!(f, "JEQ {}", addr),
            Instr::Halt => write!(f, "HALT"),
        }
    }
}

impl Instr {
    pub fn dissassemble<W: Write>(
        &self,
        chunk: &Chunk,
        offset: usize,
        output: &mut W,
    ) -> core::result::Result<(), std::io::Error> {
        write!(output, "{:04} ", offset)?;
        todo!()
    }
}
