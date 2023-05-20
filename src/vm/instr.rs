use super::value::Value;
use std::fmt::Display;

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
    Jump,
    Call,
    Return,
    Jeq,
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
            Instr::Jump => write!(f, "JMP"),
            Instr::Call => write!(f, "CALL"),
            Instr::Return => write!(f, "RET"),
            Instr::Jeq => write!(f, "JEQ"),
            Instr::Halt => write!(f, "HALT"),
        }
    }
}
