#[repr(u8)]
#[derive(Debug, Clone, PartialEq)]
pub enum OpCode {
    Pop,
    DefineGlobal,
    Const,
    Add,
    Sub,
    Mul,
    Div,
    Neg,
    Return,
}

impl From<u8> for OpCode {
    fn from(byte: u8) -> Self {
        match byte {
            0 => OpCode::Pop,
            1 => OpCode::DefineGlobal,
            2 => OpCode::Const,
            3 => OpCode::Add,
            4 => OpCode::Sub,
            5 => OpCode::Mul,
            6 => OpCode::Div,
            7 => OpCode::Neg,
            8 => OpCode::Return,
            _ => panic!("invalid opcode"),
        }
    }
}
