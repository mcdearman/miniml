#[repr(u8)]
#[derive(Debug, Clone, PartialEq)]
pub enum OpCode {
    Pop,
    DefineGlobal,
    GetGlobal,
    Const,
    Add,
    Sub,
    Mul,
    Div,
    Neg,
    Jif,
    Call,
    Return,
}

impl From<u8> for OpCode {
    fn from(byte: u8) -> Self {
        match byte {
            0 => OpCode::Pop,
            1 => OpCode::DefineGlobal,
            2 => OpCode::GetGlobal,
            3 => OpCode::Const,
            4 => OpCode::Add,
            5 => OpCode::Sub,
            6 => OpCode::Mul,
            7 => OpCode::Div,
            8 => OpCode::Neg,
            9 => OpCode::Jif,
            10 => OpCode::Call,
            11 => OpCode::Return,
            _ => panic!("invalid opcode"),
        }
    }
}
