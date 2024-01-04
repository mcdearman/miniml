use crate::{object::Object, opcode::OpCode};
use std::fmt::Display;

#[derive(Debug, Clone, PartialEq, Default)]
pub struct Chunk {
    data: Vec<Object>,
    code: Vec<u8>,
}

impl Chunk {
    pub fn write(&mut self, byte: u8) {
        log::trace!("write: {}", byte);
        self.code.push(byte);
    }

    pub fn write_data(&mut self, data: Object) -> usize {
        self.data.push(data);
        self.data.len() - 1
    }

    pub fn read(&self, ip: usize) -> u8 {
        log::trace!("code: {:?}", self.code);
        log::trace!("read: {}", ip);
        self.code[ip]
    }

    pub fn read_data(&self, ip: usize) -> Object {
        self.data[ip].clone()
    }

    pub fn in_data(&self, data: &Object) -> Option<usize> {
        self.data.iter().position(|d| d == data)
    }

    pub fn len(&self) -> usize {
        self.code.len()
    }

    pub fn data_len(&self) -> usize {
        self.data.len()
    }

    fn disassemble_instr(
        &self,
        offset: usize,
        f: &mut std::fmt::Formatter<'_>,
    ) -> Result<usize, std::fmt::Error> {
        write!(f, "{:04} ", offset)?;
        let instr = self.read(offset);
        match OpCode::from(instr) {
            OpCode::Halt => {
                writeln!(f, "HALT")?;
                Ok(offset + 1)
            }
            OpCode::Const => {
                let idx = self.read(offset + 1);
                let constant = self.read_data(idx as usize);
                writeln!(f, "CONST @{} {}", idx, constant)?;
                Ok(offset + 2)
            }
            OpCode::LoadGlobal => {
                let idx = self.read(offset + 1);
                writeln!(f, "LOAD_GLOBAL @{}", idx)?;
                Ok(offset + 2)
            }
            OpCode::StoreGlobal => {
                let idx = self.read(offset + 1);
                writeln!(f, "STORE_GLOBAL @{}", idx)?;
                Ok(offset + 2)
            }
            OpCode::LoadLocal => {
                let idx = self.read(offset + 1);
                writeln!(f, "LOAD_LOCAL @{}", idx)?;
                Ok(offset + 2)
            }
            OpCode::StoreLocal => {
                let idx = self.read(offset + 1);
                writeln!(f, "STORE_LOCAL @{}", idx)?;
                Ok(offset + 2)
            }
            OpCode::Add => {
                writeln!(f, "ADD")?;
                Ok(offset + 1)
            }
            OpCode::Sub => {
                writeln!(f, "SUB")?;
                Ok(offset + 1)
            }
            OpCode::Mul => {
                writeln!(f, "MUL")?;
                Ok(offset + 1)
            }
            OpCode::Div => {
                writeln!(f, "DIV")?;
                Ok(offset + 1)
            }
            OpCode::Rem => {
                writeln!(f, "REM")?;
                Ok(offset + 1)
            }
            OpCode::Pow => {
                writeln!(f, "POW")?;
                Ok(offset + 1)
            }
            OpCode::Not => {
                writeln!(f, "NOT")?;
                Ok(offset + 1)
            }
            OpCode::Eq => {
                writeln!(f, "EQ")?;
                Ok(offset + 1)
            }
            OpCode::Neq => {
                writeln!(f, "NEQ")?;
                Ok(offset + 1)
            }
            OpCode::Lt => {
                writeln!(f, "LT")?;
                Ok(offset + 1)
            }
            OpCode::Gt => {
                writeln!(f, "GT")?;
                Ok(offset + 1)
            }
            OpCode::Leq => {
                writeln!(f, "LEQ")?;
                Ok(offset + 1)
            }
            OpCode::Geq => {
                writeln!(f, "GEQ")?;
                Ok(offset + 1)
            }
            OpCode::And => {
                writeln!(f, "AND")?;
                Ok(offset + 1)
            }
            OpCode::Or => {
                writeln!(f, "OR")?;
                Ok(offset + 1)
            }
            OpCode::Neg => {
                writeln!(f, "NEG")?;
                Ok(offset + 1)
            }
            OpCode::Return => {
                writeln!(f, "RETURN")?;
                Ok(offset + 1)
            }
            OpCode::Push => {
                writeln!(f, "PUSH")?;
                Ok(offset + 1)
            }
            OpCode::Pop => {
                writeln!(f, "POP")?;
                Ok(offset + 1)
            }
            _ => {
                writeln!(f, "UNKNOWN")?;
                Ok(offset + 1)
            }
        }
    }
}

impl Display for Chunk {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut offset = 0;
        while offset < self.len() {
            offset = self.disassemble_instr(offset, f)?;
        }
        Ok(())
    }
}

mod tests {
    use crate::opcode::OpCode;

    #[test]
    fn disassemble_const() {
        let mut chunk = super::Chunk::default();
        let constant = crate::object::Object::Num(num_rational::Rational64::new(1, 2));
        let idx = chunk.write_data(constant);
        chunk.write(OpCode::Const as u8);
        chunk.write(idx as u8);
        insta::assert_display_snapshot!(chunk);
    }

    #[test]
    fn disassemble_add() {
        let mut chunk = super::Chunk::default();
        let c1 = crate::object::Object::Num(num_rational::Rational64::new(1, 2));
        let id1 = chunk.write_data(c1);
        chunk.write(OpCode::Const as u8);
        chunk.write(id1 as u8);
        let c2 = crate::object::Object::Num(num_rational::Rational64::new(2, 3));
        let id2 = chunk.write_data(c2);
        chunk.write(OpCode::Const as u8);
        chunk.write(id2 as u8);
        chunk.write(OpCode::Add as u8);
        insta::assert_display_snapshot!(chunk);
    }
}
