use num_rational::Rational64;
use std::{
    collections::HashMap,
    fmt::{Debug, Display},
};

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum RuntimeError {
    StackOverflow,
    StackUnderflow,
    DivideByZero,
    UnboundVariable,
    TypeMismatch,
    OpNotImplemented,
}

pub type RuntimeResult<T> = Result<T, RuntimeError>;

#[repr(u8)]
#[derive(Debug, Clone, PartialEq)]
pub enum OpCode {
    Halt,
    Const,
    LoadGlobal,
    StoreGlobal,
    LoadLocal,
    StoreLocal,
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Pow,
    Neg,
    Not,
    Eq,
    Neq,
    Lt,
    Gt,
    Leq,
    Geq,
    And,
    Or,
    Return,
    Push,
    Pop,
    Jump,
    JumpIfFalse,
    Call,
}

impl From<u8> for OpCode {
    fn from(byte: u8) -> Self {
        match byte {
            0 => OpCode::Halt,
            1 => OpCode::Const,
            2 => OpCode::LoadGlobal,
            3 => OpCode::StoreGlobal,
            4 => OpCode::StoreLocal,
            5 => OpCode::LoadLocal,
            6 => OpCode::Add,
            7 => OpCode::Sub,
            8 => OpCode::Mul,
            9 => OpCode::Div,
            10 => OpCode::Rem,
            11 => OpCode::Pow,
            12 => OpCode::Neg,
            13 => OpCode::Not,
            14 => OpCode::Eq,
            15 => OpCode::Neq,
            16 => OpCode::Lt,
            17 => OpCode::Gt,
            18 => OpCode::Leq,
            19 => OpCode::Geq,
            20 => OpCode::And,
            21 => OpCode::Or,
            22 => OpCode::Return,
            23 => OpCode::Push,
            24 => OpCode::Pop,
            25 => OpCode::Jump,
            26 => OpCode::JumpIfFalse,
            27 => OpCode::Call,
            _ => panic!("invalid opcode"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Default)]
pub struct Chunk {
    data: Vec<Object>,
    code: Vec<u8>,
}

impl Chunk {
    pub fn write(&mut self, byte: u8) {
        // log::trace!("write: {}", byte);
        self.code.push(byte);
    }

    pub fn write_data(&mut self, data: Object) -> usize {
        self.data.push(data);
        self.data.len() - 1
    }

    pub fn read(&self, ip: usize) -> u8 {
        // log::trace!("code: {:?}", self.code);
        // log::trace!("read: {}", ip);
        self.code[ip]
    }

    pub fn read_data(&self, ip: usize) -> Object {
        self.data[ip].clone()
    }

    pub fn clear_data(&mut self) {
        self.data.clear();
    }

    pub fn clear_code(&mut self) {
        self.code.clear();
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

#[derive(Debug, Clone, PartialEq)]
pub enum Object {
    Bool(bool),
    Num(Rational64),
    String(String),
    Function(Function),
    Closure(Closure),
    NativeFn(NativeFn),
    Nil,
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::Bool(b) => write!(f, "{}", b),
            Object::Num(n) => write!(f, "{}", n),
            Object::String(s) => write!(f, "{}", s),
            Object::Function(fun) => write!(f, "{}", fun),
            Object::Closure(fun) => write!(f, "{}", fun),
            Object::NativeFn(fun) => write!(f, "{:?}", fun),
            Object::Nil => write!(f, "nil"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    name: String,
    arity: u8,
    chunk: Chunk,
}

impl Function {
    pub fn new(name: String, arity: u8, chunk: Chunk) -> Self {
        Self { name, arity, chunk }
    }
}

impl Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<Fn({})>", self.name)
    }
}

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

#[derive(Clone, PartialEq)]
pub struct NativeFn {
    name: String,
    arity: u8,
    callable: fn(&mut Interpreter, &[Object]) -> RuntimeResult<Object>,
}

impl NativeFn {
    pub fn new(
        name: String,
        arity: u8,
        callable: fn(&mut Interpreter, &[Object]) -> RuntimeResult<Object>,
    ) -> Self {
        Self {
            name,
            arity,
            callable,
        }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn arity(&self) -> u8 {
        self.arity
    }

    pub fn call(&self, interp: &mut Interpreter, args: &[Object]) -> RuntimeResult<Object> {
        (self.callable)(interp, args)
    }
}

impl Debug for NativeFn {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<NativeFn({})>", self.name)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct CallFrame {
    pub ip: usize,
    pub code: Vec<u8>,
}

#[derive(Debug, Clone, PartialEq, Default)]
pub struct Interpreter {
    globals: HashMap<String, Object>,
    chunk: Chunk,
    ip: usize,
    stack: Vec<Object>,
}

impl Interpreter {
    const STACK_MAX: usize = 512;

    pub fn exec(&mut self, chunk: Chunk) -> RuntimeResult<Object> {
        self.chunk = chunk;
        loop {
            let instr = self.read_instr();
            log::trace!("Instr: {:?}", instr);
            log::trace!("Stack: {:?}", self.stack);
            match instr {
                OpCode::Halt => return Ok(Object::Nil),
                OpCode::Const => {
                    let constant = self.read_constant();
                    self.push(constant);
                }
                OpCode::LoadGlobal => {
                    let index = self.read_8_bits();
                    if let Some(value) = self.globals.get(&index.to_string()) {
                        self.push(value.clone());
                    } else {
                        return Err(RuntimeError::UnboundVariable);
                    }
                }
                OpCode::StoreGlobal => {
                    let index = self.read_8_bits();
                    let value = self.pop();
                    self.globals.insert(index.to_string(), value);
                }
                OpCode::LoadLocal => {
                    let index = self.read_8_bits();
                    let value = self.stack[index as usize].clone();
                    self.push(value);
                }
                OpCode::StoreLocal => {
                    let index = self.read_8_bits();
                    let value = self.peek();
                    self.stack[index as usize] = value;
                }
                OpCode::Add => {
                    let b = self.pop();
                    let a = self.pop();
                    match (a, b) {
                        (Object::Num(a), Object::Num(b)) => self.push(Object::Num(a + b)),
                        _ => return Err(RuntimeError::TypeMismatch),
                    }
                }
                OpCode::Sub => {
                    let b = self.pop();
                    let a = self.pop();
                    match (a, b) {
                        (Object::Num(a), Object::Num(b)) => self.push(Object::Num(a - b)),
                        _ => return Err(RuntimeError::TypeMismatch),
                    }
                }
                OpCode::Mul => {
                    let b = self.pop();
                    let a = self.pop();
                    match (a, b) {
                        (Object::Num(a), Object::Num(b)) => self.push(Object::Num(a * b)),
                        _ => return Err(RuntimeError::TypeMismatch),
                    }
                }
                OpCode::Div => {
                    let b = self.pop();
                    let a = self.pop();
                    match (a, b) {
                        (Object::Num(a), Object::Num(b)) => self.push(Object::Num(a / b)),
                        _ => return Err(RuntimeError::TypeMismatch),
                    }
                }
                OpCode::Rem => {
                    let b = self.pop();
                    let a = self.pop();
                    match (a, b) {
                        (Object::Num(a), Object::Num(b)) => self.push(Object::Num(a % b)),
                        _ => return Err(RuntimeError::TypeMismatch),
                    }
                }
                OpCode::Pow => {
                    let b = self.pop();
                    let a = self.pop();
                    match (a, b) {
                        (Object::Num(a), Object::Num(b)) => {
                            if b.is_integer() {
                                self.push(Object::Num(a.pow(b.to_integer() as i32)))
                            } else {
                                return Err(RuntimeError::TypeMismatch);
                            }
                        }

                        _ => return Err(RuntimeError::TypeMismatch),
                    }
                }
                OpCode::Neg => {
                    let value = self.pop();
                    match value {
                        Object::Num(n) => self.push(Object::Num(-n)),
                        _ => return Err(RuntimeError::TypeMismatch),
                    }
                }
                OpCode::Not => {
                    let value = self.pop();
                    match value {
                        Object::Bool(b) => self.push(Object::Bool(!b)),
                        _ => return Err(RuntimeError::TypeMismatch),
                    }
                }
                OpCode::Eq => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push(Object::Bool(a == b));
                }
                OpCode::Neq => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push(Object::Bool(a != b));
                }
                OpCode::Lt => {
                    let b = self.pop();
                    let a = self.pop();
                    match (a, b) {
                        (Object::Num(a), Object::Num(b)) => self.push(Object::Bool(a < b)),
                        _ => return Err(RuntimeError::TypeMismatch),
                    }
                }
                OpCode::Gt => {
                    let b = self.pop();
                    let a = self.pop();
                    match (a, b) {
                        (Object::Num(a), Object::Num(b)) => self.push(Object::Bool(a > b)),
                        _ => return Err(RuntimeError::TypeMismatch),
                    }
                }
                OpCode::Leq => {
                    let b = self.pop();
                    let a = self.pop();
                    match (a, b) {
                        (Object::Num(a), Object::Num(b)) => self.push(Object::Bool(a <= b)),
                        _ => return Err(RuntimeError::TypeMismatch),
                    }
                }
                OpCode::Geq => {
                    let b = self.pop();
                    let a = self.pop();
                    match (a, b) {
                        (Object::Num(a), Object::Num(b)) => self.push(Object::Bool(a >= b)),
                        _ => return Err(RuntimeError::TypeMismatch),
                    }
                }
                OpCode::And => {
                    let b = self.pop();
                    let a = self.pop();
                    match (a, b) {
                        (Object::Bool(a), Object::Bool(b)) => self.push(Object::Bool(a && b)),
                        _ => return Err(RuntimeError::TypeMismatch),
                    }
                }
                OpCode::Or => {
                    let b = self.pop();
                    let a = self.pop();
                    match (a, b) {
                        (Object::Bool(a), Object::Bool(b)) => self.push(Object::Bool(a || b)),
                        _ => return Err(RuntimeError::TypeMismatch),
                    }
                }
                OpCode::Return => {
                    return Ok(self.pop());
                }
                OpCode::Push => {
                    let constant = self.read_constant();
                    self.push(constant);
                }
                OpCode::Pop => {
                    self.pop();
                }
                OpCode::Jump => {
                    todo!()
                }
                OpCode::JumpIfFalse => {
                    todo!()
                }
                _ => return Err(RuntimeError::OpNotImplemented),
            }
            log::trace!("Stack: {:?}", self.stack);
        }
    }

    fn read_instr(&mut self) -> OpCode {
        let instr = self.chunk.read(self.ip);
        self.ip += 1;
        OpCode::from(instr)
    }

    fn read_8_bits(&mut self) -> u8 {
        let byte = self.chunk.read(self.ip);
        self.ip += 1;
        byte
    }

    fn read_16_bits(&mut self) -> u16 {
        let byte1 = self.read_8_bits();
        let byte2 = self.read_8_bits();
        u16::from_be_bytes([byte1, byte2])
    }

    fn read_24_bits(&mut self) -> u32 {
        let byte1 = self.read_8_bits();
        let byte2 = self.read_8_bits();
        let byte3 = self.read_8_bits();
        u32::from_be_bytes([byte1, byte2, byte3, 0])
    }

    fn read_constant(&mut self) -> Object {
        let idx = self.read_8_bits();
        log::trace!("const op: {:?}", idx);
        self.chunk.read_data(idx as usize).clone()
    }

    fn push(&mut self, value: Object) {
        if self.stack.len() >= Self::STACK_MAX {
            panic!("Stack overflow");
        }
        self.stack.push(value);
    }

    fn pop(&mut self) -> Object {
        self.stack.pop().expect("Stack underflow")
    }

    fn peek(&self) -> Object {
        self.stack[self.stack.len() - 1].clone()
    }
}

mod tests {
    use crate::vm::{Object, OpCode};

    #[test]
    fn disassemble_const() {
        let mut chunk = super::Chunk::default();
        let constant = Object::Num(num_rational::Rational64::new(1, 2));
        let idx = chunk.write_data(constant);
        chunk.write(OpCode::Const as u8);
        chunk.write(idx as u8);
        insta::assert_display_snapshot!(chunk);
    }

    #[test]
    fn disassemble_add() {
        let mut chunk = super::Chunk::default();
        let c1 = Object::Num(num_rational::Rational64::new(1, 2));
        let id1 = chunk.write_data(c1);
        chunk.write(OpCode::Const as u8);
        chunk.write(id1 as u8);
        let c2 = Object::Num(num_rational::Rational64::new(2, 3));
        let id2 = chunk.write_data(c2);
        chunk.write(OpCode::Const as u8);
        chunk.write(id2 as u8);
        chunk.write(OpCode::Add as u8);
        insta::assert_display_snapshot!(chunk);
    }
}
