use crate::{chunk::Chunk, object::Object, opcode::OpCode};
use std::collections::HashMap;

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
        let op = self.read_instr();
        self.chunk.read_data(op as usize).clone()
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
