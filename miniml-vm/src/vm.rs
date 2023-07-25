use crate::{
    chunk::Chunk,
    error::{Result, RuntimeError},
    opcode::OpCode,
    value::Value,
};
use miniml_util::intern::InternedString;
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub struct VM {
    chunk: Chunk,
    ip: usize,
    stack: Vec<Value>,
    globals: HashMap<InternedString, Value>,
}

impl VM {
    const STACK_MAX: usize = 256;

    pub fn new(chunk: Chunk) -> Self {
        Self {
            chunk,
            ip: 0,
            stack: vec![],
            globals: HashMap::new(),
        }
    }

    pub fn run(&mut self) -> Result<Value> {
        loop {
            let instr = self.read_instr();
            log::trace!("Instr: {:?}", instr);
            log::trace!("Stack: {:?}", self.stack);
            log::trace!("IP: {}", self.ip);
            // log::trace!("Chunk: {:?}", self.chunk);
            match instr {
                OpCode::Pop => {
                    self.pop();
                }
                OpCode::DefineGlobal => {
                    let name = self.read_string();
                    let value = self.pop();
                    log::trace!("defining global: {} = {}", name, value);
                    self.globals.insert(name, value);
                    log::trace!("Globals: {:?}", self.globals);
                    break;
                }
                OpCode::GetGlobal => {
                    let name = self.read_string();
                    log::trace!("getting global: {}", name);
                    let value = self.globals.get(&name).unwrap().clone();
                    self.push(value);
                }
                OpCode::Const => {
                    let constant = self.read_constant();
                    self.push(constant);
                }
                OpCode::Add => {
                    let b = self.pop();
                    let a = self.pop();
                    match (a.clone(), b.clone()) {
                        (Value::Int(a), Value::Int(b)) => self.push(Value::Int(a + b)),
                        (Value::Rational(a), Value::Rational(b)) => {
                            self.push(Value::Rational(a + b))
                        }
                        (Value::Real(a), Value::Real(b)) => self.push(Value::Real(a + b)),
                        (Value::Complex(a), Value::Complex(b)) => self.push(Value::Complex(a + b)),
                        _ => return Err(RuntimeError(format!("Cannot add {:?} and {:?}", a, b))),
                    }
                }
                OpCode::Sub => {
                    let b = self.pop();
                    let a = self.pop();
                    match (a.clone(), b.clone()) {
                        (Value::Int(a), Value::Int(b)) => self.push(Value::Int(a - b)),
                        (Value::Rational(a), Value::Rational(b)) => {
                            self.push(Value::Rational(a - b))
                        }
                        (Value::Real(a), Value::Real(b)) => self.push(Value::Real(a - b)),
                        (Value::Complex(a), Value::Complex(b)) => self.push(Value::Complex(a - b)),
                        _ => {
                            return Err(RuntimeError(format!(
                                "Cannot subtract {:?} and {:?}",
                                a, b
                            )))
                        }
                    }
                }
                OpCode::Mul => {
                    let b = self.pop();
                    let a = self.pop();
                    match (a.clone(), b.clone()) {
                        (Value::Int(a), Value::Int(b)) => self.push(Value::Int(a * b)),
                        (Value::Rational(a), Value::Rational(b)) => {
                            self.push(Value::Rational(a * b))
                        }
                        (Value::Real(a), Value::Real(b)) => self.push(Value::Real(a * b)),
                        (Value::Complex(a), Value::Complex(b)) => self.push(Value::Complex(a * b)),
                        _ => {
                            return Err(RuntimeError(format!(
                                "Cannot multiply {:?} and {:?}",
                                a, b
                            )))
                        }
                    }
                }
                OpCode::Div => {
                    let b = self.pop();
                    let a = self.pop();
                    match (a.clone(), b.clone()) {
                        (Value::Int(a), Value::Int(b)) => self.push(Value::Int(a / b)),
                        (Value::Rational(a), Value::Rational(b)) => {
                            self.push(Value::Rational(a / b))
                        }
                        (Value::Real(a), Value::Real(b)) => self.push(Value::Real(a / b)),
                        (Value::Complex(a), Value::Complex(b)) => self.push(Value::Complex(a / b)),
                        _ => {
                            return Err(RuntimeError(format!("Cannot divide {:?} and {:?}", a, b)))
                        }
                    }
                }
                OpCode::Neg => {
                    let value = self.pop();
                    match value.clone() {
                        Value::Int(n) => self.push(Value::Int(-n)),
                        Value::Rational(n) => self.push(Value::Rational(-n)),
                        Value::Real(n) => self.push(Value::Real(-n)),
                        Value::Complex(n) => self.push(Value::Complex(-n)),
                        _ => {
                            return Err(RuntimeError(format!(
                                "Cannot negate non number `{:?}`",
                                value
                            )))
                        }
                    }
                }
                OpCode::Return => {
                    let val = self.pop();
                    // println!("{}", val);
                    return Ok(val);
                }
                _ => return Err(RuntimeError::new("Unknown opcode")),
            }
        }
        Ok(Value::Unit)
    }

    fn read_instr(&mut self) -> OpCode {
        log::trace!("ip instr: {}", self.ip);
        let instr = self.chunk.code[self.ip];
        self.ip += 1;
        OpCode::from(instr)
    }

    fn read_constant(&mut self) -> Value {
        log::trace!("ip const: {}", self.ip);
        let op = self.read_instr();
        self.chunk.constants[op as usize].clone()
    }

    fn read_string(&mut self) -> InternedString {
        log::trace!("ip string: {}", self.ip);
        match self.read_constant() {
            Value::String(s) => s,
            _ => panic!("Expected string"),
        }
    }

    fn push(&mut self, value: Value) {
        if self.stack.len() >= Self::STACK_MAX {
            panic!("Stack overflow");
        }
        self.stack.push(value);
    }

    fn pop(&mut self) -> Value {
        self.stack.pop().expect("Stack underflow")
    }
}
