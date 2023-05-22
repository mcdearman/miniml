use std::{cell::RefCell, collections::HashMap, rc::Rc};

use self::{
    instr::Instr,
    value::{Record, Value},
};

pub mod chunk;
pub mod gc;
pub mod instr;
pub mod value;

pub struct VM {
    pc: usize,
    program: Vec<u8>,
    constants: Vec<Value>,
    stack: Vec<Value>,
    frames: Vec<CallFrame>,
    heap: Vec<Rc<Value>>,
}

pub struct CallFrame {
    pc: usize,
    stack_base: usize,
    stack_top: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub struct RuntimeError(pub String);

impl RuntimeError {
    pub fn new(msg: String) -> RuntimeError {
        RuntimeError(msg)
    }
}

impl From<&str> for RuntimeError {
    fn from(msg: &str) -> RuntimeError {
        RuntimeError::new(msg.to_string())
    }
}

pub type Result<T> = std::result::Result<T, RuntimeError>;

impl VM {
    pub fn new(program: Vec<u8>) -> VM {
        VM {
            pc: 0,
            program,
            constants: vec![],
            stack: vec![],
            frames: vec![],
            heap: vec![],
        }
    }

    pub fn run(&mut self) {
        while self.pc < self.program.len() {
            let instr = self.read_instr();
            match instr {
                Instr::Load(offset) => {
                    let value = self.stack[offset as usize].clone();
                    self.push(value);
                }
                Instr::Store(value) => {
                    self.stack.push(value);
                }
                Instr::LoadConst(offset) => {
                    let value = self.constants[offset as usize].clone();
                    self.push(value);
                }
                Instr::StoreConst(value) => {
                    self.constants.push(value);
                }
                Instr::Push(value) => {
                    self.stack.push(value.clone());
                }
                Instr::Pop => {
                    self.stack.pop();
                }
                Instr::Dup => {
                    let top = self.stack.len() - 1;
                    let value = self.stack[top].clone();
                    self.stack.push(value);
                }
                Instr::Swap => {
                    let top = self.stack.len() - 1;
                    self.stack.swap(top, top - 1);
                }
                Instr::Neg => {
                    let a = self.pop();
                    match a {
                        Value::Int(a) => self.push(Value::Int(-a)),
                        Value::Float(a) => self.push(Value::Float(-a)),
                        _ => panic!("Invalid operand type"),
                    }
                }
                Instr::Add => {
                    let a = self.pop();
                    let b = self.pop();
                    match (a, b) {
                        (Value::Int(a), Value::Int(b)) => self.push(Value::Int(a + b)),
                        (Value::Float(a), Value::Float(b)) => self.push(Value::Float(a + b)),
                        _ => panic!("Invalid operand types"),
                    }
                }
                Instr::Sub => {
                    let a = self.pop();
                    let b = self.pop();
                    match (a, b) {
                        (Value::Int(a), Value::Int(b)) => self.push(Value::Int(a - b)),
                        (Value::Float(a), Value::Float(b)) => self.push(Value::Float(a - b)),
                        _ => panic!("Invalid operand types"),
                    }
                }
                Instr::Mul => {
                    let a = self.pop();
                    let b = self.pop();
                    match (a, b) {
                        (Value::Int(a), Value::Int(b)) => self.push(Value::Int(a * b)),
                        (Value::Float(a), Value::Float(b)) => self.push(Value::Float(a * b)),
                        _ => panic!("Invalid operand types"),
                    }
                }
                Instr::Div => {
                    let a = self.pop();
                    let b = self.pop();
                    match (a, b) {
                        (Value::Int(a), Value::Int(b)) => self.push(Value::Int(a / b)),
                        (Value::Float(a), Value::Float(b)) => self.push(Value::Float(a / b)),
                        _ => panic!("Invalid operand types"),
                    }
                }
                Instr::And => {
                    let a = self.pop();
                    let b = self.pop();
                    match (a, b) {
                        (Value::Bool(a), Value::Bool(b)) => self.push(Value::Bool(a && b)),
                        _ => panic!("Invalid operand types"),
                    }
                }
                Instr::Or => {
                    let a = self.pop();
                    let b = self.pop();
                    match (a, b) {
                        (Value::Bool(a), Value::Bool(b)) => self.push(Value::Bool(a || b)),
                        _ => panic!("Invalid operand types"),
                    }
                }
                Instr::Xor => {
                    let a = self.pop();
                    let b = self.pop();
                    match (a, b) {
                        (Value::Bool(a), Value::Bool(b)) => self.push(Value::Bool(a ^ b)),
                        _ => panic!("Invalid operand types"),
                    }
                }
                Instr::Not => {
                    let a = self.pop();
                    match a {
                        Value::Bool(a) => self.push(Value::Bool(!a)),
                        _ => panic!("Invalid operand type"),
                    }
                }
                Instr::Eq => {
                    let a = self.pop();
                    let b = self.pop();
                    self.push(Value::Bool(a == b));
                }
                Instr::Neq => {
                    let a = self.pop();
                    let b = self.pop();
                    self.push(Value::Bool(a != b));
                }
                Instr::Lt => {
                    let a = self.pop();
                    let b = self.pop();
                    match (a, b) {
                        (Value::Int(a), Value::Int(b)) => self.push(Value::Bool(a < b)),
                        (Value::Float(a), Value::Float(b)) => self.push(Value::Bool(a < b)),
                        _ => panic!("Invalid operand types"),
                    }
                }
                Instr::Gt => {
                    let a = self.pop();
                    let b = self.pop();
                    match (a, b) {
                        (Value::Int(a), Value::Int(b)) => self.push(Value::Bool(a > b)),
                        (Value::Float(a), Value::Float(b)) => self.push(Value::Bool(a > b)),
                        _ => panic!("Invalid operand types"),
                    }
                }
                Instr::Jump(addr) => {
                    self.pc = addr as usize;
                }
                Instr::Call(addr) => {
                    let stack_base = self.stack.len();
                    let stack_top = stack_base + 1;
                    self.frames.push(CallFrame {
                        pc: self.pc,
                        stack_base,
                        stack_top,
                    });
                    self.pc = addr as usize;
                }
                Instr::Halt => break,
                _ => {
                    panic!("Unimplemented instr: {:?}", instr);
                }
            }
        }
    }

    fn read_byte(&mut self) -> u8 {
        let byte = self.program[self.pc];
        self.pc += 1;
        byte
    }

    fn read_8_bytes(&mut self) -> [u8; 8] {
        let mut bytes = [0; 8];
        bytes.copy_from_slice(&self.program[self.pc..self.pc + 8]);
        self.pc += 8;
        bytes
    }

    fn read_u16(&mut self) -> u16 {
        let mut bytes = [0; 2];
        bytes.copy_from_slice(&self.program[self.pc..self.pc + 2]);
        self.pc += 2;
        u16::from_le_bytes(bytes)
    }

    fn read_value(&mut self) -> Value {
        let byte = self.read_byte();
        match byte {
            0 => self.read_int(),
            1 => self.read_float(),
            2 => self.read_bool(),
            3 => self.read_str(),
            4 => self.read_symbol(),
            5 => self.read_list(),
            6 => self.read_tuple(),
            7 => self.read_map(),
            _ => panic!("Invalid value type"),
        }
    }

    fn read_int(&mut self) -> Value {
        let mut bytes = [0; 8];
        bytes.copy_from_slice(&self.program[self.pc..self.pc + 8]);
        self.pc += 8;
        Value::Int(i64::from_le_bytes(bytes))
    }

    fn read_float(&mut self) -> Value {
        let mut bytes = [0; 8];
        bytes.copy_from_slice(&self.program[self.pc..self.pc + 8]);
        self.pc += 8;
        Value::Float(f64::from_le_bytes(bytes))
    }

    fn read_bool(&mut self) -> Value {
        let byte = self.read_byte();
        match byte {
            0 => Value::Bool(false),
            1 => Value::Bool(true),
            _ => panic!("Invalid bool value"),
        }
    }

    fn read_str(&mut self) -> Value {
        let len = self.read_byte();
        let mut bytes = vec![0; len as usize];
        bytes.copy_from_slice(&self.program[self.pc..self.pc + len as usize]);
        self.pc += len as usize;
        Value::String(String::from_utf8(bytes).expect("Invalid string").into())
    }

    fn read_symbol(&mut self) -> Value {
        let len = self.read_byte();
        let mut bytes = vec![0; len as usize];
        bytes.copy_from_slice(&self.program[self.pc..self.pc + len as usize]);
        self.pc += len as usize;
        Value::Symbol(String::from_utf8(bytes).expect("Invalid symbol").into())
    }

    fn read_list(&mut self) -> Value {
        let len = self.read_byte();
        let mut list = vec![];
        for _ in 0..len {
            let value = self.read_value();
            list.push(value);
        }
        Value::List(list.into())
    }

    fn read_tuple(&mut self) -> Value {
        let len = self.read_byte();
        let mut tuple = vec![];
        for _ in 0..len {
            let value = self.read_value();
            tuple.push(value);
        }
        Value::Tuple(tuple)
    }

    fn read_map(&mut self) -> Value {
        let len = self.read_byte();
        let mut map = HashMap::new();
        for _ in 0..len {
            let key = match self.read_value() {
                Value::Symbol(s) => s,
                _ => panic!("Invalid map key"),
            };

            let value = self.read_value();
            map.insert(key.into(), value);
        }
        Value::Map(map)
    }

    fn read_record(&mut self) -> Value {
        let name = match self.read_value() {
            Value::Symbol(s) => s,
            _ => panic!("Invalid struct name"),
        };

        let len = self.read_byte();
        let mut fields = HashMap::new();
        for _ in 0..len {
            let key = match self.read_value() {
                Value::Symbol(s) => s,
                _ => panic!("Invalid struct field name"),
            };

            let value = self.read_value();
            fields.insert(key.into(), value);
        }
        Value::Record(Record { name, fields })
    }

    fn read_instr(&mut self) -> Instr {
        let byte = self.read_byte();
        match byte {
            1 => Instr::Load(self.read_u16()),
            2 => Instr::Store(self.read_value()),
            3 => Instr::LoadConst(self.read_u16()),
            4 => Instr::StoreConst(self.read_value()),
            5 => Instr::Push(self.read_value()),
            6 => Instr::Pop,
            7 => Instr::Dup,
            8 => Instr::Swap,
            9 => Instr::Drop,
            10 => Instr::Neg,
            11 => Instr::Add,
            12 => Instr::Sub,
            13 => Instr::Mul,
            14 => Instr::Div,
            15 => Instr::And,
            16 => Instr::Or,
            17 => Instr::Xor,
            18 => Instr::Not,
            19 => Instr::Eq,
            20 => Instr::Neq,
            21 => Instr::Lt,
            22 => Instr::Gt,
            23 => Instr::Jump(self.read_u16()),
            24 => Instr::Call(self.read_u16()),
            25 => Instr::Return,
            26 => Instr::Jeq(self.read_u16()),
            27 => Instr::Halt,
            _ => panic!("Invalid opcode"),
        }
    }

    fn push(&mut self, value: Value) {
        self.stack.push(value);
    }

    fn pop(&mut self) -> Value {
        self.stack.pop().expect("Stack underflow")
    }
}
