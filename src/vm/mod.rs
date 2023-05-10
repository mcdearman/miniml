use std::{collections::HashMap, fmt::Display};

use crate::{intern::InternedString, list::List};

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Int(i64),
    Float(f64),
    Bool(bool),
    String(InternedString),
    Symbol(InternedString),
    List(List<Value>),
    Tuple(Vec<Value>),
    Map(HashMap<InternedString, Value>),
    Struct(Struct),
    Nil,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Struct {
    pub name: InternedString,
    pub fields: HashMap<InternedString, Value>,
}

impl Display for Struct {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {{", self.name)?;
        for (i, (key, value)) in self.fields.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}: {}", key, value)?;
        }
        write!(f, "}}")
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Int(value) => write!(f, "{}", value),
            Value::Float(value) => write!(f, "{}", value),
            Value::Bool(value) => write!(f, "{}", value),
            Value::String(value) => write!(f, "{}", value),
            Value::Symbol(value) => write!(f, "{}", value),
            Value::List(l) => write!(f, "{}", l),
            Value::Tuple(t) => write!(f, "{:?}", t),
            Value::Map(m) => write!(f, "{:?}", m),
            Value::Struct(s) => write!(f, "{}", s),
            Value::Nil => write!(f, "nil"),
        }
    }
}

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
            Instr::Load(addr) => write!(f, "LOADV"),
            Instr::Store(Value) => write!(f, "STOREV"),
            Instr::LoadConst(addr) => write!(f, "LOADC"),
            Instr::StoreConst(value) => write!(f, "STOREC"),
            Instr::Push(value) => write!(f, "PUSH {}", value),
            Instr::Pop => write!(f, "POP"),
            Instr::Add => write!(f, "ADD"),
            Instr::Sub => write!(f, "SUB"),
            Instr::Mul => write!(f, "MUL"),
            Instr::Div => write!(f, "DIV"),
            Instr::Eq => write!(f, "EQ"),
            Instr::Neq => write!(f, "NEQ"),
            Instr::Lt => write!(f, "LT"),
            Instr::Gt => write!(f, "GT"),
            Instr::Jump => write!(f, "JMP"),
            Instr::Jeq => write!(f, "JEQ"),
            Instr::Halt => write!(f, "HALT"),
        }
    }
}

pub struct VM {
    pc: usize,
    program: Vec<u8>,
    constants: Vec<Value>,
    stack: Vec<Value>,
    frames: Vec<CallFrame>,
    heap: Vec<Value>,
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
            heap: vec![],
        }
    }

    pub fn run(&mut self) {
        while self.pc < self.program.len() {
            let instr = self.read_instr();
            match instr {
                Instr::LoadConst(offset) => {
                    let value = self.constants[offset as usize].clone();
                    self.push(value);
                }
                Instr::LoadVar(offset) => {
                    let value = self.stack[offset as usize].clone();
                    self.push(value);
                }
                Instr::StoreConst(value) => {
                    self.constants.push(value);
                }
                Instr::StoreVar(value) => {
                    self.stack.push(value);
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

    fn read_struct(&mut self) -> Value {
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
        Value::Struct(Struct { name, fields })
    }

    fn read_instr(&mut self) -> Instr {
        let byte = self.read_byte();
        match byte {
            0 => Instr::LoadConst(self.read_byte()),
            1 => Instr::LoadVar(self.read_byte()),
            2 => Instr::StoreConst(self.read_value()),
            3 => Instr::StoreVar(self.read_value()),
            4 => Instr::Alloc(u64::from_le_bytes(self.read_8_bytes())),
            5 => Instr::Neg,
            6 => Instr::Add,
            7 => Instr::Sub,
            8 => Instr::Mul,
            9 => Instr::Div,
            10 => Instr::Eq,
            11 => Instr::Neq,
            12 => Instr::Lt,
            13 => Instr::Gt,
            14 => Instr::Jump,
            15 => Instr::Jeq,
            16 => Instr::Halt,
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
