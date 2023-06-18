use crate::{
    parser::{
        ast::{Expr, Item, Lit},
        parse,
    },
    vm::{chunk::Chunk, opcode::OpCode, value::Value},
};
use std::fmt::Display;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CompilerError(pub String);

impl CompilerError {
    pub fn new(msg: &str) -> Self {
        Self(msg.to_string())
    }
}

impl Display for CompilerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

pub type Result<T> = std::result::Result<T, CompilerError>;

#[derive(Debug)]
pub struct Compiler {
    chunk: Chunk,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            chunk: Chunk::new(),
        }
    }

    pub fn compile(&mut self, ast: &Item) -> Result<Chunk> {
        match ast {
            Item::Expr(expr) => self.compile_expr(expr),
            _ => todo!(),
        }

        Ok(self.chunk.clone())
    }

    fn emit_byte(&mut self, byte: u8) {
        self.chunk.write(byte, 0)
    }

    fn emit_bytes(&mut self, byte1: u8, byte2: u8) {
        self.emit_byte(byte1);
        self.emit_byte(byte2);
    }

    fn make_const(&mut self, val: Value) -> u8 {
        let constant = self.chunk.add_constant(val);
        if constant > u8::MAX as usize {
            log::error!("Too many constants in one chunk!");
            return 0;
        }
        constant as u8
    }

    fn emit_const(&mut self, val: Value) {
        let c = self.make_const(val);
        self.emit_bytes(OpCode::Const as u8, c);
    }

    pub fn compile_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::Lit(lit) => self.compile_lit(lit),
            _ => todo!(),
        }
    }

    fn compile_lit(&mut self, lit: &Lit) {
        match lit {
            Lit::Int(i) => self.emit_const(Value::Int(*i)),
            Lit::Rational(r) => self.emit_const(Value::Rational(*r)),
            Lit::Real(f) => self.emit_const(Value::Real(*f)),
            Lit::Bool(b) => self.emit_const(Value::Bool(*b)),
            Lit::Char(c) => self.emit_const(Value::Char(*c)),
            Lit::String(s) => self.emit_const(Value::String(s.clone())),
            Lit::List(l) => todo!(),
            Lit::Complex(_) => todo!(),
            Lit::Tuple(_) => todo!(),
            Lit::Map(_) => todo!(),
            Lit::Record(_) => todo!(),
            Lit::Lambda(_) => todo!(),
        }
    }
}
