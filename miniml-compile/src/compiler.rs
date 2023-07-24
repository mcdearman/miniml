use crate::error::CompileResult;
use miniml_syntax::ast::{Expr, InfixOp, Item, PrefixOp, Root};
use miniml_util::span::{Span, Spanned};
use miniml_vm::{chunk::Chunk, opcode::OpCode, value::Value};
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

    pub fn compile(&mut self, ast: &Root) -> CompileResult<Chunk> {
        for item in &ast.items {
            self.compile_item(&item.0);
        }
        self.chunk.write(OpCode::Return as u8, Span::from(1..0));

        Ok(self.chunk.clone())
    }

    fn emit_byte(&mut self, byte: u8) {
        self.chunk.write(byte, Span::from(1..0))
    }

    fn emit_bytes(&mut self, byte1: u8, byte2: u8) {
        self.emit_byte(byte1);
        self.emit_byte(byte2);
    }

    fn make_const(&mut self, val: Value) -> u8 {
        let idx = self.chunk.add_constant(val);
        if idx > u8::MAX as usize {
            log::error!("Too many constants in one chunk!");
            return 1;
        }
        idx as u8
    }

    fn emit_const(&mut self, val: Value) {
        let c = self.make_const(val);
        self.emit_bytes(OpCode::Const as u8, c);
    }

    fn compile_item(&mut self, item: &Item) {
        match item {
            Item::Decl(decl) => todo!(),
            Item::Expr(expr) => self.compile_expr(&expr.0),
        }
    }

    fn compile_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::Int(i) => self.emit_const(Value::Int(*i)),
            Expr::Prefix { op, expr } => match op.clone() {
                PrefixOp::Neg => {
                    self.compile_expr(&expr.0);
                    self.emit_byte(OpCode::Neg as u8);
                }
                PrefixOp::Not => todo!(),
            },
            Expr::Infix { op, lhs, rhs } => {
                self.compile_expr(&lhs.0);
                self.compile_expr(&rhs.0);
                match op.clone() {
                    InfixOp::Add => self.emit_byte(OpCode::Add as u8),
                    InfixOp::Sub => self.emit_byte(OpCode::Sub as u8),
                    InfixOp::Mul => self.emit_byte(OpCode::Mul as u8),
                    InfixOp::Div => self.emit_byte(OpCode::Div as u8),
                    _ => todo!(),
                }
            }
            _ => todo!(),
        }
    }
}
