// use crate::error::CompileResult;
// use miniml_syntax::ast::{Decl, Expr, InfixOp, Lit, PrefixOp, Root};
// use miniml_util::{
//     intern::InternedString,
//     span::{Span, Spanned},
// };
// use miniml_vm::{
//     chunk::Chunk,
//     object::{Function, Object},
//     opcode::OpCode,
//     value::Value,
// };
// use std::fmt::Display;

// #[derive(Debug, Clone, PartialEq, Eq, Hash)]
// pub struct CompilerError(pub String);

// impl CompilerError {
//     pub fn new(msg: &str) -> Self {
//         Self(msg.to_string())
//     }
// }

// impl Display for CompilerError {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         write!(f, "{:?}", self)
//     }
// }

// pub type Result<T> = std::result::Result<T, CompilerError>;

// #[derive(Debug, Clone, PartialEq)]
// pub struct Local {
//     name: InternedString,
//     depth: usize,
// }

// #[derive(Debug, Clone)]
// pub struct Compiler {
//     parent: Option<Box<Compiler>>,
//     fun: Function,
//     top_level: bool,
//     locals: Vec<Local>,
// }

// impl Compiler {
//     pub fn new() -> Self {
//         Self {
//             parent: None,
//             fun: Function::new(0, Box::new(Chunk::new()), ""),
//             top_level: true,
//             locals: vec![],
//         }
//     }

//     fn chunk<'a>(&'a mut self) -> &'a mut Chunk {
//         &mut self.fun.chunk
//     }

//     pub fn compile(&mut self, ast: &Root) -> CompileResult<Function> {
//         for d in &ast.decls {
//             self.compile_decl(&d.value);
//         }
//         self.chunk().write(OpCode::Return as u8, Span::from(0..0));

//         Ok(self.fun.clone())
//     }

//     fn emit_byte(&mut self, byte: u8) {
//         self.chunk().write(byte, Span::from(0..0))
//     }

//     fn emit_bytes(&mut self, byte1: u8, byte2: u8) {
//         self.emit_byte(byte1);
//         self.emit_byte(byte2);
//     }

//     fn emit_jump(&mut self, op: OpCode) -> usize {
//         self.emit_byte(op as u8);
//         self.emit_byte(0xff);
//         self.emit_byte(0xff);
//         self.chunk().code.len() - 2
//     }

//     fn patch_jump(&mut self, offset: usize) {
//         let jump = self.chunk().code.len() - offset - 2;
//         if jump > u16::MAX as usize {
//             log::error!("Too much code to jump over!");
//             return;
//         }
//         self.chunk().code[offset] = ((jump >> 8) & 0xff) as u8;
//         self.chunk().code[offset + 1] = (jump & 0xff) as u8;
//     }

//     fn make_const(&mut self, val: Value) -> u8 {
//         let idx = self.chunk().add_constant(val);
//         if idx > u8::MAX as usize {
//             log::error!("Too many constants in one chunk!");
//             return 1;
//         }
//         idx as u8
//     }

//     fn emit_const(&mut self, val: Value) {
//         let c = self.make_const(val);
//         self.emit_bytes(OpCode::Const as u8, c);
//     }

//     fn define_var(&mut self, name: InternedString) {
//         let idx = self.make_const(Value::String(name));
//         self.emit_bytes(OpCode::DefineGlobal as u8, idx);
//     }

//     fn compile_decl(&mut self, decl: &Decl) {
//         match decl.clone() {
//             Decl::Const { name, expr } => {
//                 self.compile_expr(&expr.value);
//                 self.define_var(name.value);
//             }
//             Decl::Let { name, expr } => {
//                 self.compile_expr(&expr.value);
//                 self.define_var(name.value);
//             }
//             Decl::Fn { name, params, body } => {
//                 let fun = Function::new(params.len(), Box::new(Chunk::new()), &name.value);
//                 let mut compiler = Compiler {
//                     parent: Some(Box::new(self.clone())),
//                     fun,
//                     top_level: false,
//                     locals: vec![],
//                 };
//                 for p in &params {
//                     compiler.locals.push(Local {
//                         name: p.value,
//                         depth: 0,
//                     });
//                 }
//                 compiler.compile_expr(&body.value);
//                 compiler
//                     .chunk()
//                     .write(OpCode::Return as u8, Span::from(1..0));
//                 let idx = self.make_const(Value::Object(Object::Function(compiler.fun)));
//                 self.emit_bytes(OpCode::Const as u8, idx);
//                 self.define_var(name.value);
//             }
//         }
//     }

//     fn compile_expr(&mut self, expr: &Expr) {
//         match expr.clone() {
//             Expr::Ident(name) => {
//                 let idx = self.make_const(Value::String(name));
//                 self.emit_bytes(OpCode::GetGlobal as u8, idx);
//             }
//             Expr::Lit(lit) => match lit {
//                 Lit::Int(i) => self.emit_const(Value::Int(i)),
//                 Lit::Real(r) => self.emit_const(Value::Real(r)),
//                 Lit::String(s) => self.emit_const(Value::String(s)),
//             },
//             Expr::Prefix { op, expr } => match op.clone().value {
//                 PrefixOp::Neg => {
//                     self.compile_expr(&expr.value);
//                     self.emit_byte(OpCode::Neg as u8);
//                 }
//                 PrefixOp::Not => todo!(),
//             },
//             Expr::Infix { op, lhs, rhs } => {
//                 self.compile_expr(&lhs.value);
//                 self.compile_expr(&rhs.value);
//                 match op.clone().value {
//                     InfixOp::Add => self.emit_byte(OpCode::Add as u8),
//                     InfixOp::Sub => self.emit_byte(OpCode::Sub as u8),
//                     InfixOp::Mul => self.emit_byte(OpCode::Mul as u8),
//                     InfixOp::Div => self.emit_byte(OpCode::Div as u8),
//                     _ => todo!(),
//                 }
//             }
//             _ => todo!(),
//         }
//     }
// }