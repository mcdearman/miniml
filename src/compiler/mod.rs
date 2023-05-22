use crate::{
    intern::InternedString,
    parser::{
        ast::{Expr, Lit},
        Parser,
    },
    vm::{chunk::Chunk, instr::Instr, value::Value},
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Local {
    pub name: InternedString,
    pub depth: i8,
}

// pub struct Compiler<'src> {
//     parser: &'src Parser<'src>,
//     locals: Vec<Local>,
//     scope_depth: i8,
// }

// impl<'src> Compiler<'src> {
//     pub fn new(src: &'src str) -> Self {
//         Self {
//             parser: &Parser::new(src),
//             locals: vec![],
//             scope_depth: 0,
//         }
//     }
// }
use std::collections::HashMap;

pub struct Compiler {
    chunk: Chunk,
    symbol_table: HashMap<InternedString, u16>,
    next_constant_index: u16,
    next_instruction_index: u16,
    next_symbol_index: u16,
    next_bool_index: u16,
    next_match_index: u16,
    constants: Vec<Value>,
}

impl Compiler {
    pub fn new() -> Self {
        Compiler {
            chunk: Chunk {
                code: Vec::new(),
                constants: Vec::new(),
            },
            symbol_table: HashMap::new(),
            next_constant_index: 0,
            next_instruction_index: 0,
            next_symbol_index: 0,
            next_bool_index: 0,
            next_match_index: 0,
            constants: vec![],
        }
    }

    pub fn compile(&mut self, expr: &Expr) -> Chunk {
        self.compile_expression(expr);
        self.emit_return();
        self.chunk.clone()
    }

    fn compile_expression(&mut self, expr: &Expr) {
        match expr {
            Expr::Ident(ident) => {
                if let Some(&index) = self.symbol_table.get(ident) {
                    self.emit(Instr::Load(index));
                } else {
                    panic!("Undefined variable: {}", ident);
                }
            }
            Expr::Lit(literal) => match literal {
                Lit::Int(int) => {
                    let i = i64::try_from(int.value.clone()).expect("Integer overflow");
                    let constant_index = self.add_constant(Value::Int(i));
                    self.emit(Instr::LoadConst(constant_index));
                }
                Lit::Real(real) => {
                    let f = f64::try_from(real.0).expect("Integer overflow");
                    let constant_index = self.add_constant(Value::Float(f));
                    self.emit(Instr::LoadConst(constant_index));
                }
                Lit::Bool(value) => {
                    let bool_index = self.next_bool_index;
                    self.next_bool_index += 1;
                    // self.emit(Instr::Load(bool_index, *value));
                    todo!()
                }
                Lit::String(value) => {
                    let constant_index = self.add_constant(Value::String(value.clone()));
                    self.emit(Instr::LoadConst(constant_index));
                }
                Lit::List(values) => {
                    todo!()
                    // let mut list_values = Vec::new();
                    // for value in values.clone() {
                    //     self.compile_expression(&value);
                    //     list_values.push(self.pop_last_instruction());
                    // }
                    // let constant_index = self.add_constant(Value::List(list_values.into()));
                    // self.emit(Instr::LoadConst(constant_index));
                }
                Lit::Tuple(values) => {
                    todo!()
                    // let mut tuple_values = Vec::new();
                    // for value in values {
                    //     self.compile_expression(value);
                    //     tuple_values.push(self.pop_last_instruction());
                    // }
                    // let constant_index = self.add_constant(Value::Tuple(tuple_values));
                    // self.emit(Instr::LoadConst(constant_index));
                }
                Lit::Map(entries) => {
                    todo!()
                    // let mut map_entries = HashMap::new();
                    // for (key, value) in entries {
                    //     self.compile_expression(key);
                    //     let key_value = self.pop_last_instruction();
                    //     self.compile_expression(value);
                    //     let value_value = self.pop_last_instruction();
                    //     map_entries.insert(key_value, value_value);
                    // }
                    // let constant_index = self.add_constant(Value::Map(map_entries));
                    // self.emit(Instr::LoadConst(constant_index));
                }
                Lit::Record(record_lit) => {
                    todo!()
                    // let compiled_struct = self.compile_struct_literal(struct_lit);
                    // let constant_index = self.add_constant(Value::Struct(compiled_struct));
                    // self.emit(Instr::LoadConst(constant_index));
                }
                Lit::Rational(_) => todo!(),
                Lit::Complex(_) => todo!(),
                Lit::Char(_) => todo!(),
                Lit::Lambda(_) => todo!(),
            },
            Expr::Prefix { op, expr } => todo!(),
            Expr::Infix { op, lhs, rhs } => todo!(),
            Expr::Let(_) => todo!(),
            Expr::Fn(_) => todo!(),
            Expr::Apply(_, _) => todo!(),
            Expr::If { cond, then, else_ } => todo!(),
            Expr::Match { expr, arms } => todo!(),
            Expr::Unit => todo!(),
        }
    }

    fn add_constant(&mut self, value: Value) -> u16 {
        let index = self.next_constant_index;
        self.constants.push(value);
        self.next_constant_index += 1;
        index
    }

    // Helper methods for emitting instructions and accessing the last instruction

    fn emit(&mut self, instruction: Instr) {
        self.chunk.code.push(instruction);
        self.next_instruction_index += 1;
    }

    fn emit_return(&mut self) {
        self.emit(Instr::Return);
    }

    fn pop_last_instruction(&mut self) -> Instr {
        self.chunk.code.pop().unwrap()
    }

    // Other helper methods for emitting instructions

    // Implement other helper methods for emitting instructions here
}
