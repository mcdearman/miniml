use crate::{ast::*, chunk::Chunk, object::Object, opcode::OpCode, parse::parse};

#[derive(Debug, Clone, PartialEq)]
pub enum CompilerError {
    ParseError,
    NameResolutionError,
}

pub type CompilerResult<T> = Result<T, CompilerError>;

#[derive(Debug, Clone, Default)]
pub struct Compiler {
    chunk: Chunk,
    locals: Vec<Local>,
    scope_depth: usize,
}

impl Compiler {
    pub fn compile<'src>(&mut self, src: &'src str) -> CompilerResult<Chunk> {
        match parse(src) {
            Ok(root) => {
                // log::trace!("{:#?}", root);
                for item in root.items() {
                    log::trace!("item: {:#?}", item);
                    // println!("item: {:?}", item);
                    self.compile_item(item)?;
                    self.chunk.write(OpCode::Return as u8);
                }
            }
            Err(err) => {
                println!("{:#?}", err);
            }
        };
        log::trace!("chunk: {}", self.chunk);
        Ok(self.chunk.clone())
    }

    fn compile_item(&mut self, item: &Item) -> CompilerResult<()> {
        match item.kind() {
            ItemKind::Expr(expr) => self.compile_expr(expr),
            ItemKind::Decl(decl) => self.compile_decl(decl),
        }
    }

    fn compile_expr(&mut self, expr: &Expr) -> CompilerResult<()> {
        match expr.kind() {
            ExprKind::Lit(l) => match l.clone() {
                Lit::Num(n) => {
                    let offset = self.chunk.write_data(Object::Num(n));
                    self.chunk.write(OpCode::Const as u8);
                    self.chunk.write(offset as u8);
                }
                Lit::Bool(b) => {
                    let offset = self.chunk.write_data(Object::Bool(b));
                    self.chunk.write(OpCode::Const as u8);
                    self.chunk.write(offset as u8);
                }
                Lit::String(s) => {
                    let offset = self.chunk.write_data(Object::String(s));
                    self.chunk.write(OpCode::Const as u8);
                    self.chunk.write(offset as u8);
                }
            },
            ExprKind::Ident(name) => {
                if let Some(idx) = self.resolve_local(name.name()) {
                    self.chunk.write(OpCode::LoadLocal as u8);
                    self.chunk.write(idx as u8);
                    return Ok(());
                } else if let Some(idx) = self.chunk.in_data(&Object::String(name.to_string())) {
                    self.chunk.write(OpCode::LoadGlobal as u8);
                    self.chunk.write(idx as u8);
                    return Ok(());
                } else {
                    return Err(CompilerError::NameResolutionError);
                }
            }
            ExprKind::Call { fun, args } => todo!(),
            ExprKind::Unary { op, expr } => {
                self.compile_expr(expr)?;
                match op.kind() {
                    UnaryOpKind::Neg => {
                        self.chunk.write(OpCode::Neg as u8);
                    }
                    UnaryOpKind::Not => {
                        self.chunk.write(OpCode::Not as u8);
                    }
                }
            }
            ExprKind::Binary { op, lhs, rhs } => match op.kind() {
                BinaryOpKind::Add => {
                    self.compile_expr(lhs)?;
                    self.compile_expr(rhs)?;
                    self.chunk.write(OpCode::Add as u8);
                }
                BinaryOpKind::Sub => {
                    self.compile_expr(lhs)?;
                    self.compile_expr(rhs)?;
                    self.chunk.write(OpCode::Sub as u8);
                }
                BinaryOpKind::Mul => {
                    self.compile_expr(lhs)?;
                    self.compile_expr(rhs)?;
                    self.chunk.write(OpCode::Mul as u8);
                }
                BinaryOpKind::Div => {
                    self.compile_expr(lhs)?;
                    self.compile_expr(rhs)?;
                    self.chunk.write(OpCode::Div as u8);
                }
                BinaryOpKind::Rem => {
                    self.compile_expr(lhs)?;
                    self.compile_expr(rhs)?;
                    self.chunk.write(OpCode::Rem as u8);
                }
                BinaryOpKind::Pow => {
                    self.compile_expr(lhs)?;
                    self.compile_expr(rhs)?;
                    self.chunk.write(OpCode::Pow as u8);
                }
                BinaryOpKind::Eq => {
                    self.compile_expr(lhs)?;
                    self.compile_expr(rhs)?;
                    self.chunk.write(OpCode::Eq as u8);
                }
                BinaryOpKind::Neq => {
                    self.compile_expr(lhs)?;
                    self.compile_expr(rhs)?;
                    self.chunk.write(OpCode::Neq as u8);
                }
                BinaryOpKind::Lt => {
                    self.compile_expr(lhs)?;
                    self.compile_expr(rhs)?;
                    self.chunk.write(OpCode::Lt as u8);
                }
                BinaryOpKind::Lte => {
                    self.compile_expr(lhs)?;
                    self.compile_expr(rhs)?;
                    self.chunk.write(OpCode::Leq as u8);
                }
                BinaryOpKind::Gt => {
                    self.compile_expr(lhs)?;
                    self.compile_expr(rhs)?;
                    self.chunk.write(OpCode::Gt as u8);
                }
                BinaryOpKind::Gte => {
                    self.compile_expr(lhs)?;
                    self.compile_expr(rhs)?;
                    self.chunk.write(OpCode::Geq as u8);
                }
                BinaryOpKind::And => {
                    self.compile_expr(lhs)?;
                    self.compile_expr(rhs)?;
                    self.chunk.write(OpCode::And as u8);
                }
                BinaryOpKind::Or => {
                    self.compile_expr(lhs)?;
                    self.compile_expr(rhs)?;
                    self.chunk.write(OpCode::Or as u8);
                }
            },
            ExprKind::If { cond, then, else_ } => todo!(),
            ExprKind::Let { name, expr, body } => {
                self.scope_depth += 1;
                self.compile_expr(expr)?;
                self.chunk.write(OpCode::StoreLocal as u8);
                let idx = self.add_local(name.to_string());
                self.chunk.write(idx as u8);
                self.compile_expr(body)?;
                self.scope_depth -= 1;
                while let Some(local) = self.locals.last() {
                    if local.depth() > self.scope_depth {
                        self.chunk.write(OpCode::Pop as u8);
                        self.locals.pop();
                    } else {
                        break;
                    }
                }
            }
            ExprKind::Fn {
                name,
                params,
                expr,
                body,
            } => todo!(),
            ExprKind::Lambda { params, expr } => todo!(),
            ExprKind::Unit => todo!(),
        }
        Ok(())
    }

    fn compile_decl(&mut self, decl: &Decl) -> CompilerResult<()> {
        match decl.kind() {
            DeclKind::Let { name, expr } => {
                self.compile_expr(expr)?;
                self.chunk.write(OpCode::StoreGlobal as u8);
                let idx = self.chunk.write_data(Object::String(name.to_string()));
                log::trace!("idx: {}", idx);
                self.chunk.write(idx as u8);
                self.chunk.write(OpCode::LoadGlobal as u8);
                self.chunk.write(idx as u8);
            }
            DeclKind::Fn { name, params, expr } => todo!(),
        }
        Ok(())
    }

    fn resolve_local(&mut self, name: &str) -> Option<usize> {
        for (idx, local) in self.locals.iter().enumerate().rev() {
            if local.name() == name {
                return Some(idx);
            }
        }
        None
    }

    fn add_local(&mut self, name: String) -> usize {
        self.locals.push(Local::new(name, self.scope_depth));
        self.locals.len() - 1
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Local {
    name: String,
    depth: usize,
}

impl Local {
    pub fn new(name: String, depth: usize) -> Self {
        Self { name, depth }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn depth(&self) -> usize {
        self.depth
    }
}
