use self::parser::Parser;

pub mod ast;
// pub mod context_handler;
// pub mod hir;
pub mod node;
pub mod parser;
pub mod span;
// pub mod unique_id;

#[derive(Debug)]
pub struct Compiler<'src, P: Parser> {
    src: &'src str,
    parser: P,
    // ctx_handler: C,
}
