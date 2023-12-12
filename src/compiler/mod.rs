use parser::Parser;
use string_interner::StringInterner;

use self::context_handler::ContextHandler;

pub mod ast;
pub mod context_handler;
pub mod hir;
pub mod interned_string;
pub mod node;
pub mod parser;
pub mod span;
pub mod string_interner;
pub mod unique_id;

#[derive(Debug, Clone)]
pub struct Compiler<'src, I: StringInterner, P: Parser, C: ContextHandler> {
    src: &'src str,
    interner: I,
    parser: P,
    ctx_handler: C,
}
