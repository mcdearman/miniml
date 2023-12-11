use parser::Parser;
use string_interner::StringInterner;

pub mod interned_string;
pub mod node;
pub mod parser;
pub mod string_interner;

#[derive(Debug, Clone)]
pub struct Compiler<'src, I: StringInterner, P: Parser> {
    src: &'src str,
    interner: I,
    parser: P,
}
