use string_interner::StringInterner;

// pub mod ast;
// pub mod context_handler;
// pub mod hir;
pub mod interned_string;
pub mod node;
// pub mod parser;
pub mod span;
pub mod string_interner;
// pub mod unique_id;

#[derive(Debug, Clone)]
pub struct Compiler<'src, I: StringInterner> {
    src: &'src str,
    interner: I,
    // parser: P,
    // ctx_handler: C,
}
