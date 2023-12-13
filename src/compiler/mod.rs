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

#[derive(Debug)]
pub struct Compiler<'src, I: StringInterner> {
    src: &'src str,
    interner: &'src I,
    // parser: P,
    // ctx_handler: C,
}
