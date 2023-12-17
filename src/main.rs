mod compiler;
mod lexer;
mod num;
mod object;
mod span;
mod token;

fn main() {
    env_logger::init();

    // open examples/notebook.mn
    let src = std::fs::read_to_string("examples/notebook.mn").unwrap();
    let mut comp = compiler::Compiler::new(&src);
    comp.run();
}
