use repl::repl;

mod interpreter;
mod parse;
mod rename;
mod repl;
mod utils;
mod vm;

fn main() {
    env_logger::init();
    repl();
}
