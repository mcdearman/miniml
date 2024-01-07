use repl::repl;

mod parse;
mod rename;
mod repl;
mod utils;
mod vm;

fn main() {
    env_logger::init();
    repl();
}
