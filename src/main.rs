use std::{cell::RefCell, rc::Rc};

use clap::Parser;
use interpreter::{eval, repl::repl};
use parser::ast::Item;

mod compiler;
mod intern;
mod interpreter;
mod list;
mod parser;
mod vm;

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Cli {
    filepath: Option<String>,

    #[arg(short, long, action = clap::ArgAction::Count)]
    debug: u8,
}

fn main() {
    let args = Cli::parse();

    if let Some(filepath) = args.filepath {
        let src = std::fs::read_to_string(filepath).expect("failed to read file");
        let env = Rc::new(RefCell::new(interpreter::Env::new()));
        match &parser::Parser::new(&src).item().expect("failed to parse") {
            Item::Data(_) => todo!(),
            Item::Decl(d) => {
                interpreter::handle_decl(env.clone(), d).expect("failed to handle decl")
            }
            Item::Expr(e) => println!("{}", eval(env.clone(), e).expect("failed to eval")),
        }
    } else {
        repl();
    }
}

// fn main() {

// }
