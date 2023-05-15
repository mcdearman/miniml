use std::{cell::RefCell, rc::Rc};

use interpreter::{eval, Env};
use parser::Parser;

mod compiler;
mod intern;
mod interpreter;
mod list;
mod parser;
mod vm;

fn main() {
    let input = "\"Hello, World!\"";
    let ast = Parser::new(input).item().expect("failed to parse");
    match ast {
        parser::Item::Data(_) => todo!(),
        parser::Item::Decl(_) => todo!(),
        parser::Item::Expr(e) => {
            println!("{:?}", eval(Rc::new(RefCell::new(Env::new())), &e));
        }
    }
}
