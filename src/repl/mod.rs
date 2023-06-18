use crate::{
    compiler::Compiler,
    parser::{ast::Item, parse},
    vm::VM,
};
use std::{
    cell::RefCell,
    io::{self, Write},
    rc::Rc,
};

pub fn repl() {
    println!("Welcome to the MiniML REPL!");
    print!("> ");
    io::stdout().flush().expect("failed to flush stdout");
    let mut src = String::new();
    loop {
        io::stdin()
            .read_line(&mut src)
            .expect("failed to read from stdin");
        match &parse(&src) {
            (Some(item), _) => match Compiler::new().compile(item) {
                Ok(chunk) => {
                    // println!("AST: {:?}", item);
                    // println!("{:?}", chunk);
                    match VM::new(chunk).run() {
                        Ok(val) => println!("{}", val),
                        Err(err) => eprintln!("Runtime Error: {}", err),
                    }
                }
                Err(err) => {
                    eprintln!("Compiler Error: {}", err);
                }
            },
            (None, errs) => {
                for err in errs {
                    eprintln!("Parser Error: {}", err);
                }
            }
        }
        src.clear();
        print!("\n> ");
        io::stdout().flush().expect("failed to flush stdout");
    }
}
