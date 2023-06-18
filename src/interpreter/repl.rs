use crate::{
    interpreter::{eval, handle_decl, Env},
    parser::{ast::Item, parse},
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
    let env = Rc::new(RefCell::new(Env::new()));
    loop {
        io::stdin()
            .read_line(&mut src)
            .expect("failed to read from stdin");
        match parse(&src) {
            (Some(item), _) => match item {
                Item::Data(d) => todo!(),
                Item::Decl(d) => match handle_decl(env.clone(), &d) {
                    Ok(()) => println!("{}", d),
                    Err(e) => eprintln!("Runtime Error: {}", e),
                },
                Item::Expr(e) => match eval(env.clone(), &e) {
                    Ok(v) => println!("{}", v),
                    Err(e) => eprintln!("Runtime Error: {}", e),
                },
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
