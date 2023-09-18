use std::io::{self, Write};

use miniml_syntax::parse;

pub fn repl() {
    println!("Welcome to the MiniML REPL!");
    print!("> ");
    io::stdout().flush().expect("failed to flush stdout");
    let mut src = String::new();
    // let env = Env::new();
    loop {
        io::stdin()
            .read_line(&mut src)
            .expect("failed to read from stdin");
        match parse(&*src, true) {
            (Some(root), errors) => {
                if !errors.is_empty() {
                    println!("errors: {:?}", errors);
                } else {
                    println!("root: {:?}", root);
                }
            }
            (None, errors) => {
                println!("errors: {:?}", errors);
            }
        }
        src.clear();
        print!("\n> ");
        io::stdout().flush().expect("failed to flush stdout");
    }
}
