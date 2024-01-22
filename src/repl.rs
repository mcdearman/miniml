use std::io::{self, Write};

use crate::db::Database;

pub fn repl() {
    let mut src = String::new();
    // let mut compiler = Compiler::default();
    // let mut vm = Interpreter::default();
    let mut db = Database::new();
    loop {
        print!("> ");
        io::stdout().flush().unwrap();
        io::stdin()
            .read_line(&mut src)
            .expect("Failed to read line");
        match src.trim() {
            "db" => {
                println!("db: {:?}\n", db.clone());
                src.clear();
                continue;
            }
            // "ast" => {
            //     println!("ast: {:?}\n", interpreter.ast());
            //     src.clear();
            //     continue;
            // }
            // "res_env" => {
            //     println!("res: {:?}\n", interpreter.res_env());
            //     src.clear();
            //     continue;
            // }
            "exit" => break,
            _ => (),
        }
        // let
        // match interpreter.eval(&*src) {
        //     Ok(obj) => println!(""),
        //     Err(err) => println!("Error: {:?}", err),
        // }
        // log::trace!("src: {:?}", src);
        // match compiler.compile(&*src) {
        //     Ok(chunk) => match vm.exec(chunk) {
        //         Ok(obj) => println!("{}\n", obj),
        //         Err(err) => println!("Error: {:?}", err),
        //     },
        //     Err(err) => println!("Error: {:?}", err),
        // }
        io::stdout().flush().unwrap();
        src.clear();
    }
}
