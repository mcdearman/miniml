use std::io::{self, Write};

use miniml_syntax::reader::read::read;

pub fn repl() {
    let mut src = String::new();
    // let mut compiler = Compiler::default();
    // let mut vm = Interpreter::default();
    loop {
        print!("> ");
        io::stdout().flush().unwrap();
        io::stdin()
            .read_line(&mut src)
            .expect("Failed to read line");
        match read(&*src) {
            Ok(obj) => println!("{}", obj),
            Err(err) => println!("Error: {:?}", err),
        }
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