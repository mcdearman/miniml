mod infer;
mod lex;
mod parse;
mod rename;
mod runtime;
mod utils;

fn main() {
    env_logger::init();
    // let mut src = String::new();
    // loop {
    //     print!("> ");
    //     io::stdout().flush().unwrap();
    //     io::stdin()
    //         .read_line(&mut src)
    //         .expect("Failed to read line");
    //     match src.trim() {
    //         // "db" => {
    //         //     println!("db: {:?}\n", db.clone());
    //         //     src.clear();
    //         //     continue;
    //         // }
    //         // "ast" => {
    //         //     println!("ast: {:?}\n", interpreter.ast());
    //         //     src.clear();
    //         //     continue;
    //         // }
    //         // "res_env" => {
    //         //     println!("res: {:?}\n", interpreter.res_env());
    //         //     src.clear();
    //         //     continue;
    //         // }
    //         "exit" => break,
    //         _ => (),
    //     }
    //     let root = match parse(&src, true) {
    //         (Some(root), errors) => {
    //             if errors.is_empty() {
    //                 root
    //             } else {
    //                 println!("Errors: {:#?}", errors);
    //                 src.clear();
    //                 continue;
    //             }
    //         }
    //         (None, errors) => {
    //             println!("Errors: {:#?}", errors);
    //             src.clear();
    //             continue;
    //         }
    //     };
    //     println!("root: {:#?}\n", root);
    //     // let
    //     // match interpreter.eval(&*src) {
    //     //     Ok(obj) => println!(""),
    //     //     Err(err) => println!("Error: {:?}", err),
    //     // }
    //     // log::trace!("src: {:?}", src);
    //     // match compiler.compile(&*src) {
    //     //     Ok(chunk) => match vm.exec(chunk) {
    //     //         Ok(obj) => println!("{}\n", obj),
    //     //         Err(err) => println!("Error: {:?}", err),
    //     //     },
    //     //     Err(err) => println!("Error: {:?}", err),
    //     // }
    //     io::stdout().flush().unwrap();
    //     src.clear();
    // }
}
