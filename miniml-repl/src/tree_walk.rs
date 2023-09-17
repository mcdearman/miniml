// use miniml_eval::{env::Env, eval::eval};
// use miniml_syntax::parser::Parser;
// use std::{
//     cell::RefCell,
//     io::{self, Write},
// };

// // fn default_env() -> Rc<RefCell<Env>> {
// //     let env = Env::new();
// // }

// pub fn repl() {
//     println!("Welcome to the MiniML REPL!");
//     print!("> ");
//     io::stdout().flush().expect("failed to flush stdout");
//     let mut src = String::new();
//     let env = Env::new();
//     loop {
//         io::stdin()
//             .read_line(&mut src)
//             .expect("failed to read from stdin");
//         let mut parser = Parser::new(&src);
//         match parser.repl_parse() {
//             (root, errors) => {
//                 if !errors.is_empty() {
//                     println!("Parser Errors: {:?}", errors);
//                     continue;
//                 }
//                 // log::trace!("root: {:?}", Format::from(root.clone()));
//                 match eval(env.clone(), &root.value) {
//                     Ok(value) => println!("{}", value),
//                     Err(error) => println!("Runtime Error: {}", error),
//                 }
//             }
//         }
//         src.clear();
//         print!("\n> ");
//         io::stdout().flush().expect("failed to flush stdout");
//     }
// }
