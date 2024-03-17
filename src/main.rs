use crate::{lex::token_stream::TokenStream, parse::parse};
use analysis::infer::TypeSolver;
use rename::resolver::Resolver;
use std::io::{self, Write};

mod analysis;
mod lex;
mod parse;
mod rename;
// mod runtime;
mod utils;

fn main() {
    env_logger::init();
    let mut src = String::new();
    let mut res = Resolver::new();
    let builtins = res.builtins();
    let scoped_interner = res.env().dump_to_interner();
    let mut solver = TypeSolver::new(builtins.clone(), scoped_interner);

    loop {
        print!("> ");
        io::stdout().flush().unwrap();
        std::io::stdin().read_line(&mut src).unwrap();

        let stream = TokenStream::new(&src);
        let expr = match parse(stream) {
            Ok(ast) => {
                // println!("AST: {:#?}", ast);
                ast
            }
            Err(e) => {
                println!("Error: {:#?}", e);
                src.clear();
                continue;
            }
        };

        let nir = match res.resolve_expr(&expr) {
            Ok(nir) => {
                // println!("NIR: {:#?}", nir);
                nir
            }
            Err(e) => {
                println!("Error: {:#?}", e);
                src.clear();
                continue;
            }
        };

        let tir = match solver.infer(&src, &nir) {
            Ok(tir) => {
                println!("{:?}", tir);
                src.clear();
                tir
            }
            Err(e) => {
                println!("Error: {:#?}", e);
                src.clear();
                continue;
            }
        };
    }
}

// fn main() {
//     env_logger::init();
//     let src = fs::read_to_string("examples/notebook.mn").expect("failed to read file");
//     let stream = TokenStream::new(&*src);
//     // println!("TOKENS: {:#?}", stream.tokenize());
//     let root = match parse(stream, false) {
//         (Some(root), errors) => {
//             if errors.is_empty() {
//                 root
//             } else {
//                 println!("Errors: {:#?}", errors);
//                 return;
//             }
//         }
//         (None, errors) => {
//             println!("Errors: {:#?}", errors);
//             return;
//         }
//     };
//     // println!("AST: {:#?}", root);

//     let mut res = Resolver::new();
//     // println!("Builtins: {:#?}", builtins);
//     // let res_env = rename::env::Env::new_with_builtins(builtins.clone());
//     let (nir, errors) = res.resolve(root);
//     if !errors.is_empty() {
//         println!("Errors: {:#?}", errors);
//         println!("env: {:#?}", res.env());
//         return;
//     }
//     let builtins = res.builtins();
//     // println!("Builtins: {:#?}", builtins);
//     // println!("NIR: {:#?}", nir);
//     if let Some(root) = nir.clone() {
//         if !errors.is_empty() {
//             panic!("{:#?}", errors);
//         }
//         // println!("NIR: {:#?}", root);

//         let scoped_interner = res.env().dump_to_interner();

//         let mut solver = TypeSolver::new(&*src, root, builtins.clone(), scoped_interner);
//         if let (Some(tir), errors) = solver.solve() {
//             if !errors.is_empty() {
//                 println!("Errors: {:#?}", errors);
//                 return;
//             }
//             println!("TIR: {:#?}", tir);

//             // let env = default_env(builtins.clone());
//             // match eval(&*src, env.clone(), tir) {
//             //     Ok(val) => {
//             //         println!("{}", val);
//             //     }
//             //     Err(err) => {
//             //         println!("Error: {:#?}", err);
//             //     }
//             // }
//         };
//     }
// }
