use crate::{lex::token_stream::TokenStream, parse::parse};
// use analysis::infer::TypeSolver;
// use rename::resolver::Resolver;
use std::io::{self, Write};

// mod analysis;
mod lex;
mod parse;
// mod rename;
// mod runtime;
mod utils;

fn main() {
    env_logger::init();
    let mut src = String::new();
    // let mut res = Resolver::new();
    // let builtins = res.builtins();
    // let scoped_interner = res.env().dump_to_interner();
    // let mut solver = TypeSolver::new(builtins.clone(), scoped_interner);

    loop {
        print!("> ");
        io::stdout().flush().unwrap();
        std::io::stdin().read_line(&mut src).unwrap();

        let stream = TokenStream::new(&src);
        if let (Some(ast), parse_errors) = parse(stream) {
            if !parse_errors.is_empty() {
                for error in parse_errors {
                    println!("Error: {:#?}", error);
                }
                src.clear();
                continue;
            }
            println!("{:#?}", ast);
        }

        //     let nir = match res.resolve(&expr) {
        //         Ok(nir) => {
        //             // log::debug!("{:?}", nir);
        //             nir
        //         }
        //         Err(e) => {
        //             println!("Error: {:#?}", e);
        //             src.clear();
        //             continue;
        //         }
        //     };

        //     let tir = match solver.infer(&src, &nir) {
        //         Ok(tir) => {
        //             log::debug!("{:?}", tir);
        //             src.clear();
        //             tir
        //         }
        //         Err(e) => {
        //             println!("Error: {:#?}", e);
        //             src.clear();
        //             continue;
        //         }
        //     };
    }
}
