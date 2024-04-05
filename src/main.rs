use crate::{
    lex::token_stream::TokenStream,
    parse::{ast, parse},
};
use rename::resolver::Resolver;
// use analysis::infer::TypeSolver;
use std::io::{self, Write};

// mod analysis;
mod lex;
mod parse;
mod rename;
// mod runtime;
mod utils;

fn main() {
    env_logger::init();
    let mut src = String::new();
    let mut res = Resolver::new();
    // let builtins = res.builtins();
    // let scoped_interner = res.env().dump_to_interner();
    // let mut solver = TypeSolver::new(builtins.clone(), scoped_interner);

    loop {
        print!("> ");
        io::stdout().flush().unwrap();
        std::io::stdin().read_line(&mut src).unwrap();

        let stream = TokenStream::new(&src);
        // let (ast, parse_errors) = parse(stream, true);
        let root = match parse(stream, true) {
            (Some(root), _) => {
                log::debug!("{:#?}", root);
                src.clear();
                root
            }
            (_, errors) => {
                println!("{:#?}", errors);
                src.clear();
                continue;
            }
        };

        // let nir =

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
