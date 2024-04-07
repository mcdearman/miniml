use crate::{analysis::infer::tir, lex::token_stream::TokenStream, parse::parse};
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
        // let (ast, parse_errors) = parse(stream, true);
        let ast = match parse(stream, true) {
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

        let nir = match res.resolve(&ast) {
            (Some(nir), _) => {
                log::debug!("{:#?}", nir);
                src.clear();
                nir
            }
            (_, errors) => {
                println!("{:#?}", errors);
                src.clear();
                continue;
            }
        };

        let tir = match solver.infer(&src, &nir) {
            (Some(tir), _) => {
                log::debug!("{:#?}", tir);
                src.clear();
                tir
            }
            (_, errors) => {
                println!("Error: {:#?}", errors);
                src.clear();
                continue;
            }
        };
    }
}
