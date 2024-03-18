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
