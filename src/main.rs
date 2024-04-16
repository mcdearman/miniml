use crate::{analysis::infer::tir, lex::token_iter::TokenIter, parse::parse, runtime::interpreter::eval};
use analysis::infer::TypeSolver;
use rename::resolver::Resolver;
use runtime::default_env;
use std::io::{self, Write};

mod analysis;
mod lex;
mod parse;
mod rename;
mod runtime;
mod utils;

fn main() {
    env_logger::init();
    let mut src = String::new();
    let mut res = Resolver::new();
    let builtins = res.builtins();
    let scoped_interner = res.env().dump_to_interner();
    let mut solver = TypeSolver::new(builtins.clone(), scoped_interner);
    let env = default_env(builtins.clone());

    loop {
        print!("> ");
        io::stdout().flush().unwrap();
        std::io::stdin().read_line(&mut src).unwrap();
        if src.trim() == "sub" {
            println!("{:#?}", solver.sub());
            src.clear();
            continue;
        }

        let stream = TokenIter::new(&src);
        // let (ast, parse_errors) = parse(stream, true);
        let ast = match parse(stream, true) {
            (Some(root), _) => {
                log::debug!("{:#?}", root);
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
                tir
            }
            (_, errors) => {
                println!("Error: {:#?}", errors);
                src.clear();
                continue;
            }
        };
        match eval(&src, env.clone(), tir) {
            Ok(val) => println!("{}", val),
            Err(err) => println!("Error: {:#?}", err),
        }
        src.clear();
    }
}
