use crate::{lex::token_stream::TokenStream, parse::parse};
use infer::TypeSolver;
use rename::{nir, resolver::Resolver};
use runtime::{default_env, eval::eval};
use std::fs;

mod infer;
mod lex;
mod parse;
mod rename;
mod runtime;
mod utils;

fn main() {
    env_logger::init();
    let src = fs::read_to_string("examples/notebook.mn").expect("failed to read file");
    let stream = TokenStream::new(&src);
    // println!("TOKENS: {:#?}", stream.tokenize());
    let root = match parse(stream, false) {
        (Some(root), errors) => {
            if errors.is_empty() {
                root
            } else {
                println!("Errors: {:#?}", errors);
                return;
            }
        }
        (None, errors) => {
            println!("Errors: {:#?}", errors);
            return;
        }
    };
    // println!("AST: {:#?}", root);

    let mut res = Resolver::new();
    let builtins = res.builtins().clone();
    // println!("Builtins: {:#?}", builtins);
    let res_env = rename::env::Env::new_with_builtins(builtins.clone());
    let (nir, errors) = res.resolve(res_env.clone(), &root);
    if let Some(root) = nir.clone() {
        if !errors.is_empty() {
            panic!("{:#?}", errors);
        }
        // println!("NIR: {:#?}", root);

        let mut solver = TypeSolver::new(&*src, root, builtins);
        let tir = match solver.solve() {
            Ok(tir) => {
                println!("TIR: {:#?}", tir);
                tir
            }
            Err(err) => {
                panic!("Inference error: {:#?}", err);
            }
        };
        let env = default_env(builtins);
        match eval(&*src, env.clone(), tir) {
            Ok(val) => {
                println!("{}", val);
            }
            Err(err) => {
                println!("Error: {:#?}", err);
            }
        }
    } else {
        println!("Errors: {:#?}", errors);
    }
}
