use crate::{
    infer::TypeSolver,
    lex::token_stream::TokenStream,
    parse::parse,
    runtime::{default_env, interpreter::eval},
};
use rename::resolver::Resolver;
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
    let stream = TokenStream::new(&*src);
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
    // println!("Builtins: {:#?}", builtins);
    // let res_env = rename::env::Env::new_with_builtins(builtins.clone());
    let (nir, errors) = res.resolve(root);
    if !errors.is_empty() {
        println!("Errors: {:#?}", errors);
        println!("env: {:#?}", res.env());
        return;
    }
    let builtins = res.builtins();
    // println!("Builtins: {:#?}", builtins);
    // println!("NIR: {:#?}", nir);
    if let Some(root) = nir.clone() {
        if !errors.is_empty() {
            panic!("{:#?}", errors);
        }
        // println!("NIR: {:#?}", root);

        let mut solver = TypeSolver::new(&*src, root, builtins.clone());
        if let (Some(tir), errors) = solver.solve() {
            if !errors.is_empty() {
                println!("Errors: {:#?}", errors);
                return;
            }
            println!("TIR: {:#?}", tir);

            let env = default_env(builtins.clone());
            match eval(&*src, env.clone(), tir) {
                Ok(val) => {
                    println!("{}", val);
                }
                Err(err) => {
                    println!("Error: {:#?}", err);
                }
            }
        };
    }
}
