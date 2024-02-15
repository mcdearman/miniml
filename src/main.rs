use crate::{lex::token_stream::TokenStream, parse::parse};
use rename::{nir, resolver::Resolver};
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
    let builtins = res.builtins();
    println!("Builtins: {:#?}", builtins);
    let res_env = rename::env::Env::new_with_builtins(builtins.clone());
    let (nir, errors) = res.resolve(res_env.clone(), &root);
    if let Some(root) = nir.clone() {
        println!("NIR: {:#?}", root);
    } else {
        println!("Errors: {:#?}", errors);
    }

    let mut ctx = infer::context::Context::new();
    match infer::infer(&*src, &mut ctx, &nir.unwrap()) {
        Ok((tir, ctx)) => {
            println!("TIR: {:#?}", tir);
            println!("Context: {:#?}", ctx);
        }
        Err(err) => {
            println!("Error: {:#?}", err);
        }
    }
}
