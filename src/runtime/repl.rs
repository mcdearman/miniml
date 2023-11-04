use crate::{
    analysis::{
        infer::{type_inference, Context},
        res::{self, resolve},
    },
    runtime::tree_walk::{self, eval, Env},
    syntax::parse::parse,
};
use std::io::{self, Write};

pub fn repl() {
    println!("Welcome to the MiniML REPL!");
    print!("> ");
    io::stdout().flush().expect("failed to flush stdout");
    let mut src = String::new();
    let res_env = res::Env::new();
    let eval_env = tree_walk::Env::new();
    loop {
        io::stdin()
            .read_line(&mut src)
            .expect("failed to read from stdin");
        let (ast, errors) = parse(&src);
        if !errors.is_empty() {
            println!("parse errors: {:?}", errors);
            src.clear();
            print!("\n> ");
            io::stdout().flush().expect("failed to flush stdout");
            continue;
        }
        let (res, errors) = resolve(res_env.clone(), &ast.unwrap());
        if !errors.is_empty() {
            println!("resolve errors: {:?}", errors);
            src.clear();
            print!("\n> ");
            io::stdout().flush().expect("failed to flush stdout");
            continue;
        }
        let ctx = Context::new();
        match type_inference(ctx, res.unwrap()) {
            Ok(root) => match eval(&src, eval_env.clone(), &root) {
                Ok(val) => println!("{}", val),
                Err(errors) => println!("evaluation errors: {:?}", errors),
            },
            Err(errors) => println!("inference errors: {:?}", errors),
        }
        src.clear();
        print!("\n> ");
        io::stdout().flush().expect("failed to flush stdout");
    }
}
