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
    print!("miniml> ");
    io::stdout().flush().expect("failed to flush stdout");
    let mut src = String::new();
    let res_env = res::Env::new();
    let eval_env = tree_walk::Env::new();
    let mut ctx = Context::new();
    loop {
        io::stdin()
            .read_line(&mut src)
            .expect("failed to read from stdin");
        let (ast, errors) = parse(&src);
        // println!("ast: {:?}", ast);
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
        match type_inference(&mut ctx, res.unwrap()) {
            Ok((root, new_ctx)) => {
                println!("root: {:?}", root);
                match eval(&src, eval_env.clone(), &root) {
                    Ok(val) => {
                        ctx = new_ctx;
                        println!("{}", val)
                    }
                    Err(errors) => println!("evaluation errors: {:?}", errors),
                }
            }

            Err(errors) => println!("inference errors: {:?}", errors),
        }
        src.clear();
        print!("\n> ");
        io::stdout().flush().expect("failed to flush stdout");
    }
}
