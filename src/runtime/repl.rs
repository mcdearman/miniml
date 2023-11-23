use crate::{
    analysis::{
        infer::{self, type_inference},
        res::{self, resolve},
    },
    runtime::tree_walk::{self, default_ctx, default_res_env, eval, primitive_ids},
    syntax::{ast, parse::parse},
    util::node::Node,
};
use std::io::{self, Write};

pub fn repl() {
    print!("miniml> ");
    io::stdout().flush().expect("failed to flush stdout");
    let mut src = String::new();
    let mut full_src = String::new();
    let prims = primitive_ids();
    let res_env = default_res_env(prims.clone());
    let eval_env = tree_walk::default_env(prims.clone());
    let mut ctx = default_ctx(prims);
    let mut ast: Option<Node<ast::Root>> = None;
    let mut res: Option<Node<res::Root>> = None;
    let mut tast: Option<Node<infer::Root>> = None;
    // println!("repl ctx: {:?}", ctx);
    // println!("env: {:?}", eval_env.clone());
    loop {
        io::stdin()
            .read_line(&mut src)
            .expect("failed to read from stdin");
        full_src.push_str(&src);
        match src.trim() {
            "env" => {
                println!("{:#?}", eval_env.borrow());
                src.clear();
                print!("\n> ");
                io::stdout().flush().expect("failed to flush stdout");
                continue;
            }
            "ast" => {
                println!("{:?}", ast.clone().unwrap());
                src.clear();
                print!("\n> ");
                io::stdout().flush().expect("failed to flush stdout");
                continue;
            }
            "res" => {
                println!("{:?}", res.clone().unwrap());
                src.clear();
                print!("\n> ");
                io::stdout().flush().expect("failed to flush stdout");
                continue;
            }
            "tast" => {
                println!("{:?}", tast.clone().unwrap());
                src.clear();
                print!("\n> ");
                io::stdout().flush().expect("failed to flush stdout");
                continue;
            }
            "ctx" => {
                println!("{:?}", ctx);
                src.clear();
                print!("\n> ");
                io::stdout().flush().expect("failed to flush stdout");
                continue;
            }
            "exit" => break,
            _ => (),
        }
        let (p, errors) = parse(&src);
        ast = p;
        // println!("AST: {:?}", ast);
        if !errors.is_empty() {
            println!("parse errors: {:?}", errors);
            src.clear();
            print!("\n> ");
            io::stdout().flush().expect("failed to flush stdout");
            continue;
        }
        let (r, errors) = resolve(res_env.clone(), &ast.clone().unwrap());
        res = r;
        if !errors.is_empty() {
            println!("resolve errors: {:?}", errors);
            src.clear();
            print!("\n> ");
            io::stdout().flush().expect("failed to flush stdout");
            continue;
        }
        // println!("RES: {:?}", res);
        match type_inference(&*src, &mut ctx, res.clone().unwrap()) {
            Ok((root, new_ctx)) => {
                // println!("TAST: {:?}", root);
                tast = Some(root.clone());
                match eval(&src, &full_src, eval_env.clone(), &root) {
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
