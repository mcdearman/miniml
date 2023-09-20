use std::{path::Path, sync::mpsc::channel};

use clap::Parser;
use miniml_repl::tree_walk::repl;
use miniml_syntax::parse;
use notify::{event, Event, Watcher};

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Cli {
    filepath: Option<String>,

    #[arg(short, long, action = clap::ArgAction::Count)]
    debug: u8,
}

fn main() {
    env_logger::init();
    let args = Cli::parse();

    if let Some(filepath) = args.filepath {
        parse_file(&*filepath);
        let (tx, rx) = channel::<Event>();
        let fp = filepath.clone();
        let mut watcher = notify::recommended_watcher(
            move |res: Result<Event, notify::Error>| match res {
                Ok(event) => match event.kind {
                    event::EventKind::Modify(_) => {
                        tx.send(event).unwrap();
                    }
                    _ => println!("other: {:?}", event.kind),
                },
                Err(e) => println!("watch error: {:?}", e),
            },
        )
        .unwrap();
        watcher
            .watch(Path::new(&*fp.clone()), notify::RecursiveMode::NonRecursive)
            .unwrap();
        loop {
            match rx.recv() {
                Ok(_) => parse_file(&*fp),
                Err(e) => println!("watch error: {:?}", e),
            }
        }
    } else {
        repl();
    }
}

fn parse_file(filepath: &str) {
    let src = std::fs::read_to_string(filepath.clone()).expect("failed to read file");
    match parse(&*src, false) {
        (Some(root), errors) => {
            if !errors.is_empty() {
                println!("errors: {:?}", errors);
            } else {
                println!("root: {:?}", root);
            }
        }
        (None, errors) => {
            println!("errors: {:?}", errors);
        }
    }
}

// use logos::Logos;
// use miniml_syntax::{
//     ast::Format,
//     lex::{Token, TokenKind},
//     parser::Parser,
// };
// use miniml_util::{intern::InternedString, span::Spannable};

// fn main() {
//     env_logger::init();
//     // let src = "fn gcd a b = if b = 0 then a else gcd b (a % b)\nfn main = println gcd 85 51; ()";
//     // let src = "fn main = -x + 2 + 2^-1/-2 * (4.5 - 2); ()";

//     // let src = "fn main = let x = 1 in x; ()";
//     // // let src = "fn add x y = x + y\nfn main = add 1 2; ()";
//     // let tokens = TokenKind::lexer(src).spanned().collect::<Vec<_>>();
//     // println!("tokens: {:?}", tokens);
//     // // let src = "fn main = foo 1 |> bar 2";
//     // let mut parser = Parser::new(src);
//     // let (root, errors) = parser.parse();
//     // // println!("root: {:#?}", root);
//     // let ast = Format {
//     //     indent: 0,
//     //     value: root.clone(),
//     // };

//     // let mut compiler = Compiler::new();
//     // let fun = compiler.compile(&ast.0).expect("failed to compile");
//     // let frame = CallFrame::new(Box::new(fun));
//     // // log::trace!("chunk: {:?}", chunk);
//     // // log::trace!("disasm: {}", chunk);
//     // let mut vm = VM::new(frame);
//     // let res = vm.run().expect("runtime error");
//     // println!("{}", res);
// }
