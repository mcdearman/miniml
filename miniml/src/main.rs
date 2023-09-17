use std::f32::consts::E;

use chumsky::{input::Stream, prelude::Input, span::SimpleSpan, Parser};
use logos::Logos;
use miniml_syntax::{parse::parser, token::Token};
// use miniml_eval::env::Env;
// use miniml_repl::tree_walk::repl;

// #[derive(Parser)]
// #[command(author, version, about, long_about = None)]
// struct Cli {
//     filepath: Option<String>,

//     #[arg(short, long, action = clap::ArgAction::Count)]
//     debug: u8,
// }

fn main() {
    env_logger::init();
    let src = "let x = x + 1";
    let tokens = Token::lexer(&src).spanned().map(|(tok, span)| match tok {
        Ok(tok) => (tok, SimpleSpan::from(span)),
        Err(err) => panic!("lex error: {:?}", err),
    });
    let tok_stream = Stream::from_iter(tokens).spanned(SimpleSpan::from(src.len()..src.len()));
    match parser().parse(tok_stream).into_result() {
        Ok(root) => {
            println!("{:?}", root);
        }
        Err(errors) => println!("error: {:?}", errors),
    }

    // let args = Cli::parse();
    // let env = Env::new();

    // if let Some(filepath) = args.filepath {
    //     let src = std::fs::read_to_string(filepath).expect("failed to read file");
    //     let mut parser = miniml_syntax::parser::Parser::new(&src);
    //     match parser.repl_parse() {
    //         (root, errors) => {
    //             if !errors.is_empty() {
    //                 println!("errors: {:?}", errors);
    //             } else {
    //                 println!("root: {:?}", root);
    //             }
    //         }
    //     }
    // } else {
    //     repl();
    // }
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
