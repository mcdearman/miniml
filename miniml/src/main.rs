// use clap::Parser;

// #[derive(Parser)]
// #[command(author, version, about, long_about = None)]
// struct Cli {
//     filepath: Option<String>,

//     #[arg(short, long, action = clap::ArgAction::Count)]
//     debug: u8,
// }

// fn main() {
//     env_logger::init();
//     let args = Cli::parse();

//     if let Some(filepath) = args.filepath {
//         // let src = std::fs::read_to_string(filepath).expect("failed to read file");
//         // match &parse(&src) {
//         //     (Some(item), _) => match Compiler::new().compile(item) {
//         //         Ok(chunk) => {
//         //             println!("{:?}", chunk);
//         //         }
//         //         Err(err) => {
//         //             eprintln!("Compiler Error: {}", err);
//         //         }
//         //     },
//         //     (None, errs) => {
//         //         for err in errs {
//         //             eprintln!("Parser Error: {}", err);
//         //         }
//         //     }
//         // }
//     } else {
//         repl();
//     }
// }

use logos::Logos;
use miniml_syntax::lex::TokenKind;
use miniml_syntax::parser::Parser;

fn main() {
    env_logger::init();
    // let src = "fn gcd a b = if b = 0 then a else gcd b (a % b)\nfn main = println gcd 85 51; ()";
    let src = "2 + 1/2";
    let tokens = TokenKind::lexer(src).spanned().collect::<Vec<_>>();
    println!("tokens: {:?}", tokens);
    // let src = "fn main = foo 1 |> bar 2";
    let parser = Parser::new(src);
    let parse = parser.parse();
    log::trace!("parse errors: {:?}", parse.errors);
    let node = parse.syntax();
    let res = &parse.resolver;
    println!("{}", node.debug(res, true));

    // let mut compiler = Compiler::new();
    // let fun = compiler.compile(&ast.0).expect("failed to compile");
    // let frame = CallFrame::new(Box::new(fun));
    // // log::trace!("chunk: {:?}", chunk);
    // // log::trace!("disasm: {}", chunk);
    // let mut vm = VM::new(frame);
    // let res = vm.run().expect("runtime error");
    // println!("{}", res);
}
