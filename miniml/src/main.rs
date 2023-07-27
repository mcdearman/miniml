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

use miniml_compile::compiler::Compiler;
use miniml_syntax::{lex::lex, parser::Parser};
use miniml_vm::vm::VM;

fn main() {
    env_logger::init();
    let src = "let x = 1";
    let tokens = lex(src).expect("failed to lex");
    log::trace!("tokens: {}", tokens);
    let mut parser = Parser::new(tokens);
    let ast = parser.parse().expect("failed to parse");
    log::trace!("ast: {:?}", ast);
    let mut compiler = Compiler::new();
    let chunk = compiler.compile(&ast.0).expect("failed to compile");
    log::trace!("chunk: {:?}", chunk);
    log::trace!("disasm: {}", chunk);
    let mut vm = VM::new(chunk);
    let res = vm.run().expect("runtime error");
    println!("{}", res);
}
