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

use miniml_compile::compiler::Compiler;
use miniml_syntax::{lex::lex, parser::Parser};
use miniml_vm::vm::VM;

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
fn main() {
    let src = "-523";
    let tokens = lex(src).expect("failed to lex");
    let mut parser = Parser::new(tokens);
    let ast = parser.parse().expect("failed to parse");
    println!("ast: {:?}", ast);
    let mut compiler = Compiler::new();
    let chunk = compiler.compile(&ast.0).expect("failed to compile");
    let mut vm = VM::new(chunk);
    let res = vm.run().expect("runtime error");
    println!("val: {:?}", res);
}
