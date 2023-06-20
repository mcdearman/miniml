use clap::Parser;
use compiler::Compiler;
use parser::ast::Item;
use repl::repl;

mod compiler;
mod intern;
mod list;
mod parser;
mod repl;
mod vm;

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
        let src = std::fs::read_to_string(filepath).expect("failed to read file");
        match &parse(&src) {
            (Some(item), _) => match Compiler::new().compile(item) {
                Ok(chunk) => {
                    println!("{:?}", chunk);
                }
                Err(err) => {
                    eprintln!("Compiler Error: {}", err);
                }
            },
            (None, errs) => {
                for err in errs {
                    eprintln!("Parser Error: {}", err);
                }
            }
        }
    } else {
        repl();
    }
}
