use crate::{analysis::infer::tir, lex::token_iter::TokenIter, parse::parse, runtime::eval::eval};
use analysis::infer::TypeSolver;
use rename::resolver::Resolver;
use runtime::default_env;
use rustyline::{error::ReadlineError, validate::Validator, DefaultEditor};
use std::io::{self, Read, Write};

mod analysis;
mod lex;
mod parse;
mod rename;
mod runtime;
mod utils;

struct TermValidator;

impl Validator for TermValidator {
    fn validate(
        &self,
        ctx: &mut rustyline::validate::ValidationContext,
    ) -> rustyline::Result<rustyline::validate::ValidationResult> {
        if ctx.input().trim().ends_with(";;") {
            Ok(rustyline::validate::ValidationResult::Valid(None))
        } else {
            Ok(rustyline::validate::ValidationResult::Incomplete)
        }
    }
}

fn main() {
    env_logger::init();
    let mut src = String::new();
    let mut res = Resolver::new();
    let builtins = res.builtins();
    let scoped_interner = res.env().dump_to_interner();
    let mut solver = TypeSolver::new(builtins.clone(), scoped_interner);
    let env = default_env(builtins.clone());

    let mut rl = DefaultEditor::new().expect("Failed to create editor");
    if rl.load_history("history.txt").is_err() {
        println!("No previous history.");
    }

    loop {
        let readline = rl.readline("> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str())
                    .expect("Failed to add history entry");
            }
            Err(ReadlineError::Interrupted) => {
                println!("CTRL-C");
                break;
            }
            Err(ReadlineError::Eof) => {
                println!("CTRL-D");
                break;
            }
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
    }
    rl.save_history("history.txt")
        .expect("Failed to save history");
    // loop {
    //     print!("> ");
    //     io::stdout().flush().unwrap();
    //     std::io::stdin().read_line(&mut src).unwrap();
    //     // std::io::stdin().read(buf)
    //     if src.trim() == "sub" {
    //         println!("{:#?}", solver.sub());
    //         src.clear();
    //         continue;
    //     }

    //     if src.trim().ends_with(";;") {
    //         src = src
    //             .trim()
    //             .strip_suffix(";;")
    //             .expect("Failed to strip suffix")
    //             .to_string();

    // }
}
