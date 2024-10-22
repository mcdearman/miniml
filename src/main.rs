use dbg_pls::color;
use itertools::Itertools;
use lex::token_iter::TokenIter;
use parse::parse;
// use runtime::interpreter::Interpreter;
use rustyline::{
    error::ReadlineError, validate::Validator, Completer, Editor, Helper, Highlighter, Hinter,
};
use utils::intern::InternedString;

// mod analysis;
mod lex;
mod parse;
mod rename;
// mod runtime;
mod utils;

#[derive(Completer, Helper, Highlighter, Hinter)]
struct TermValidator;

impl Validator for TermValidator {
    fn validate(
        &self,
        ctx: &mut rustyline::validate::ValidationContext,
    ) -> rustyline::Result<rustyline::validate::ValidationResult> {
        if ctx.input().ends_with("\n") {
            Ok(rustyline::validate::ValidationResult::Valid(None))
        } else {
            Ok(rustyline::validate::ValidationResult::Incomplete)
        }
    }
}

fn main() {
    env_logger::init();
    let h = TermValidator;
    let mut rl = Editor::new().expect("Failed to create editor");
    rl.set_helper(Some(h));
    if rl.load_history(".miniml_history").is_err() {
        println!("No previous history.");
    }

    loop {
        let readline = rl.readline("> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str())
                    .expect("Failed to add history entry");
                let stream = TokenIter::new(&line);
                let ast = match parse(stream, true) {
                    (Some(ast), _) => {
                        log::debug!("AST: {:#?}", ast);
                        Some(ast)
                    }
                    (None, parse_errors) => {
                        log::error!("Parse errors: {:?}", parse_errors);
                        None
                    }
                };
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
    rl.save_history(".miniml_history")
        .expect("Failed to save history");
}
