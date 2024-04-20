use runtime::interpreter::Interpreter;
use rustyline::{
    error::ReadlineError, validate::Validator, Completer, Editor, Helper, Highlighter, Hinter,
};

mod analysis;
mod lex;
mod parse;
mod rename;
mod runtime;
mod utils;

#[derive(Completer, Helper, Highlighter, Hinter)]
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
    let h = TermValidator;
    let mut rl = Editor::new().expect("Failed to create editor");
    rl.set_helper(Some(h));
    if rl.load_history("history.txt").is_err() {
        println!("No previous history.");
    }
    let mut interpreter = Interpreter::new();

    loop {
        let readline = rl.readline("> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str())
                    .expect("Failed to add history entry");
                match interpreter.run(line.trim().strip_suffix(";;").unwrap_or(&*line)) {
                    Ok(val) => println!("{}\n", val),
                    Err(err) => println!("{}", err),
                }
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
}
