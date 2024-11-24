use miniml_rename::resolver::Resolver;
use miniml_syntax::{parse::parse, token_iter::TokenIter};
use rustyline::{
    error::ReadlineError, validate::Validator, Completer, Editor, Helper, Highlighter, Hinter,
};

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

    println!("Welcome to MiniML!");

    let mut res = Resolver::new();
    // let mut solver = TypeSolver::new();

    loop {
        let readline = rl.readline("> ");
        match readline {
            Ok(line) => {
                match line.trim() {
                    ":q" | ":quit" => break,
                    // "ctx" => {
                    //     log::debug!("Context: {:#?}", solver.ctx);
                    //     continue;
                    // }
                    // "mctx" => {
                    //     log::debug!("Meta context: {:#?}", solver.meta_ctx);
                    //     continue;
                    // }
                    "clear" => {
                        rl.clear_history();
                        continue;
                    }
                    _ => (),
                }
                rl.add_history_entry(line.as_str())
                    .expect("Failed to add history entry");
                let stream = TokenIter::new(&line);

                match parse(stream, true) {
                    (Some(ast), _) => {
                        log::debug!("AST: {:#?}", ast);
                        match res.resolve(&ast) {
                            (Some(nir), _) => {
                                log::debug!("NIR: {:#?}", nir);

                                // let (tir, type_errors) = solver.infer(&line, &nir);
                                // if !type_errors.is_empty() {
                                //     log::error!("Type errors: {:#?}", type_errors);
                                //     continue;
                                // }
                                // log::debug!("TIR: {:#?}", tir);
                            }
                            (None, res_errors) => {
                                log::error!("Resolution errors: {:#?}", res_errors);
                                continue;
                            }
                        }
                    }
                    (None, parse_errors) => {
                        log::error!("Parse errors: {:#?}", parse_errors);
                        continue;
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
