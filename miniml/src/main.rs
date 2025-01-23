use miniml_analysis::scc::SCC;
use miniml_ast::token_stream::TokenStream;
use miniml_infer::solver::TypeSolver;
use miniml_parse::parse;
use miniml_rename::resolver::Resolver;
use pretty::{
    termcolor::{Color, ColorChoice, ColorSpec, StandardStream},
    Arena, DocAllocator,
};
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

    let arena = Arena::new();
    let red = arena
        .text("Welcome to MiniML!")
        .annotate(ColorSpec::new().set_fg(Some(Color::Red)).clone());
    red.render_colored(80, StandardStream::stdout(ColorChoice::Auto))
        .expect("Failed to render colored text");

    let h = TermValidator;
    let mut rl = Editor::new().expect("Failed to create editor");
    rl.set_helper(Some(h));
    if rl.load_history(".miniml_history").is_err() {
        eprintln!("No previous history.");
    }

    // println!("Welcome to MiniML!");

    let mut res = Resolver::new();
    let mut solver = TypeSolver::new();

    loop {
        let readline = rl.readline("> ");
        match readline {
            Ok(line) => {
                match line.trim() {
                    ":q" | ":quit" => break,
                    "ctx" => {
                        log::debug!("Context: {:#?}", solver.ctx);
                        continue;
                    }
                    "clear" => {
                        rl.clear_history().expect("history failed to clear");
                        continue;
                    }
                    _ => (),
                }
                rl.add_history_entry(line.as_str())
                    .expect("Failed to add history entry");
                let stream = TokenStream::new(&line);

                match parse(stream, true) {
                    (Some(ast), _) => {
                        // log::debug!("AST: {:#?}", ast);
                        match res.resolve(&ast) {
                            (Some(nir), errors) => {
                                if !errors.is_empty() {
                                    // log::error!("Resolution errors: {:#?}", errors);
                                    eprint!("Resolution errors: {:#?}", errors);
                                    res.clear_errors();
                                    continue;
                                }
                                // log::debug!("NIR: {:#?}", nir);
                                // println!("NIR: {:#?}", nir);
                                let mut scc = SCC::new();
                                let sir = scc.run(&nir);
                                // log::debug!("SCC: {:#?}", sir);
                                // println!("SCC: {:#?}", sir);
                                let (tir, errors) = solver.infer(&*line, &sir);
                                if !errors.is_empty() {
                                    // log::error!("Inference errors: {:#?}", errors);
                                    eprint!("Inference errors: {:#?}", errors);
                                    continue;
                                }
                                println!("TIR: {:#?}", tir);
                            }
                            (None, res_errors) => {
                                // log::error!("Resolution errors: {:#?}", res_errors);
                                eprint!("Resolution errors: {:#?}", res_errors);
                                continue;
                            }
                        }
                    }
                    (None, parse_errors) => {
                        // log::error!("Parse errors: {:#?}", parse_errors);
                        eprint!("Parse errors: {:#?}", parse_errors);
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
