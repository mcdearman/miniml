use clap::Parser;
use miniml_eval::env::Env;

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Cli {
    filepath: Option<String>,

    #[arg(short, long, action = clap::ArgAction::Count)]
    debug: u8,
}

fn main() {
    env_logger::init();
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

fn ack_memo(m: i64, n: i64) -> i64 {
    let mut memo = vec![vec![0; n as usize + 1]; m as usize + 1];
    fn ack(m: i64, n: i64, memo: &mut Vec<Vec<i64>>) -> i64 {
        if m == 0 {
            return n + 1;
        }
        if n == 0 {
            return ack(m - 1, 1, memo);
        }
        if memo[m as usize][n as usize] != 0 {
            return memo[m as usize][n as usize];
        }
        let res = ack(m - 1, ack(m, n - 1, memo), memo);
        memo[m as usize][n as usize] = res;
        res
    }
    ack(m, n, &mut memo)
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
