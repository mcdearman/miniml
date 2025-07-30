use itertools::Itertools;
use logos::Logos;
use mmc_rustbits::token::TokenKind;

fn main() {
    let src = "0o8";
    let lexer = TokenKind::lexer(src);
    let tokens = lexer.collect_vec();
    println!("{:#?}", tokens);
}
