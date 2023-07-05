use self::error::Error;

pub mod ast;
pub mod cst;
pub mod error;
pub mod span;
mod tests;
pub mod token;

#[derive(Debug)]
pub struct Parser {
    tokens: TokenStream,
    errors: Vec<Error>,
}

