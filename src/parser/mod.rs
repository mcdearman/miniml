use self::{error::Error, token::TokenStream};

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

impl Parser {
    pub fn new<'src>(src: &'src str) -> Self {
        Self {
            tokens: TokenStream::new(src),
            errors: vec![],
        }
    }

    pub fn parse(&mut self) {}
}
