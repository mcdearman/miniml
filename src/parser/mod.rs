use self::{ast::Item, error::Error};
use itertools::Itertools;

pub mod ast;
pub mod cst;
pub mod error;
pub mod lex;
pub mod span;
mod tests;
pub mod token;

pub fn parse(src: &str) -> (Option<Item>, Vec<Error>) {
    let mut errors = vec![];

    let (tokens, lex_errs) = lex::lex(src);
    errors.append(
        &mut lex_errs
            .into_iter()
            .map(|e| Error {
                message: format!("{}", e),
            })
            .collect_vec(),
    );

    todo!()
}
