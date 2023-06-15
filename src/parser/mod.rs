use self::cst::Tree;

pub mod ast;
pub mod cst;
pub mod error;
pub mod event;
pub mod parse;
pub mod span;
mod tests;
pub mod token;

pub fn parse(src: &str) -> Tree {
    todo!()
}
