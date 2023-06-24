use super::{error::Error, token::Token};
use chumsky::span::SimpleSpan;
use logos::Logos;

pub fn lex(src: &str) -> (Vec<(Token, SimpleSpan)>, Vec<Error>) {
    let mut errors = vec![];
    let mut tokens = vec![];
    for (res, span) in Token::lexer(src).spanned() {
        match res {
            Ok(token) => tokens.push((token, SimpleSpan::from(span))),
            Err(err) => errors.push(Error {
                message: format!("{:?}: {:?}", span, err),
            }),
        }
    }
    (tokens, errors)
}