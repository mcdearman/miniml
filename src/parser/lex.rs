use super::{
    error::ParserError,
    token::{Token, TokenKind},
};
use chumsky::span::SimpleSpan;
use logos::Logos;

pub fn lex(src: &str) -> (Vec<(Token, SimpleSpan)>, Vec<ParserError>) {
    let mut errors = vec![];
    let mut tokens = vec![];
    for (res, span) in TokenKind::lexer(src).spanned() {
        match res {
            Ok(token) => tokens.push(Token {
                kind: token,
                span: span.into(),
            }),
            Err(err) => errors.push(ParserError {
                message: format!("{:?}: {:?}", span, err),
            }),
        }
    }
    (tokens, errors)
}
