use super::{
    error::ParserError,
    token::{Token, TokenKind, TokenStream},
};
use logos::Logos;

pub fn lex(src: &str) -> (TokenStream, Vec<ParserError>) {
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
    (TokenStream::new(tokens), errors)
}
