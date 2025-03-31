use logos::{Lexer, Logos};
use mmc_token::{Token, TokenKind};
use mmc_utils::span::Span;

#[derive(Debug, Clone)]
pub struct TokenStream<'src> {
    logos: Lexer<'src, TokenKind>,
}

impl<'src> TokenStream<'src> {
    #[inline]
    pub(crate) fn new(src: &'src str) -> Self {
        Self {
            logos: TokenKind::lexer(src),
        }
    }

    #[inline]
    fn next(&mut self) -> Token {
        match self.logos.next().map(|res| match res {
            Ok(t) => (t, Span::from(self.logos.span())),
            Err(_) => (TokenKind::Error, Span::from(self.logos.span())),
        }) {
            Some((token, _)) => token,
            None => TokenKind::Eof,
        }
    }

    #[inline]
    fn peek(&mut self) -> Token {
        match self.logos.peekable().peek() {
            Some(Ok(t)) => *t,
            Some(Err(_)) => TokenKind::Error,
            None => TokenKind::Eof,
        }
    }
}
