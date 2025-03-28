use logos::{Lexer, Logos};
use mmc_token::Token;
use mmc_utils::span::Span;

#[derive(Debug, Clone)]
pub struct TokenStream<'src> {
    logos: Lexer<'src, Token>,
}

impl<'src> TokenStream<'src> {
    #[inline]
    pub(crate) fn new(src: &'src str) -> Self {
        Self {
            logos: Token::lexer(src),
        }
    }
}

impl<'src> Iterator for TokenStream<'src> {
    type Item = (Token, Span);

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        self.logos.next().map(|res| match res {
            Ok(t) => (t, Span::from(self.logos.span())),
            Err(_) => (Token::Error, Span::from(self.logos.span())),
        })
    }
}
