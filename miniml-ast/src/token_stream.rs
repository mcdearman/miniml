use super::token::Token;
use logos::Logos;
use miniml_utils::span::Span;

#[derive(Debug, Clone)]
pub struct TokenStream<'src> {
    logos: logos::Lexer<'src, Token>,
}

impl<'src> TokenStream<'src> {
    pub fn new(src: &'src str) -> Self {
        Self {
            logos: Token::lexer(src),
        }
    }
}

impl<'src> Iterator for TokenStream<'src> {
    type Item = (Token, Span);

    fn next(&mut self) -> Option<Self::Item> {
        self.logos.next().map(|res| match res {
            Ok(t) => (t, Span::from(self.logos.span())),
            Err(_) => (Token::Error, Span::from(self.logos.span())),
        })
    }
}
