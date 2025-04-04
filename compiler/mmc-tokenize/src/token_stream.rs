use logos::{Lexer, Logos};
use mmc_token::{Token, TokenKind};
use mmc_utils::span::Span;

#[derive(Debug, Clone)]
pub struct TokenStream<'src> {
    logos: Lexer<'src, TokenKind>,
    peek: Option<Token>,
}

impl<'src> TokenStream<'src> {
    #[inline(always)]
    pub(crate) fn new(src: &'src str) -> Self {
        Self {
            logos: TokenKind::lexer(src),
            peek: None,
        }
    }

    #[inline]
    pub fn next(&mut self) -> Token {
        match self.logos.next().map(|res| match res {
            Ok(t) => (t, Span::from(self.logos.span())),
            Err(_) => (TokenKind::Error, Span::from(self.logos.span())),
        }) {
            Some((token, s)) => Token::new(token, s),
            None => Token::new(TokenKind::Eof, Span::from(self.logos.span())),
        }
    }

    #[inline]
    pub fn peek(&mut self) -> Token {
        if let Some(token) = self.peek.clone() {
            token
        } else {
            let token = self.next();
            self.peek = Some(token.clone());
            token
        }
    }

    pub fn collect_tokens(&mut self) -> Vec<Token> {
        let mut tokens = Vec::new();
        loop {
            let token = self.peek();
            if *token.kind() == TokenKind::Eof {
                tokens.push(token);
                break;
            }
            tokens.push(token);
            self.peek = None;
        }
        tokens
    }
}
