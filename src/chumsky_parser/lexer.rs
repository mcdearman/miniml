use super::token::Token;
use logos::Logos;

#[derive(Debug)]
pub struct Lexer<'src> {
    logos: logos::Lexer<'src, Token>,
    peek: Option<Token>,
}

impl<'src> Lexer<'src> {
    pub fn new(input: &'src str) -> Self {
        Self {
            logos: Token::lexer(input),
            peek: None,
        }
    }

    pub fn peek(&mut self) -> Option<Token> {
        if self.peek.is_none() {
            self.peek = self.logos.next().map(|r| match r {
                Ok(t) => Some(t),
                Err(_) => None,
            });
        }
        self.peek
    }

    pub fn next(&mut self) -> Option<Token> {
        if self.peek.is_some() {
            self.peek.take()
        } else {
            self.logos.next()
        }
    }
}
