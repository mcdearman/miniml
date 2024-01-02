use crate::{node::Node, token::Token};
use logos::Logos;
use std::fmt::Debug;

pub struct Lexer<'src> {
    logos: logos::SpannedIter<'src, Token>,
    peek: Option<Node<Token>>,
}

impl<'src> Lexer<'src> {
    pub fn new(input: &'src str) -> Self {
        Self {
            logos: Token::lexer(input).spanned(),
            peek: None,
        }
    }

    pub fn peek(&mut self) -> Option<Node<Token>> {
        if self.peek.is_none() {
            self.peek = self.next();
        }
        self.peek.clone()
    }
}

impl Debug for Lexer<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Lexer").field("peek", &self.peek).finish()
    }
}

impl Iterator for Lexer<'_> {
    type Item = Node<Token>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.peek.is_some() {
            self.peek.take()
        } else {
            self.logos.next().map(|(r, s)| match r {
                Ok(t) => Node::new(t, s.into()),
                Err(_) => Node::new(Token::Error, s.into()),
            })
        }
    }
}
