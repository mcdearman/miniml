use crate::{
    ast::{Decl, Root},
    error::{ParserError, SyntaxError},
    lex::{Token, TokenKind},
};
use logos::{Lexer, Logos};
use miniml_util::span::{Spannable, Spanned};

#[derive(Debug)]
pub struct Parser<'src> {
    src: &'src str,
    lexer: Lexer<'src, TokenKind>,
    peek: Option<Token>,
    errors: Vec<ParserError>,
}

impl<'src> Parser<'src> {
    pub fn new(src: &'src str) -> Self {
        Self {
            src,
            lexer: TokenKind::lexer(src),
            peek: None,
            errors: vec![],
        }
    }

    fn fetch_token(&mut self) -> Token {
        match self.lexer.next().map(|res| (res, self.lexer.span())) {
            Some((res, s)) => match res {
                Ok(t) => t.spanned(s.into()),
                Err(_) => {
                    self.errors.push(SyntaxError::LexerError.spanned(s.into()));
                    self.fetch_token()
                }
            },
            None => TokenKind::Eof.spanned(self.lexer.span().into()),
        }
    }

    fn peek(&mut self) -> Token {
        if let Some(token) = self.peek.clone() {
            token
        } else {
            let token = self.fetch_token();
            self.peek = Some(token.clone());
            token
        }
    }

    fn next(&mut self) -> Token {
        if let Some(token) = self.peek.take() {
            token
        } else {
            self.fetch_token()
        }
    }

    fn eat(&mut self, token: TokenKind) {
        if self.peek().value == token {
            self.next();
        } else {
            let peek = self.peek();
            self.errors
                .push(SyntaxError::UnexpectedToken(peek.clone()).spanned(peek.span));
            self.next();
        }
    }

    fn text(&self) -> &'src str {
        &self.src[self.lexer.span()]
    }

    pub fn parse(mut self) -> (Root, Vec<ParserError>) {
        let mut decls = vec![];
        while self.peek().value != TokenKind::Eof {
            match self.de
        }
        (Root { decls }, self.errors)
    }

    fn decl(&mut self) {

    }
}
