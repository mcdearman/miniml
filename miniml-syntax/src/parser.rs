use crate::{
    ast::{Decl, Expr, Root},
    error::{ParserError, SyntaxError},
    lex::{Token, TokenKind},
};
use logos::{Lexer, Logos};
use miniml_util::{
    intern::InternedString,
    span::{Spannable, Spanned},
};

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

    fn eat(&mut self, kind: TokenKind) {
        if self.peek().value == kind {
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
            decls.push(self.decl());
        }
        (Root { decls }, self.errors)
    }

    fn decl(&mut self) -> Spanned<Decl> {
        log::trace!("decl: {:?}", self.peek());
        match self.peek().value {
            TokenKind::Const => self.const_(),
            TokenKind::Let => self.let_(),
            TokenKind::Fn => self.fn_(),
            _ => {
                let next @ Spanned { value, span } = &self.tokens.next();
                log::trace!("unexpected token in decl: {:?}", next);
                Err(SyntaxError::UnexpectedToken(value.clone()).spanned(span.clone()))
            }
        }
    }

    fn const_(&mut self) -> Spanned<Decl> {
        log::trace!("const: {:?}", self.peek());
        let start = self.peek().span;
        self.eat(TokenKind::Const);
        let name = self.ident();
        self.eat(TokenKind::Eq);
        let expr = self.expr()?;
        Decl::Const {
            name,
            expr: Box::new(expr),
        }
        .spanned(start.extend(self.peek().span))
    }

    fn ident(&mut self) -> Spanned<InternedString> {
        let span = self.peek().span.clone();
        match self.peek().value {
            TokenKind::Ident => {
                let name = self.text();
                self.next();
                InternedString::from(name).spanned(span)
            }
            _ => {
                let tok @ Spanned { value, span } = self.next();
                self.errors
                    .push(SyntaxError::UnexpectedToken(tok).spanned(span));
                Expr::Error.spanned(span)
            }
        }
    }
}
