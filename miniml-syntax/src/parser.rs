use crate::{
    error::{ParserError, SyntaxError},
    lex::{Token, TokenKind},
    syntax_kind::{Miniml, SyntaxKind},
};
use cstree::{
    interning::Resolver,
    testing::{GreenNode, GreenNodeBuilder, SyntaxNode},
};
use logos::{Lexer, Logos, SpannedIter};
use miniml_util::{
    intern::{self, InternedString},
    span::{Span, Spannable, Spanned},
};
use std::{fmt::Debug, iter::Peekable};

#[derive(Clone)]
pub struct Parse<T> {
    pub green_node: GreenNode,
    pub resolver: T,
    pub errors: Vec<ParserError>,
}

impl<T> Parse<T> {
    pub fn syntax(&self) -> SyntaxNode<SyntaxKind> {
        SyntaxNode::new_root(self.green_node.clone())
    }
}

#[derive(Debug)]
pub struct Parser<'src> {
    src: &'src str,
    lexer: Lexer<'src, TokenKind>,
    peek: Option<Token>,
    builder: GreenNodeBuilder<'static, 'static, Miniml>,
    errors: Vec<Spanned<SyntaxError>>,
}

impl<'src> Parser<'src> {
    pub fn new(src: &'src str) -> Self {
        Self {
            src,
            lexer: TokenKind::lexer(src),
            peek: None,
            builder: GreenNodeBuilder::new(),
            errors: vec![],
        }
    }

    fn fetch_token(&mut self) -> Token {
        match self.lexer.next().map(|res| (res, self.lexer.span())) {
            Some((res, s)) => match res {
                Ok(t) => {
                    if let TokenKind::Whitespace = t {
                        let text = self.text();
                        self.builder.token(SyntaxKind::Whitespace, text);
                        self.fetch_token()
                    } else {
                        let tok = t.spanned(s.into());
                        tok
                    }
                }
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

    pub fn parse(mut self) -> Parse<impl Resolver> {
        self.builder.start_node(SyntaxKind::Root);
        self.expr();
        self.builder.finish_node();
        let (tree, cache) = self.builder.finish();
        let interner = cache
            .expect("no cache")
            .into_interner()
            .expect("no interner");
        Parse {
            green_node: tree,
            resolver: interner,
            errors: self.errors.clone(),
        }
    }

    fn expr(&mut self) {
        self.builder.start_node(SyntaxKind::Expr);
        self.term();
        self.builder.finish_node();
    }

    fn term(&mut self) {
        let term = self.builder.checkpoint();
        self.factor();

        match self.peek().value {
            TokenKind::Plus => {
                self.builder.start_node_at(term, SyntaxKind::Term);
                self.builder.static_token(SyntaxKind::Plus);
                self.next();
                self.term();
                self.builder.finish_node();
            }
            TokenKind::Minus => {
                self.builder.start_node_at(term, SyntaxKind::Term);
                self.builder.static_token(SyntaxKind::Minus);
                self.next();
                self.term();
                self.builder.finish_node();
            }
            _ => {}
        }
    }

    fn factor(&mut self) {
        let factor = self.builder.checkpoint();
        self.power();

        match self.peek().value {
            TokenKind::Star => {
                self.builder.start_node_at(factor, SyntaxKind::Factor);
                self.builder.static_token(SyntaxKind::Star);
                self.next();
                self.factor();
                self.builder.finish_node();
            }
            TokenKind::Slash => {
                self.builder.start_node_at(factor, SyntaxKind::Factor);
                self.builder.static_token(SyntaxKind::Slash);
                self.next();
                self.factor();
                self.builder.finish_node();
            }
            _ => {}
        }
    }

    fn power(&mut self) {
        let power = self.builder.checkpoint();
        self.unary();

        match self.peek().value {
            TokenKind::Caret => {
                self.builder.start_node_at(power, SyntaxKind::Power);
                self.builder.static_token(SyntaxKind::Caret);
                self.next();
                self.power();
                self.builder.finish_node();
            }
            _ => {}
        }
    }

    fn unary(&mut self) {
        match self.peek().value {
            TokenKind::Minus => {
                self.builder.start_node(SyntaxKind::Unary);
                self.builder.static_token(SyntaxKind::Minus);
                self.next();
                self.unary();
                self.builder.finish_node();
            }
            _ => self.atom(),
        }
    }

    fn atom(&mut self) {
        self.builder.start_node(SyntaxKind::Atom);
        match self.peek().value {
            TokenKind::Int
            | TokenKind::Rational
            | TokenKind::Real
            | TokenKind::Complex
            | TokenKind::String
            | TokenKind::Char => self.lit(),
            TokenKind::Ident => self.ident(),
            TokenKind::LParen => {
                self.builder.static_token(SyntaxKind::LParen);
                self.eat(TokenKind::LParen);
                self.expr();
                self.builder.static_token(SyntaxKind::RParen);
                self.eat(TokenKind::RParen);
            }
            _ => {}
        }
        self.builder.finish_node();
    }

    fn ident(&mut self) {
        self.builder.start_node(SyntaxKind::Ident);
        let text = self.text();
        self.builder.token(SyntaxKind::Ident, text);
        self.next();
        self.builder.finish_node();
    }

    fn lit(&mut self) {
        self.builder.start_node(SyntaxKind::Lit);
        match self.peek().value {
            TokenKind::Int => {
                let text = self.text();
                self.builder.token(SyntaxKind::Int, text);
                self.next();
            }
            TokenKind::Rational => {
                let text = self.text();
                self.builder.token(SyntaxKind::Rational, text);
                self.next();
            }
            TokenKind::Real => {
                let text = self.text();
                self.builder.token(SyntaxKind::Real, text);
                self.next();
            }
            TokenKind::Complex => {
                let text = self.text();
                self.builder.token(SyntaxKind::Complex, text);
                self.next();
            }
            TokenKind::String => {
                let text = self.text();
                self.builder.token(SyntaxKind::String, text);
                self.next();
            }
            TokenKind::Char => {
                let text = self.text();
                self.builder.token(SyntaxKind::Char, text);
                self.next();
            }
            _ => {}
        }
        self.builder.finish_node();
    }
}

mod tests {
    use super::Parser;

    // #[test]
    // fn test_int() {
    //     let src = "42";
    //     let parse = Parser::new(src).parse();
    //     let node = parse.syntax();
    //     let res = &parse.resolver;
    //     insta::assert_snapshot!(node.debug(res, true));
    // }

    // #[test]
    // fn test_rational() {
    //     let src = "42/7";
    //     let parse = Parser::new(src).parse();
    //     let node = parse.syntax();
    //     let res = &parse.resolver;
    //     insta::assert_snapshot!(node.debug(res, true));
    // }
}
