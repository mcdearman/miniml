use crate::{
    error::SyntaxError,
    lex::{Token, TokenStream},
    syntax_kind::{Miniml, SyntaxKind},
};
use cstree::{
    interning::Resolver,
    testing::{GreenNode, GreenNodeBuilder},
};
use miniml_util::{intern::InternedString, span::Spanned};

#[derive(Debug)]
pub struct Parser {
    tokens: TokenStream,
    builder: GreenNodeBuilder<'static, 'static, Miniml>,
    errors: Vec<Spanned<SyntaxError>>,
}

pub struct Parse<T> {
    green_node: GreenNode,
    resolver: T,
    errors: Vec<Spanned<SyntaxError>>,
}

impl Parser {
    pub fn new<'src>(tokens: TokenStream) -> Self {
        Self {
            tokens,
            builder: GreenNodeBuilder::new(),
            errors: vec![],
        }
    }

    fn bump(&mut self) -> Spanned<Token> {
        self.tokens.next()
    }

    pub fn parse(&mut self) -> Parse<impl Resolver> {
        self.builder.start_node(SyntaxKind::Root);
        self.expr();
        self.builder.finish_node();
        todo!()
    }

    fn expr(&mut self) {
        self.builder.start_node(SyntaxKind::Term);
        self.term();
        while self.tokens.at(Token::Add) || self.tokens.at(Token::Sub) {
            self.builder.start_node(SyntaxKind::Term);
            self.bump();
            self.term();
            self.builder.finish_node();
        }
        self.builder.finish_node();
    }

    fn term(&mut self) {
        self.builder.start_node(SyntaxKind::Factor);
        self.factor();
        while self.tokens.at(Token::Mul) || self.tokens.at(Token::Div) {
            self.builder.start_node(SyntaxKind::Factor);
            self.bump();
            self.factor();
            self.builder.finish_node();
        }
        self.builder.finish_node();
    }

    fn factor(&mut self) {
        match self.tokens.peek().value {
            Token::Int(n) => {
                self.builder.start_node(SyntaxKind::Factor);
                self.bump();
                self.builder.finish_node();
            }
            Token::LParen => {
                self.builder.start_node(SyntaxKind::Factor);
                self.bump();
                self.expr();
                self.tokens.eat(Token::RParen).or(Err("expected `(`"))?;
                self.builder.finish_node();
            }
            _ => {
                self.errors.push(Err("expected int or paren".into()));
            }
        }
    }
}
