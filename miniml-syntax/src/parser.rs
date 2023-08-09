use cstree::testing::GreenNodeBuilder;
use miniml_util::{intern::InternedString, span::Spanned};

use crate::{
    lex::{Token, TokenStream},
    syntax_kind::{Miniml, SyntaxKind},
};

#[derive(Debug)]
pub struct Parser {
    tokens: TokenStream,
    builder: GreenNodeBuilder<'static, 'static, Miniml>,
}

impl Parser {
    pub fn new<'src>(tokens: TokenStream) -> Self {
        Self {
            tokens,
            builder: GreenNodeBuilder::new(),
        }
    }

    fn bump(&mut self) -> Spanned<Token> {
        self.tokens.next()
    }

    pub fn parse(&mut self) -> Result<(), InternedString> {
        self.builder.start_node(SyntaxKind::Root);
        self.parse_expr()?;
        self.builder.finish_node();
        Ok(())
    }

    fn parse_expr(&mut self) -> Result<(), InternedString> {
        self.builder.start_node(SyntaxKind::Term);
        self.parse_term()?;
        while self.tokens.at(Token::Add) || self.tokens.at(Token::Sub) {
            self.builder.start_node(SyntaxKind::Term);
            self.bump();
            self.parse_term()?;
            self.builder.finish_node();
        }
        self.builder.finish_node();
        Ok(())
    }

    fn parse_term(&mut self) -> Result<(), InternedString> {
        self.builder.start_node(SyntaxKind::Factor);
        self.parse_factor()?;
        while self.tokens.at(Token::Mul) || self.tokens.at(Token::Div) {
            self.builder.start_node(SyntaxKind::Factor);
            self.bump();
            self.parse_factor()?;
            self.builder.finish_node();
        }
        self.builder.finish_node();
        Ok(())
    }

    fn parse_factor(&mut self) -> Result<(), InternedString> {
        match self.tokens.peek().value {
            Token::Int(n) => {
                self.builder.start_node(SyntaxKind::Factor);
                self.bump();
                self.builder.finish_node();
            }
            Token::LParen => {
                self.builder.start_node(SyntaxKind::Factor);
                self.bump();
                self.parse_expr()?;
                self.tokens.eat(Token::RParen).or(Err("expected `(`"))?;
                self.builder.finish_node();
            }
            _ => {
                return Err("expected int or paren".into());
            }
        }
        Ok(())
    }
}
