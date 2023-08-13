// use crate::error::{ParserError, SyntaxError};
// use crate::{error::ParserError, lsp::Parser};
// use cstree::Syntax;
// use cstree::{
//     interning::Resolver,
//     testing::{GreenNode, GreenNodeBuilder, SyntaxNode},
// };
// use logos::{Lexer, Logos, SpannedIter};
// use miniml_util::{
//     intern::{self, InternedString},
//     span::{Span, Spannable, Spanned},
// };
// use std::{fmt::Debug, iter::Peekable};

// use super::error::SyntaxError;
// use itertools::join;
// use logos::Logos;
// use miniml_util::{
//     intern::InternedString,
//     span::{Span, Spannable, Spanned},
// };
// use num_complex::Complex;
// use num_rational::Rational64;
// use std::{
//     fmt::{Debug, Display},
//     iter::Peekable,
//     vec::IntoIter,
// };

// #[derive(Logos, Debug, Clone, PartialEq)]
// pub enum TokenKind {
//     Eof,
//     #[regex("[ \n\t\r]+")]
//     Whitespace,
//     #[regex(r#"--[^\n]*|/\*([^*]|\**[^*/])*\*+/"#)]
//     Comment,
//     #[regex(r##"([A-Za-z]|_)([A-Za-z]|_|\d)*"##)]
//     Ident,
//     #[regex(
//         r#"-?((0b[0-1]+)|(0o[0-7]+)|(0x[0-9a-fA-F]+)|([1-9]\d*|0))"#,
//         priority = 2
//     )]
//     Int,
//     #[regex(
//         r#"-?((0b[0-1]+)|(0o[0-7]+)|(0x[0-9a-fA-F]+)|([1-9]\d*|0))/-?((0b[0-1]+)|(0o[0-7]+)|(0x[0-9a-fA-F]+)|([1-9]\d*|0))"#,
//         priority = 1
//     )]
//     Rational,
//     #[regex(r#"((\d+(\.\d+))|(\.\d+))([Ee](\+|-)?\d+)?"#, priority = 1)]
//     Real,
//     #[regex(r#"((\d+(\.\d+)?)|(\.\d+))([Ee](\+|-)?\d+)?i"#, priority = 0)]
//     Complex,
//     #[regex(r#"'\w'"#)]
//     Char,
//     #[regex(r#""((\\"|\\\\)|[^\\"])*""#)]
//     String,
//     #[token("+")]
//     Plus,
//     #[token("-")]
//     Minus,
//     #[token("*")]
//     Star,
//     #[token("/")]
//     Slash,
//     #[token("%")]
//     Percent,
//     #[token("^")]
//     Caret,

//     #[token("\\")]
//     Backslash,
//     #[token("->")]
//     Arrow,
//     #[token("=>")]
//     FatArrow,
//     #[token("|")]
//     Pipe,
//     #[token("|>")]
//     PipeArrow,

//     #[token("=")]
//     Eq,
//     #[token("<")]
//     Lt,
//     #[token(">")]
//     Gt,
//     #[token("!=")]
//     Neq,
//     #[token("<=")]
//     Leq,
//     #[token(">=")]
//     Geq,

//     #[token("(")]
//     LParen,
//     #[token(")")]
//     RParen,
//     #[token("[")]
//     LBrack,
//     #[token("]")]
//     RBrack,
//     #[token("{")]
//     LBrace,
//     #[token("}")]
//     RBrace,
//     #[token(",")]
//     Comma,
//     #[token(".")]
//     Period,
//     #[token(";")]
//     Semicolon,
//     #[token(":")]
//     Colon,

//     #[token("pub")]
//     Pub,
//     #[token("mod")]
//     Module,
//     #[token("end")]
//     End,
//     #[token("use")]
//     Use,
//     #[token("const")]
//     Const,
//     #[token("let")]
//     Let,
//     #[token("fn")]
//     Fn,
//     #[token("struct")]
//     Struct,
//     #[token("match")]
//     Match,
//     #[token("with")]
//     With,
//     #[token("and")]
//     And,
//     #[token("or")]
//     Or,
//     #[token("not")]
//     Not,
//     #[token("if")]
//     If,
//     #[token("then")]
//     Then,
//     #[token("elif")]
//     Elif,
//     #[token("else")]
//     Else,
//     #[token("in")]
//     In,
// }

// impl Display for TokenKind {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         write!(
//             f,
//             "{}",
//             match self {
//                 TokenKind::Eof => "<EOF>",
//                 TokenKind::Whitespace => "<WS>",
//                 TokenKind::Comment => "Comment",
//                 TokenKind::Ident => "Ident",
//                 TokenKind::Int => "Int",
//                 TokenKind::Rational => "Rational",
//                 TokenKind::Real => "Real",
//                 TokenKind::Complex => "Complex",
//                 TokenKind::Char => "Char",
//                 TokenKind::String => "String",
//                 TokenKind::Plus => "+",
//                 TokenKind::Minus => "-",
//                 TokenKind::Star => "*",
//                 TokenKind::Slash => "/",
//                 TokenKind::Percent => "%",
//                 TokenKind::Caret => "^",
//                 TokenKind::Backslash => "\\",
//                 TokenKind::Arrow => "->",
//                 TokenKind::FatArrow => "=>",
//                 TokenKind::Pipe => "|",
//                 TokenKind::PipeArrow => "|>",
//                 TokenKind::Eq => "=",
//                 TokenKind::Lt => "<",
//                 TokenKind::Gt => ">",
//                 TokenKind::Neq => "!=",
//                 TokenKind::Leq => "<=",
//                 TokenKind::Geq => ">=",
//                 TokenKind::LParen => "(",
//                 TokenKind::RParen => ")",
//                 TokenKind::LBrack => "[",
//                 TokenKind::RBrack => "]",
//                 TokenKind::LBrace => "{",
//                 TokenKind::RBrace => "}",
//                 TokenKind::Comma => ",",
//                 TokenKind::Period => ".",
//                 TokenKind::Semicolon => ";",
//                 TokenKind::Colon => ":",
//                 TokenKind::Pub => "pub",
//                 TokenKind::Module => "mod",
//                 TokenKind::End => "end",
//                 TokenKind::Use => "use",
//                 TokenKind::Const => "const",
//                 TokenKind::Let => "let",
//                 TokenKind::Fn => "fn",
//                 TokenKind::Struct => "struct",
//                 TokenKind::Match => "match",
//                 TokenKind::With => "with",
//                 TokenKind::And => "and",
//                 TokenKind::Or => "or",
//                 TokenKind::Not => "not",
//                 TokenKind::If => "if",
//                 TokenKind::Then => "then",
//                 TokenKind::Elif => "elif",
//                 TokenKind::Else => "else",
//                 TokenKind::In => "in",
//             }
//         )
//     }
// }

// pub type Token = Spanned<TokenKind>;

// #[repr(u32)]
// #[derive(Syntax, Debug, Clone, Copy, PartialEq, Eq)]
// pub enum SyntaxKind {
//     // Tokens / Terminals
//     Error,
//     Comment,
//     Whitespace,
//     Ident,
//     // Keywords
//     #[static_text("pub")]
//     Pub,
//     #[static_text("mod")]
//     Module,
//     #[static_text("end")]
//     End,
//     #[static_text("use")]
//     Use,
//     #[static_text("const")]
//     Const,
//     #[static_text("let")]
//     Let,
//     #[static_text("fn")]
//     Fn,
//     #[static_text("struct")]
//     Struct,
//     #[static_text("match")]
//     Match,
//     #[static_text("with")]
//     With,
//     #[static_text("and")]
//     And,
//     #[static_text("or")]
//     Or,
//     #[static_text("not")]
//     Not,
//     #[static_text("if")]
//     If,
//     #[static_text("elif")]
//     Elif,
//     #[static_text("then")]
//     Then,
//     #[static_text("else")]
//     Else,
//     #[static_text("in")]
//     In,
//     // Literals
//     Char,
//     String,
//     Complex,
//     Real,
//     Rational,
//     Int,
//     // Punctuation
//     #[static_text("+")]
//     Plus,
//     #[static_text("-")]
//     Minus,
//     #[static_text("*")]
//     Star,
//     #[static_text("/")]
//     Slash,
//     #[static_text("%")]
//     Percent,
//     #[static_text("^")]
//     Caret,
//     #[static_text("\\")]
//     Backslash,
//     #[static_text("->")]
//     Arrow,
//     #[static_text("=>")]
//     FatArrow,
//     #[static_text("|")]
//     Pipe,
//     #[static_text("|>")]
//     PipeArrow,
//     #[static_text("=")]
//     Eq,
//     #[static_text("<")]
//     Lt,
//     #[static_text(">")]
//     Gt,
//     #[static_text("!=")]
//     Neq,
//     #[static_text("<=")]
//     Leq,
//     #[static_text(">=")]
//     Geq,
//     #[static_text("(")]
//     LParen,
//     #[static_text(")")]
//     RParen,
//     // Nodes / Non-terminals
//     Lit,
//     IfExpr,
//     Lambda,
//     LetExpr,
//     Atom,
//     Apply,
//     Unary,
//     Power,
//     Factor,
//     Term,
//     Expr,
//     Root,
// }

// pub type Miniml = SyntaxKind;

// #[derive(Clone)]
// pub struct Parse<T> {
//     pub green_node: GreenNode,
//     pub resolver: T,
//     pub errors: Vec<ParserError>,
// }

// impl<T> Parse<T> {
//     pub fn syntax(&self) -> SyntaxNode<SyntaxKind> {
//         SyntaxNode::new_root(self.green_node.clone())
//     }
// }

// #[derive(Debug)]
// pub struct Parser<'src> {
//     src: &'src str,
//     lexer: Lexer<'src, TokenKind>,
//     peek: Option<Token>,
//     builder: GreenNodeBuilder<'static, 'static, Miniml>,
//     errors: Vec<ParserError>,
// }

// impl<'src> Parser<'src> {
//     pub fn new(src: &'src str) -> Self {
//         Self {
//             src,
//             lexer: TokenKind::lexer(src),
//             peek: None,
//             builder: GreenNodeBuilder::new(),
//             errors: vec![],
//         }
//     }

//     fn fetch_token(&mut self) -> Token {
//         match self.lexer.next().map(|res| (res, self.lexer.span())) {
//             Some((res, s)) => match res {
//                 Ok(t) => {
//                     if let TokenKind::Whitespace = t {
//                         let text = self.text();
//                         self.builder.token(SyntaxKind::Whitespace, text);
//                         self.fetch_token()
//                     } else {
//                         let tok = t.spanned(s.into());
//                         tok
//                     }
//                 }
//                 Err(_) => {
//                     self.errors.push(SyntaxError::LexerError.spanned(s.into()));
//                     self.fetch_token()
//                 }
//             },
//             None => TokenKind::Eof.spanned(self.lexer.span().into()),
//         }
//     }

//     fn peek(&mut self) -> Token {
//         if let Some(token) = self.peek.clone() {
//             token
//         } else {
//             let token = self.fetch_token();
//             self.peek = Some(token.clone());
//             token
//         }
//     }

//     fn next(&mut self) -> Token {
//         if let Some(token) = self.peek.take() {
//             token
//         } else {
//             self.fetch_token()
//         }
//     }

//     fn eat(&mut self, token: TokenKind) {
//         if self.peek().value == token {
//             self.next();
//         } else {
//             let peek = self.peek();
//             self.errors
//                 .push(SyntaxError::UnexpectedToken(peek.clone()).spanned(peek.span));
//             self.next();
//         }
//     }

//     fn text(&self) -> &'src str {
//         &self.src[self.lexer.span()]
//     }

//     pub fn parse(mut self) -> Parse<impl Resolver> {
//         self.builder.start_node(SyntaxKind::Root);
//         self.expr();
//         self.builder.finish_node();
//         let (tree, cache) = self.builder.finish();
//         let interner = cache
//             .expect("no cache")
//             .into_interner()
//             .expect("no interner");
//         Parse {
//             green_node: tree,
//             resolver: interner,
//             errors: self.errors.clone(),
//         }
//     }

//     fn expr(&mut self) {
//         self.builder.start_node(SyntaxKind::Expr);
//         self.term();
//         self.builder.finish_node();
//     }

//     fn term(&mut self) {
//         let term = self.builder.checkpoint();
//         self.factor();

//         match self.peek().value {
//             TokenKind::Plus => {
//                 self.builder.start_node_at(term, SyntaxKind::Term);
//                 self.builder.static_token(SyntaxKind::Plus);
//                 self.next();
//                 self.term();
//                 self.builder.finish_node();
//             }
//             TokenKind::Minus => {
//                 self.builder.start_node_at(term, SyntaxKind::Term);
//                 self.builder.static_token(SyntaxKind::Minus);
//                 self.next();
//                 self.term();
//                 self.builder.finish_node();
//             }
//             _ => {}
//         }
//     }

//     fn factor(&mut self) {
//         let factor = self.builder.checkpoint();
//         self.power();

//         match self.peek().value {
//             TokenKind::Star => {
//                 self.builder.start_node_at(factor, SyntaxKind::Factor);
//                 self.builder.static_token(SyntaxKind::Star);
//                 self.next();
//                 self.factor();
//                 self.builder.finish_node();
//             }
//             TokenKind::Slash => {
//                 self.builder.start_node_at(factor, SyntaxKind::Factor);
//                 self.builder.static_token(SyntaxKind::Slash);
//                 self.next();
//                 self.factor();
//                 self.builder.finish_node();
//             }
//             _ => {}
//         }
//     }

//     fn power(&mut self) {
//         let power = self.builder.checkpoint();
//         self.unary();

//         match self.peek().value {
//             TokenKind::Caret => {
//                 self.builder.start_node_at(power, SyntaxKind::Power);
//                 self.builder.static_token(SyntaxKind::Caret);
//                 self.next();
//                 self.power();
//                 self.builder.finish_node();
//             }
//             _ => {}
//         }
//     }

//     fn unary(&mut self) {
//         match self.peek().value {
//             TokenKind::Minus => {
//                 self.builder.start_node(SyntaxKind::Unary);
//                 self.builder.static_token(SyntaxKind::Minus);
//                 self.next();
//                 self.unary();
//                 self.builder.finish_node();
//             }
//             _ => self.atom(),
//         }
//     }

//     fn atom(&mut self) {
//         self.builder.start_node(SyntaxKind::Atom);
//         match self.peek().value {
//             TokenKind::Int
//             | TokenKind::Rational
//             | TokenKind::Real
//             | TokenKind::Complex
//             | TokenKind::String
//             | TokenKind::Char => self.lit(),
//             TokenKind::Ident => self.ident(),
//             TokenKind::LParen => {
//                 self.builder.static_token(SyntaxKind::LParen);
//                 self.eat(TokenKind::LParen);
//                 self.expr();
//                 self.builder.static_token(SyntaxKind::RParen);
//                 self.eat(TokenKind::RParen);
//             }
//             _ => {}
//         }
//         self.builder.finish_node();
//     }

//     fn ident(&mut self) {
//         self.builder.start_node(SyntaxKind::Ident);
//         let text = self.text();
//         self.builder.token(SyntaxKind::Ident, text);
//         self.next();
//         self.builder.finish_node();
//     }

//     fn lit(&mut self) {
//         self.builder.start_node(SyntaxKind::Lit);
//         match self.peek().value {
//             TokenKind::Int => {
//                 let text = self.text();
//                 self.builder.token(SyntaxKind::Int, text);
//                 self.next();
//             }
//             TokenKind::Rational => {
//                 let text = self.text();
//                 self.builder.token(SyntaxKind::Rational, text);
//                 self.next();
//             }
//             TokenKind::Real => {
//                 let text = self.text();
//                 self.builder.token(SyntaxKind::Real, text);
//                 self.next();
//             }
//             TokenKind::Complex => {
//                 let text = self.text();
//                 self.builder.token(SyntaxKind::Complex, text);
//                 self.next();
//             }
//             TokenKind::String => {
//                 let text = self.text();
//                 self.builder.token(SyntaxKind::String, text);
//                 self.next();
//             }
//             TokenKind::Char => {
//                 let text = self.text();
//                 self.builder.token(SyntaxKind::Char, text);
//                 self.next();
//             }
//             _ => {}
//         }
//         self.builder.finish_node();
//     }
// }

// mod tests {
//     use super::Parser;

//     // #[test]
//     // fn test_int() {
//     //     let src = "42";
//     //     let parse = Parser::new(src).parse();
//     //     let node = parse.syntax();
//     //     let res = &parse.resolver;
//     //     insta::assert_snapshot!(node.debug(res, true));
//     // }

//     // #[test]
//     // fn test_rational() {
//     //     let src = "42/7";
//     //     let parse = Parser::new(src).parse();
//     //     let node = parse.syntax();
//     //     let res = &parse.resolver;
//     //     insta::assert_snapshot!(node.debug(res, true));
//     // }
// }
