use self::cst::{Node, SyntaxKind};
use self::event::MarkOpened;
use self::token::*;
use self::{ast::*, event::Event};
use crate::parser::cst::Child;
use crate::{intern::InternedString, list::List, T};
use logos::{Lexer, Logos};
use num_bigint::BigInt;
use num_complex::Complex64;
use num_rational::Rational64;
use std::{
    cell::Cell,
    collections::HashMap,
    fmt::Display,
    hash::Hash,
    ops::{Index, Range},
};

pub mod ast;
pub mod cst;
pub mod event;
mod tests;
pub mod token;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ParserError(pub String);

impl ParserError {
    pub fn new(msg: &str) -> Self {
        Self(msg.to_string())
    }
}

impl Display for ParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

pub type Result<T> = std::result::Result<T, ParserError>;

pub struct Parser<'src> {
    src: &'src str,
    logos: Lexer<'src, TokenKind>,
    peek: Option<Token>,
    fuel: Cell<u32>,
    events: Vec<Event>,
}

impl<'src> Parser<'src> {
    pub fn new(src: &'src str) -> Self {
        Self {
            src,
            logos: TokenKind::lexer(src),
            peek: None,
            fuel: Cell::new(256),
            events: vec![],
        }
    }

    fn open(&mut self) -> MarkOpened {
        let mark = MarkOpened {
            index: self.events.len(),
        };
        self.events.push(Event::Open {
            kind: SyntaxKind::Error,
        });
        mark
    }

    fn close(&mut self, m: MarkOpened, kind: SyntaxKind) {
        self.events[m.index] = Event::Open { kind };
        self.events.push(Event::Close);
    }

    fn advance(&mut self) {
        assert!(!self.eof());
        self.fuel.set(256);
        self.events.push(Event::Advance);
        self.next();
    }

    fn eof(&mut self) -> bool {
        self.at(T![EOF])
    }

    fn nth(&self, lookahead: usize) -> TokenKind {
        if self.fuel.get() == 0 {
            panic!("Parser out of fuel");
        }
        self.fuel.set(self.fuel.get() - 1);
        self.logos.clone().nth(lookahead).map_or(T![EOF], |t| t)
    }

    fn at(&mut self, kind: TokenKind) -> bool {
        self.peek().kind == kind
    }

    fn eat(&mut self, expected: TokenKind) -> bool {
        if self.at(expected) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn expect(&mut self, expected: TokenKind) {
        if self.eat(expected.clone()) {
            return;
        } else {
            eprintln!("Expected {:?} got {:?}", expected, self.peek());
        }
    }

    fn advance_with_error(&mut self, error: &str) {
        let m = self.open();
        eprintln!("{}", error);
        self.advance();
        self.close(m, SyntaxKind::Error)
    }

    pub fn tokenize(&mut self) -> Vec<Token> {
        self.logos
            .clone()
            .spanned()
            .map(|(t, s)| Token {
                kind: t,
                span: s.into(),
            })
            .collect()
    }

    fn text(&self, token: Token) -> &'src str {
        &self.src[token.span]
    }

    fn next(&mut self) -> Token {
        if let Some(t) = self.peek.take() {
            t
        } else {
            self.generate()
        }
    }

    fn peek(&mut self) -> Token {
        if let Some(t) = self.peek.clone() {
            t
        } else {
            let t = self.generate();
            self.peek = Some(t.clone());
            t
        }
    }

    fn generate(&mut self) -> Token {
        match self.logos.next().map(|t| (t, self.logos.span())) {
            None => Token {
                kind: T![EOF],
                span: Span::new(0, 0),
            },
            Some((T![comment], _)) => self.generate(),
            Some((t, s)) => Token {
                kind: t,
                span: s.into(),
            },
        }
    }

    fn build_tree(&self) -> Node {
        let mut events = self.events.clone();
        let mut stack = vec![];

        assert!(matches!(events.pop(), Some(Event::Close)));

        for e in events {
            match e {
                Event::Open { kind } => {
                    stack.push(Node {
                        kind,
                        children: vec![],
                    });
                }
                Event::Close => {
                    let tree = stack.pop().unwrap();
                    stack.last_mut().unwrap().children.push(Child::Node(tree));
                }
                Event::Advance => {
                    let token = self.next();
                    stack.last_mut().unwrap().children.push(Child::Token(token));
                }
            }
        }
        assert!(stack.len() == 1);
        stack.pop().unwrap()
    }

    // parsing functions

    pub fn file(&mut self) {
        let m = self.open();

        while !self.eof() {
            self.item();
        }

        self.close(m, SyntaxKind::File);
    }

    pub fn item(&mut self) {
        let m = self.open();

        match self.peek().kind {
            // T![data] => self.data(),
            // T![let] => match self.let_() {
            //     LetKind::Decl(decl) => Ok(Item::Decl(decl)),
            //     LetKind::Expr(expr) => Ok(Item::Expr(expr)),
            // },
            // T![fn] => match self.fn_()? {
            //     FnKind::Decl(decl) => Ok(Item::Decl(decl)),
            //     FnKind::Expr(expr) => Ok(Item::Expr(expr)),
            // },
            _ => self.expr(),
        }

        self.close(m, SyntaxKind::Item);
    }

    // fn data(&mut self) {
    //     let m = self.open();

    //     self.eat(T![data]);
    //     self.expect(T![ident]);
    //     self.eat(T![=]);
    //     let mut fields = vec![];
    //     self.eat(T!['{']);
    //     while !self.at(T!['}']) {
    //         self.ident();
    //         self.eat(T![,]);
    //     }

    //     self.close(m, SyntaxKind::Data);
    // }

    fn expr(&mut self) {
        self.pipe()
    }

    // <pipe> ::= <stmt> (<ws>* "|>" <ws>* <pipe>)?
    fn pipe(&mut self) {
        let m = self.open();

        self.stmt();

        if self.eat(T![|>]) {
            self.pipe();
        }

        self.close(m, SyntaxKind::Expr);
    }

    // <stmt> ::= <or> (<ws>* ";" <ws>* <stmt>)?
    fn stmt(&mut self) {
        let m = self.open();

        self.or();

        if self.eat(T![;]) {
            self.stmt();
        }

        self.close(m, SyntaxKind::Expr);
    }

    fn or(&mut self) {
        let m = self.open();

        self.and();

        if self.eat(T![||]) {
            self.or();
        }

        self.close(m, SyntaxKind::Or);
    }

    fn and(&mut self) {
        let m = self.open();

        self.cmp();

        if self.eat(T![&&]) {
            self.and();
        }

        self.close(m, SyntaxKind::And);
    }

    // <cmp> ::= <term> (<ws>* ("<" | ">" | "<=" | ">=" | "=" | "!=") <ws>* <cmp>)?
    fn cmp(&mut self) {
        let m = self.open();

        self.term();

        if self.eat(T![<])
            || self.eat(T![>])
            || self.eat(T![<=])
            || self.eat(T![>=])
            || self.eat(T![=])
            || self.eat(T![!=])
        {
            self.cmp();
        }

        self.close(m, SyntaxKind::Cmp);
    }

    // <term> ::= <factor> (<ws>* ("+" | "-") <ws>* <term>)?
    fn term(&mut self) {
        let m = self.open();

        self.factor();

        if self.eat(T![+]) || self.eat(T![-]) {
            self.term();
        }

        self.close(m, SyntaxKind::Term);
    }

    // <factor> ::= <power> (<ws>* ("*" | "/" | "%") <ws>* <factor>)?
    fn factor(&mut self) {
        let m = self.open();

        self.power();

        if self.eat(T![*]) || self.eat(T![/]) || self.eat(T![%]) {
            self.factor();
        }

        self.close(m, SyntaxKind::Factor);
    }

    // <power> ::= <unary> (<ws>* "^" <ws>* <power>)?
    fn power(&mut self) {
        let m = self.open();

        self.unary();

        if self.eat(T![^]) {
            self.power();
        }

        self.close(m, SyntaxKind::Power);
    }

    // <unary> ::= <apply> | ("-" | "!") <unary>
    fn unary(&mut self) {
        let m = self.open();

        if self.eat(T![-]) || self.eat(T![!]) {
            self.unary();
        } else {
            self.apply();
        }

        self.close(m, SyntaxKind::Unary);
    }

    // <apply> ::= <atom> (<ws>* <atom>)*
    fn apply(&mut self) {
        let m = self.open();

        self.atom();

        while !self.eof() && !self.at(T![|>]) && !self.at(T![;]) {
            self.atom();
        }

        self.close(m, SyntaxKind::Apply);
    }

    // <atom> ::= <ident> | <lit> | <if> | <letExpr> | <fnExpr> | "(" <expr> ")"
    fn atom(&mut self) {
        let m = self.open();

        match self.peek().kind {
            T![ident] => self.ident(),
            T![bool] | T![int] | T![real] | T![str] | T![char] | T!['['] | T!['('] | T!['{'] => {
                self.lit()
            }
            T![if] => self.if_(),
            T![let] => self.let_expr(),
            _ => self.advance_with_error("Expected an atom"),
        }

        self.close(m, SyntaxKind::Atom);
    }

    fn lit(&mut self) {
        let m = self.open();

        match self.peek().kind {
            T![bool] => self.bool(),
            T![int] => self.int(),
            T![real] => self.real(),
            T![str] => self.string(),
            T![char] => self.char(),
            T![lambda] => self.lambda(),
            T!['['] => self.list(),
            T!['('] => {
                self.eat(T!['(']);
                if self.eat(T![')']) {
                    self.close(m.clone(), SyntaxKind::Unit);
                } else {
                    self.expr();
                    self.eat(T![')']);
                    self.close(m.clone(), SyntaxKind::Tuple);
                }
            }
            T!['{'] => self.map(),
            _ => self.advance_with_error("Expected a literal"),
        }

        self.close(m, SyntaxKind::Lit);
    }

    // <if> ::= "if" <ws>+ <expr> <ws>+ "then" <ws>+ <expr>
    //     (<ws>+ "elif" <ws>+ <expr> <ws>+ "then" <ws>+ <expr>)* (<ws>+ "else" <ws>+ <expr>)?
    fn if_(&mut self) {
        let m = self.open();

        self.expect(T![if]);
        self.expr();
        self.expect(T![then]);
        self.expr();

        while self.eat(T![elif]) {
            self.expr();
            self.expect(T![then]);
            self.expr();
        }

        if self.eat(T![else]) {
            self.expr();
        }

        self.close(m, SyntaxKind::If);
    }

    fn let_expr(&mut self) {
        let m = self.open();

        self.expect(T![let]);
        self.let_();
        self.expect(T![in]);
        self.expr();

        self.close(m, SyntaxKind::LetExpr);
    }

    fn let_(&mut self) {
        let m = self.open();

        self.expect(T![ident]);
        if self.eat(T![=]) {
            self.expr();
        } else {
            self.expect(T![:]);
            self.expr();
        }

        self.close(m, SyntaxKind::Let);
    }

    fn bool(&mut self) {
        let m = self.open();
        self.expect(T![bool]);
        self.close(m, SyntaxKind::Bool);
    }

    fn int(&mut self) {
        let m = self.open();
        self.expect(T![int]);
        self.close(m, SyntaxKind::Int);
    }

    fn real(&mut self) {
        let m = self.open();
        self.expect(T![real]);
        self.close(m, SyntaxKind::Real);
    }

    fn string(&mut self) {
        let m = self.open();
        self.expect(T![str]);
        self.close(m, SyntaxKind::String);
    }

    fn char(&mut self) {
        let m = self.open();
        self.expect(T![char]);
        self.close(m, SyntaxKind::Char);
    }

    // <lambda> ::= "\\" <ws>+ <ident>+ <ws>+ "->" <ws>+ <expr>
    fn lambda(&mut self) {
        let m = self.open();

        self.expect(T![lambda]);
        self.ident();
        self.expect(T![->]);
        self.expr();

        self.close(m, SyntaxKind::Lambda);
    }

    // <list> ::= "[" <ws>* <expr> (<ws>* "," <ws>* <expr>)* <ws>* "]"
    fn list(&mut self) {
        let m = self.open();

        self.expect(T!['[']);
        if self.eat(T![']']) {
            self.close(m, SyntaxKind::List);
            return;
        }
        self.expr();
        while self.eat(T![,]) {
            self.expr();
        }
        self.expect(T![']']);

        self.close(m, SyntaxKind::List);
    }

    fn map(&mut self) {
        let m = self.open();

        self.expect(T!['{']);
        if self.eat(T!['}']) {
            self.close(m, SyntaxKind::Map);
            return;
        }
        self.expr();
        while self.eat(T![,]) {
            self.expr();
        }
        self.expect(T!['}']);

        self.close(m, SyntaxKind::Map);
    }

    fn ident(&mut self) {
        let m = self.open();
        self.expect(T![ident]);
        self.close(m, SyntaxKind::Ident);
    }

    // fn curry_fn(&mut self, params: Vec<Pattern>, body: Expr) -> Result<Lambda> {
    //     let last = params.first().ok_or(ParserError(
    //         "Cannot create a lambda with no parameters".to_string(),
    //     ))?;
    //     let iter = params.clone().into_iter().rev().filter(|p| p != last);
    //     let b = iter.fold(body, |acc, p| {
    //         Expr::Lit(Lit::Lambda(Lambda {
    //             param: p,
    //             body: Box::new(acc),
    //         }))
    //     });
    //     Ok(Lambda {
    //         param: last.clone(),
    //         body: Box::new(b),
    //     })
    // }

    // fn curry_apply(&mut self, args: Vec<Expr>, func: Expr) -> Expr {
    //     args.into_iter()
    //         .fold(func, |acc, arg| Expr::Apply(Box::new(acc), Box::new(arg)))
    // }
}
