use std::iter::Peekable;

use crate::{intern::InternedString, T};

use self::{
    ast::{Decl, Expr, File, InfixOp, Item, LetExpr, Pattern, PrefixOp},
    error::Error,
    span::Span,
    token::{Token, TokenKind},
};
use itertools::Itertools;
use logos::{Lexer, Logos};

pub mod ast;
pub mod cst;
pub mod error;
pub mod span;
mod tests;
pub mod token;

#[derive(Debug)]
pub struct Parser<'src> {
    lexer: Lexer<'src, TokenKind>,
    peek: Option<Token>,
    src: &'src str,
    errors: Vec<Error>,
}

impl<'src> Parser<'src> {
    pub fn new(src: &'src str) -> Self {
        Self {
            lexer: TokenKind::lexer(src),
            peek: None,
            src,
            errors: vec![],
        }
    }

    fn fetch_token(&mut self) -> Token {
        match self.lexer.next().map(|res| (res, self.lexer.span())) {
            None => Token {
                kind: T![EOF],
                span: Span::new(0, 0),
            },
            Some((Ok(t), s)) => match t {
                T![comment] => self.fetch_token(),
                _ => Token {
                    kind: t,
                    span: s.into(),
                },
            },
            Some((Err(e), s)) => {
                self.errors.push(Error {
                    message: format!("Lexer error: {:?}: {:?}", s, e),
                });
                self.fetch_token()
            }
        }
    }

    fn next(&mut self) -> Token {
        if let Some(t) = self.peek.take() {
            t
        } else {
            self.fetch_token()
        }
    }

    fn peek(&mut self) -> Token {
        if let Some(t) = self.peek.clone() {
            t
        } else {
            let t = self.fetch_token();
            self.peek = Some(t.clone());
            t
        }
    }

    fn at(&mut self, kind: TokenKind) -> bool {
        self.peek().kind == kind
    }

    fn eat(&mut self, kind: TokenKind) -> bool {
        if self.at(kind) {
            self.next();
            true
        } else {
            false
        }
    }

    fn text(&self, token: Token) -> &'src str {
        &self.src[token.span]
    }

    pub fn parse(&mut self) -> (File, Vec<Error>) {
        (self.file(), self.errors.clone())
    }

    // <file> ::= (<item> <ws>+)+
    fn file(&mut self) -> File {
        let mut items = vec![];
        while self.peek().kind != T![EOF] {
            if let Some(item) = self.item() {
                items.push(item);
            }
        }
        File { items }
    }

    // <item> ::= <decl> | <expr>
    fn item(&mut self) -> Option<Item> {
        match self.peek().kind {
            T![let] | T![type] | T![mod] => match self.decl() {
                Decl::Error => None,
                decl => Some(Item::Decl(decl)),
            },
            _ => match self.expr() {
                Expr::Error => None,
                expr => Some(Item::Expr(expr)),
            },
        }
    }

    fn decl(&mut self) -> Decl {
        todo!()
        // match self.peek().kind {
        //     T![let] => self.let_decl(),
        //     T![type] => self.type_decl(),
        //     T![mod] => self.mod_decl(),
        //     _ => None,
        // }
    }

    fn expr(&mut self) -> Expr {
        self.pipe()
    }

    // <pipe> ::= <stmt> (<ws>* "|>" <ws>* <pipe>)?
    fn pipe(&mut self) -> Expr {
        let lhs = self.stmt();
        if self.eat(T![|>]) {
            Expr::Infix {
                op: InfixOp::Pipe,
                lhs: Box::new(lhs),
                rhs: Box::new(self.pipe()),
            }
        } else {
            lhs
        }
    }

    // <stmt> ::= <or> (<ws>* ";" <ws>* <expr>)?
    // foo 1 |> print "%d"; bar 2
    fn stmt(&mut self) -> Expr {
        let lhs = self.or();
        if self.eat(T![;]) {
            Expr::Let(LetExpr {
                pattern: Pattern::Wildcard,
                value: Box::new(lhs),
                body: Box::new(self.expr()),
            })
        } else {
            lhs
        }
    }

    // <or> ::= <and> (<ws>* "||" <ws>* <or>)?
    fn or(&mut self) -> Expr {
        let lhs = self.and();
        if self.eat(T![||]) {
            Expr::Infix {
                op: InfixOp::Or,
                lhs: Box::new(lhs),
                rhs: Box::new(self.or()),
            }
        } else {
            lhs
        }
    }

    // <and> ::= <cmp> (<ws>* "&&" <ws>* <and>)?
    fn and(&mut self) -> Expr {
        let lhs = self.cmp();
        if self.eat(T![&&]) {
            Expr::Infix {
                op: InfixOp::And,
                lhs: Box::new(lhs),
                rhs: Box::new(self.and()),
            }
        } else {
            lhs
        }
    }

    // <cmp> ::= <term> (<ws>* ("<" | ">" | "<=" | ">=" | "=" | "!=") <ws>* <cmp>)?
    fn cmp(&mut self) -> Expr {
        let lhs = self.term();
        match self.peek().kind {
            _ => lhs,
        }
    }

    // <term> ::= <factor> (<ws>* ("+" | "-") <ws>* <term>)?
    fn term(&mut self) -> Expr {
        let lhs = self.factor();
        match self.peek().kind {
            T![+] => {
                self.next();
                Expr::Infix {
                    op: InfixOp::Add,
                    lhs: Box::new(lhs),
                    rhs: Box::new(self.term()),
                }
            }
            T![-] => {
                self.next();
                Expr::Infix {
                    op: InfixOp::Sub,
                    lhs: Box::new(lhs),
                    rhs: Box::new(self.term()),
                }
            }
            _ => lhs,
        }
    }

    // <factor> ::= <power> (<ws>* ("*" | "/") <ws>* <factor>)?
    fn factor(&mut self) -> Expr {
        let lhs = self.power();
        match self.peek().kind {
            T![*] => {
                self.next();
                Expr::Infix {
                    op: InfixOp::Mul,
                    lhs: Box::new(lhs),
                    rhs: Box::new(self.factor()),
                }
            }
            T![/] => {
                self.next();
                Expr::Infix {
                    op: InfixOp::Div,
                    lhs: Box::new(lhs),
                    rhs: Box::new(self.factor()),
                }
            }
            _ => lhs,
        }
    }

    // <power> ::= <unary> (<ws>* "^" <ws>* <power>)?
    fn power(&mut self) -> Expr {
        let lhs = self.unary();
        match self.peek().kind {
            T![^] => {
                self.next();
                Expr::Infix {
                    op: InfixOp::Pow,
                    lhs: Box::new(lhs),
                    rhs: Box::new(self.power()),
                }
            }
            _ => lhs,
        }
    }

    // <unary> ::= <apply> | ("-" | "!") <unary>
    fn unary(&mut self) -> Expr {
        match self.peek().kind {
            T![-] => {
                self.next();
                Expr::Prefix {
                    op: PrefixOp::Neg,
                    expr: Box::new(self.unary()),
                }
            }
            T![!] => {
                self.next();
                Expr::Prefix {
                    op: PrefixOp::Not,
                    expr: Box::new(self.unary()),
                }
            }
            _ => self.apply(),
        }
    }

    // <apply> ::= <atom> (<ws>+ <apply>)?
    fn apply(&mut self) -> Expr {
        let lhs = self.atom();
        match self.peek().kind {
            T![ident] | T![int] | T![real] | T![str] | T![char] | T![bool] => {
                Expr::Apply(Box::new(lhs), Box::new(self.apply()))
            }
            _ => lhs,
        }
    }

    // <atom> ::= <ident> | <lit> | <if> | <letExpr> | <fnExpr> | "(" <expr> ")"
    fn atom(&mut self) -> Expr {
        match self.peek().kind {
            T![ident] => self.ident(),
            T![int] | T![real] | T![str] | T![char] | T![bool] => self.lit(),
            T![if] => self.if_expr(),
            T![let] => self.let_expr(),
            T![fn] => self.fn_expr(),
            T!['('] => {
                self.next();
                let expr = self.expr();
                self.eat(T![')']);
                expr
            }
            _ => Expr::Error,
        }
    }

    fn ident(&mut self) -> Expr {
        let token = self.next();
        let text = self.text(token);
        Expr::Ident(InternedString::from(text))
    }
}
