use std::vec;

use crate::{
    ast::{Decl, Expr, InfixOp, Lit, PrefixOp, Root},
    error::{ParseResult, ParserError, SyntaxError},
    token::{Token, TokenKind},
};
use logos::{Lexer, Logos};
use miniml_util::{
    intern::InternedString,
    span::{Span, Spannable, Spanned},
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

    fn at(&mut self, kind: TokenKind) -> bool {
        self.peek().value == kind
    }

    fn at_atom(&mut self) -> bool {
        match self.peek().value {
            TokenKind::Ident
            | TokenKind::Int
            | TokenKind::Rational
            | TokenKind::Char
            | TokenKind::String
            | TokenKind::Let
            | TokenKind::If
            | TokenKind::LParen => true,
            _ => false,
        }
    }

    fn at_decl(&mut self) -> bool {
        match self.peek().value {
            TokenKind::Const | TokenKind::Let | TokenKind::Fn => true,
            _ => false,
        }
    }

    fn eat(&mut self, kind: TokenKind) -> ParseResult<()> {
        if self.at(kind) {
            self.next();
            Ok(())
        } else {
            let next = self.next();
            Err(SyntaxError::UnexpectedToken(next.value).spanned(next.span))
        }
    }

    fn text(&self) -> &'src str {
        &self.src[self.lexer.span()]
    }

    pub fn parse(&mut self) -> (Spanned<Root>, Vec<ParserError>) {
        let start = self.peek().span;
        let mut decls = vec![];
        while self.peek().value != TokenKind::Eof {
            match self.decl() {
                Ok(decl) => decls.push(decl),
                Err(err) => self.errors.push(err),
            }
        }
        let end = self.peek().span;
        (
            Root { decls }.spanned(start.extend(end)),
            self.errors.clone(),
        )
    }

    pub fn repl_parse(&mut self) -> (Spanned<Root>, Vec<ParserError>) {
        if self.at_decl() {
            self.parse()
        } else {
            let start = self.peek().span;
            let expr = self.expr();
            let end = self.peek().span;

            let body = Box::new(expr.unwrap_or_else(|err| {
                self.errors.push(err);
                Expr::Error.spanned(start.extend(end))
            }));
            (
                Root {
                    decls: vec![Decl::Fn {
                        name: InternedString::from("main").spanned(Span::new(0, 0)),
                        params: vec![],
                        body,
                    }
                    .spanned(start.extend(end))],
                }
                .spanned(start.extend(end)),
                self.errors.clone(),
            )
        }
    }

    fn decl(&mut self) -> ParseResult<Spanned<Decl>> {
        log::trace!("decl: {:?}", self.peek());
        match self.peek().value {
            TokenKind::Const => self.const_(),
            TokenKind::Let => self.let_(),
            TokenKind::Fn => self.fn_(),
            _ => {
                let next @ Spanned { value, span } = &self.next();
                log::trace!("unexpected token in decl: {:?}", next);
                Err(SyntaxError::UnexpectedToken(value.clone()).spanned(span.clone()))
            }
        }
    }

    fn let_(&mut self) -> ParseResult<Spanned<Decl>> {
        log::trace!("let: {:?}", self.peek());
        let start = self.peek().span;
        self.eat(TokenKind::Let)?;
        let name = self.ident()?;
        let mut params = vec![];
        while self.at(TokenKind::Ident) {
            params.push(self.ident()?);
        }
        self.eat(TokenKind::Eq)?;
        let expr = self.expr()?;

        match &expr.value {
            lam @ Expr::Lambda { params, body } => Ok(Decl::Let {
                name,
                expr: Box::new(lam.spanned(start.extend(self.peek().span))),
                rec: true,
            }
            .spanned(start.extend(self.peek().span))),
            _ => {
                if params.is_empty() {
                    Ok(Decl::Let {
                        name,
                        expr: Box::new(expr),
                        rec: false,
                    }
                    .spanned(start.extend(self.peek().span)))
                } else {
                    Ok(Decl::Let {
                        name,
                        expr: Box::new(
                            Expr::Lambda {
                                params,
                                body: Box::new(expr),
                            }
                            .spanned(start.extend(self.peek().span)),
                        ),
                        rec: true,
                    }
                    .spanned(start.extend(self.peek().span)))
                }
            }
        }
    }

    // <fn> ::= <ident> (<ws>+ <ident>)* <ws>+ "=" <ws>+ <expr>
    fn fn_(&mut self) -> ParseResult<Spanned<Decl>> {
        log::trace!("fn: {:?}", self.peek());
        let start = self.peek().span;
        self.eat(TokenKind::Fn)?;
        let name = self.ident()?;
        let mut params = vec![];
        while self.at(TokenKind::Ident) {
            params.push(self.ident()?);
        }
        self.eat(TokenKind::Eq)?;
        let body = self.expr()?;
        Ok(Decl::Fn {
            name,
            params,
            body: Box::new(body),
        }
        .spanned(start.extend(self.peek().span)))
    }

    fn const_(&mut self) -> ParseResult<Spanned<Decl>> {
        log::trace!("const: {:?}", self.peek());
        let start = self.peek().span;
        self.eat(TokenKind::Const)?;
        let name = self.ident()?;
        self.eat(TokenKind::Eq)?;
        let expr = self.expr()?;
        Ok(Decl::Const {
            name,
            expr: Box::new(expr),
        }
        .spanned(start.extend(self.peek().span)))
    }

    // <expr> ::= <pipe>
    fn expr(&mut self) -> ParseResult<Spanned<Expr>> {
        log::trace!("expr: {:?}", self.peek());
        self.pipe()
    }

    // <pipe> ::= <stmt> (<ws>* "|>" <ws>* <pipe>)?
    fn pipe(&mut self) -> ParseResult<Spanned<Expr>> {
        log::trace!("enter pipe: {:?}", self.peek());
        let start = self.peek().span;
        let lhs = self.stmt()?;
        if self.at(TokenKind::Pipe) {
            log::trace!("pipe: {:?}", self.peek());
            let op_span = self.peek().span.clone();
            self.eat(TokenKind::Pipe)?;
            let rhs = self.pipe()?;
            Ok(Expr::Infix {
                op: InfixOp::Pipe.spanned(op_span),
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            }
            .spanned(start.extend(self.peek().span)))
        } else {
            Ok(lhs)
        }
    }

    // <stmt> ::= <or> (<ws>* ";" <ws>* <stmt>)?
    fn stmt(&mut self) -> ParseResult<Spanned<Expr>> {
        log::trace!("enter stmt: {:?}", self.peek());
        let start = self.peek().span;
        let lhs = self.or()?;
        if self.at(TokenKind::Semicolon) {
            log::trace!("stmt: {:?}", self.peek());
            let op_span = self.peek().span.clone();
            self.eat(TokenKind::Semicolon)?;
            let rhs = self.stmt()?;
            Ok(Expr::Infix {
                op: InfixOp::Stmt.spanned(op_span),
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            }
            .spanned(start.extend(self.peek().span)))
        } else {
            Ok(lhs)
        }
    }

    // <or> ::= <and> (<ws>* "or" <ws>* <or>)?
    fn or(&mut self) -> ParseResult<Spanned<Expr>> {
        log::trace!("enter or: {:?}", self.peek());
        let start = self.peek().span;
        let lhs = self.and()?;
        if self.at(TokenKind::Or) {
            log::trace!("or: {:?}", self.peek());
            let op_span = self.peek().span.clone();
            self.eat(TokenKind::Or)?;
            let rhs = self.or()?;
            Ok(Expr::Infix {
                op: InfixOp::Or.spanned(op_span),
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            }
            .spanned(start.extend(self.peek().span)))
        } else {
            Ok(lhs)
        }
    }

    // <and> ::= <cmp> (<ws>* "and" <ws>* <and>)?
    fn and(&mut self) -> ParseResult<Spanned<Expr>> {
        log::trace!("enter and: {:?}", self.peek());
        let start = self.peek().span;
        let lhs = self.cmp()?;
        if self.at(TokenKind::And) {
            log::trace!("and: {:?}", self.peek());
            let op_span = self.peek().span.clone();
            self.eat(TokenKind::And)?;
            let rhs = self.and()?;
            Ok(Expr::Infix {
                op: InfixOp::And.spanned(op_span),
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            }
            .spanned(start.extend(self.peek().span)))
        } else {
            Ok(lhs)
        }
    }

    // <cmp> ::= <term> (<ws>* ("==" | "!=" | "<" | "<=" | ">" | ">=") <ws>* <cmp>)?
    fn cmp(&mut self) -> ParseResult<Spanned<Expr>> {
        log::trace!("enter cmp: {:?}", self.peek());
        let start = self.peek().span;
        let lhs = self.term()?;
        match self.peek().value {
            TokenKind::Lt => {
                log::trace!("cmp: {:?}", self.peek());
                let op_span = self.peek().span.clone();
                self.eat(TokenKind::Lt)?;
                let rhs = self.cmp()?;
                Ok(Expr::Infix {
                    op: InfixOp::Lt.spanned(op_span),
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                }
                .spanned(start.extend(self.peek().span)))
            }
            TokenKind::Gt => {
                log::trace!("cmp: {:?}", self.peek());
                let op_span = self.peek().span.clone();
                self.eat(TokenKind::Gt)?;
                let rhs = self.cmp()?;
                Ok(Expr::Infix {
                    op: InfixOp::Gt.spanned(op_span),
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                }
                .spanned(start.extend(self.peek().span)))
            }
            TokenKind::Leq => {
                log::trace!("cmp: {:?}", self.peek());
                let op_span = self.peek().span.clone();
                self.eat(TokenKind::Leq)?;
                let rhs = self.cmp()?;
                Ok(Expr::Infix {
                    op: InfixOp::Leq.spanned(op_span),
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                }
                .spanned(start.extend(self.peek().span)))
            }
            TokenKind::Geq => {
                log::trace!("cmp: {:?}", self.peek());
                let op_span = self.peek().span.clone();
                self.eat(TokenKind::Geq)?;
                let rhs = self.cmp()?;
                Ok(Expr::Infix {
                    op: InfixOp::Geq.spanned(op_span),
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                }
                .spanned(start.extend(self.peek().span)))
            }
            TokenKind::Eq => {
                log::trace!("cmp: {:?}", self.peek());
                let op_span = self.peek().span.clone();
                self.eat(TokenKind::Eq)?;
                let rhs = self.cmp()?;
                Ok(Expr::Infix {
                    op: InfixOp::Eq.spanned(op_span),
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                }
                .spanned(start.extend(self.peek().span)))
            }
            TokenKind::Neq => {
                log::trace!("cmp: {:?}", self.peek());
                let op_span = self.peek().span.clone();
                self.eat(TokenKind::Neq)?;
                let rhs = self.cmp()?;
                Ok(Expr::Infix {
                    op: InfixOp::Neq.spanned(op_span),
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                }
                .spanned(start.extend(self.peek().span)))
            }
            _ => Ok(lhs),
        }
    }

    // <term> ::= <factor> (<ws>* ("+" | "-") <ws>* <term>)?
    fn term(&mut self) -> ParseResult<Spanned<Expr>> {
        log::trace!("enter term: {:?}", self.peek());
        let start = self.peek().span;
        let lhs = self.factor()?;
        match self.peek().value {
            TokenKind::Plus => {
                let op_span = self.peek().span.clone();
                self.eat(TokenKind::Plus)?;
                log::trace!("term: {:?}", self.peek());
                let rhs = self.term()?;
                Ok(Expr::Infix {
                    op: InfixOp::Add.spanned(op_span),
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                }
                .spanned(start.extend(self.peek().span)))
            }
            TokenKind::Minus => {
                let op_span = self.peek().span.clone();
                self.eat(TokenKind::Minus)?;
                log::trace!("term: {:?}", self.peek());
                let rhs = self.term()?;
                Ok(Expr::Infix {
                    op: InfixOp::Sub.spanned(op_span),
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                }
                .spanned(start.extend(self.peek().span)))
            }
            _ => Ok(lhs),
        }
    }

    // <factor> ::= <power> (<ws>* ("*" | "/" | "%") <ws>* <factor>)?
    fn factor(&mut self) -> ParseResult<Spanned<Expr>> {
        log::trace!("enter factor: {:?}", self.peek());
        let start = self.peek().span;
        let lhs = self.power()?;
        match self.peek().value {
            TokenKind::Star => {
                log::trace!("factor: {:?}", self.peek());
                let op_span = self.peek().span.clone();
                self.eat(TokenKind::Star)?;
                let rhs = self.factor()?;
                Ok(Expr::Infix {
                    op: InfixOp::Mul.spanned(op_span),
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                }
                .spanned(start.extend(self.peek().span)))
            }
            TokenKind::Slash => {
                log::trace!("factor: {:?}", self.peek());
                let op_span = self.peek().span.clone();
                self.eat(TokenKind::Slash)?;
                let rhs = self.factor()?;
                Ok(Expr::Infix {
                    op: InfixOp::Div.spanned(op_span),
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                }
                .spanned(start.extend(self.peek().span)))
            }
            TokenKind::Percent => {
                log::trace!("factor: {:?}", self.peek());
                let op_span = self.peek().span.clone();
                self.eat(TokenKind::Percent)?;
                let rhs = self.factor()?;
                Ok(Expr::Infix {
                    op: InfixOp::Rem.spanned(op_span),
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                }
                .spanned(start.extend(self.peek().span)))
            }

            _ => Ok(lhs),
        }
    }

    // <power> ::= <unary> (<ws>* "^" <ws>* <power>)?
    fn power(&mut self) -> ParseResult<Spanned<Expr>> {
        log::trace!("enter power: {:?}", self.peek());
        let start = self.peek().span;
        let lhs = self.unary()?;
        match self.peek().value {
            TokenKind::Caret => {
                log::trace!("power: {:?}", self.peek());
                let op_span = self.peek().span.clone();
                self.eat(TokenKind::Caret)?;
                let rhs = self.power()?;
                Ok(Expr::Infix {
                    op: InfixOp::Pow.spanned(op_span),
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                }
                .spanned(start.extend(self.peek().span)))
            }

            _ => Ok(lhs),
        }
    }

    // <unary> ::= <apply> | ("-" | "not") <unary>
    fn unary(&mut self) -> ParseResult<Spanned<Expr>> {
        log::trace!("enter unary: {:?}", self.peek());
        let start = self.peek().span;
        match self.peek().value {
            TokenKind::Minus => {
                log::trace!("unary: {:?}", self.peek());
                let op_span = self.peek().span.clone();
                self.eat(TokenKind::Minus)?;
                let rhs = self.unary()?;
                Ok(Expr::Prefix {
                    op: PrefixOp::Neg.spanned(op_span),
                    expr: Box::new(rhs),
                }
                .spanned(start.extend(self.peek().span)))
            }
            TokenKind::Not => {
                log::trace!("unary: {:?}", self.peek());
                let op_span = self.peek().span.clone();
                self.eat(TokenKind::Not)?;
                let rhs = self.unary()?;
                Ok(Expr::Prefix {
                    op: PrefixOp::Not.spanned(op_span),
                    expr: Box::new(rhs),
                }
                .spanned(start.extend(self.peek().span)))
            }
            _ => self.apply(),
        }
    }

    // <apply> ::= <atom> (<ws>+ <atom>)*
    fn apply(&mut self) -> ParseResult<Spanned<Expr>> {
        log::trace!("enter apply: {:?}", self.peek());
        let start = self.peek().span;
        let fun = self.atom()?;
        log::trace!("fun: {:?}", fun);
        log::trace!("apply: {:?}", self.peek());
        let mut args = vec![];
        let mut span = self.peek().span.clone();
        if self.at_atom() {
            loop {
                log::trace!("apply: {:?}", self.peek());
                span = self.peek().span.clone();
                args.push(self.atom()?);
                if !self.at_atom() {
                    break;
                }
            }
            Ok(Expr::Apply {
                fun: Box::new(fun),
                args,
            }
            .spanned(start.extend(span)))
        } else {
            log::trace!("apply ret: {:?}", self.peek());
            return Ok(fun);
        }
    }

    // <atom> ::= <ident> | <lit> | <letExpr> | <lambda> | <list> | <tuple> | <map> | "(" <ws>* <expr> <ws>* ")"
    fn atom(&mut self) -> ParseResult<Spanned<Expr>> {
        log::trace!("enter atom: {:?}", self.peek());
        let start = self.peek().span;
        match self.peek().value {
            TokenKind::Ident => {
                log::trace!("atom: {:?}", self.peek());
                let name = self.ident()?;
                Ok(Expr::Ident(name).spanned(start))
            }
            TokenKind::Int
            | TokenKind::Rational
            | TokenKind::Real
            | TokenKind::Complex
            | TokenKind::Char
            | TokenKind::String => {
                let lit = self.lit()?;
                Ok(Expr::Lit(lit.clone().into()).spanned(lit.span))
            }
            TokenKind::Let => self.let_expr(),
            TokenKind::If => self.if_(),
            TokenKind::Backslash => self.lambda(),
            TokenKind::LParen => {
                log::trace!("atom: {:?}", self.peek());
                self.eat(TokenKind::LParen)?;
                if self.at(TokenKind::RParen) {
                    let span = self.peek().span.clone();
                    self.eat(TokenKind::RParen)?;
                    log::trace!("unit span: {:?}", span);
                    return Ok(Expr::Unit.spanned(start.extend(span)));
                }
                let expr = self.expr()?;
                self.eat(TokenKind::RParen)?;
                Ok(expr)
            }
            _ => {
                let tok = self.next();
                Err(SyntaxError::UnexpectedToken(tok.value).spanned(tok.span))
            }
        }
    }

    fn let_expr(&mut self) -> ParseResult<Spanned<Expr>> {
        log::trace!("enter let: {:?}", self.peek());
        let start = self.peek().span;
        self.eat(TokenKind::Let)?;
        let name = self.ident()?;
        self.eat(TokenKind::Eq)?;
        let expr = self.expr()?;
        self.eat(TokenKind::In)?;
        let body = self.expr()?;
        Ok(Expr::Let {
            name,
            expr: Box::new(expr),
            body: Box::new(body),
            rec: false,
        }
        .spanned(start.extend(self.peek().span)))
    }

    fn if_(&mut self) -> ParseResult<Spanned<Expr>> {
        log::trace!("enter if: {:?}", self.peek());
        let start = self.peek().span;
        self.eat(TokenKind::If)?;
        let cond = self.expr()?;
        self.eat(TokenKind::Then)?;
        let then = self.expr()?;
        let mut elifs = vec![];
        while self.at(TokenKind::Elif) {
            self.eat(TokenKind::Elif)?;
            let cond = self.expr()?;
            self.eat(TokenKind::Then)?;
            let then = self.expr()?;
            elifs.push((Box::new(cond), Box::new(then)));
        }
        self.eat(TokenKind::Else)?;
        let else_ = self.expr()?;
        Ok(Expr::If {
            cond: Box::new(cond),
            then: Box::new(then),
            elifs,
            else_: Box::new(else_),
        }
        .spanned(start.extend(self.peek().span)))
    }

    fn lambda(&mut self) -> ParseResult<Spanned<Expr>> {
        log::trace!("enter lambda: {:?}", self.peek());
        let start = self.peek().span;
        self.eat(TokenKind::Backslash)?;
        let mut params = vec![];
        let mut span = self.peek().span.clone();
        while !self.at(TokenKind::Arrow) {
            span = self.peek().span.clone();
            params.push(self.ident()?);
        }
        self.eat(TokenKind::Arrow)?;
        let body = self.expr()?;
        Ok(Expr::Lambda {
            params,
            body: Box::new(body),
        }
        .spanned(start.extend(span)))
    }

    fn ident(&mut self) -> ParseResult<Spanned<InternedString>> {
        let span = self.peek().span.clone();
        match self.peek().value {
            TokenKind::Ident => {
                let name = self.text();
                self.next();
                Ok(InternedString::from(name).spanned(span))
            }
            _ => {
                let tok = self.next();
                Err(SyntaxError::UnexpectedToken(tok.value).spanned(tok.span))
            }
        }
    }

    fn lit(&mut self) -> ParseResult<Spanned<Lit>> {
        log::trace!("enter lit: {:?}", self.peek());
        let peek = self.peek();
        match peek.value {
            TokenKind::Int => {
                log::trace!("lit: {:?}", self.peek());
                let value = self.text();
                let span = self.peek().span.clone();
                self.next();
                Ok(Lit::Int(
                    value
                        .parse()
                        .map_err(|_| SyntaxError::InvalidIntLit(peek.clone()).spanned(peek.span))?,
                )
                .spanned(span))
            }
            TokenKind::Rational => {
                log::trace!("lit: {:?}", self.peek());
                let value = self.text();
                self.next();
                Ok(Lit::Rational(value.parse().map_err(|_| {
                    SyntaxError::InvalidRationalLit(peek.clone()).spanned(peek.span)
                })?)
                .spanned(peek.span))
            }
            TokenKind::Real => {
                log::trace!("lit: {:?}", self.peek());
                let value = self.text();
                self.next();
                Ok(Lit::Real(
                    value.parse().map_err(|_| {
                        SyntaxError::InvalidRealLit(peek.clone()).spanned(peek.span)
                    })?,
                )
                .spanned(peek.span))
            }
            TokenKind::Complex => {
                log::trace!("lit: {:?}", self.peek());
                let value = self.text();
                self.next();
                Ok(Lit::Complex(value.parse().map_err(|_| {
                    SyntaxError::InvalidComplexLit(peek.clone()).spanned(peek.span)
                })?)
                .spanned(peek.span))
            }
            _ => {
                let tok = self.next();
                Err(SyntaxError::UnexpectedToken(tok.value).spanned(tok.span))
            }
        }
    }
}

mod tests {
    use crate::{ast::Format, parser::Parser};

    #[test]
    fn test_int() {
        let mut parser = Parser::new("123");
        let expr = parser.expr().expect("parse error");
        let fmt = Format {
            indent: 0,
            value: expr,
        };
        insta::assert_debug_snapshot!(fmt);
    }

    #[test]
    fn test_binary() {
        let mut parser = Parser::new("0b10101");
        let expr = parser.expr().expect("parse error");
        let fmt = Format {
            indent: 0,
            value: expr,
        };
        insta::assert_debug_snapshot!(fmt);
    }

    #[test]
    fn test_rational() {
        let mut parser = Parser::new("123/456");
        let expr = parser.expr().expect("parse error");
        let fmt = Format {
            indent: 0,
            value: expr,
        };
        insta::assert_debug_snapshot!(fmt);
    }

    #[test]
    fn test_real() {
        let mut parser = Parser::new("123.456");
        let expr = parser.expr().expect("parse error");
        let fmt = Format {
            indent: 0,
            value: expr,
        };
        insta::assert_debug_snapshot!(fmt);
    }

    #[test]
    fn test_complex() {
        let mut parser = Parser::new("123.456i");
        let expr = parser.expr().expect("parse error");
        let fmt = Format {
            indent: 0,
            value: expr,
        };
        insta::assert_debug_snapshot!(fmt);
    }

    #[test]
    fn test_unit() {
        let mut parser = Parser::new("()");
        let expr = parser.expr().expect("parse error");
        let fmt = Format {
            indent: 0,
            value: expr,
        };
        insta::assert_debug_snapshot!(fmt);
    }

    #[test]
    fn test_neg() {
        let mut parser = Parser::new("-x");
        let expr = parser.expr().expect("parse error");
        let fmt = Format {
            indent: 0,
            value: expr,
        };
        insta::assert_debug_snapshot!(fmt);
    }

    #[test]
    fn test_add() {
        let mut parser = Parser::new("1 + 2");
        let expr = parser.expr().expect("parse error");
        let fmt = Format {
            indent: 0,
            value: expr,
        };
        insta::assert_debug_snapshot!(fmt);
    }

    #[test]
    fn test_infix() {
        let mut parser = Parser::new("1 + 2 * 3 / 4^1/2 - 2.5");
        let expr = parser.expr().expect("parse error");
        let fmt = Format {
            indent: 0,
            value: expr,
        };
        insta::assert_debug_snapshot!(fmt);
    }

    #[test]
    fn test_if() {
        let mut parser = Parser::new("if x then y else z");
        let expr = parser.expr().expect("parse error");
        let fmt = Format {
            indent: 0,
            value: expr,
        };
        insta::assert_debug_snapshot!(fmt);
    }

    #[test]
    fn test_elif() {
        let mut parser = Parser::new("if x then y elif a then b else z");
        let expr = parser.expr().expect("parse error");
        let fmt = Format {
            indent: 0,
            value: expr,
        };
        insta::assert_debug_snapshot!(fmt);
    }

    #[test]
    fn test_if_atom() {
        let mut parser = Parser::new("add 1 if x then y else z");
        let expr = parser.expr().expect("parse error");
        let fmt = Format {
            indent: 0,
            value: expr,
        };
        insta::assert_debug_snapshot!(fmt);
    }

    #[test]
    fn test_lambda() {
        let mut parser = Parser::new("\\x y -> x + y");
        let expr = parser.expr().expect("parse error");
        let fmt = Format {
            indent: 0,
            value: expr,
        };
        insta::assert_debug_snapshot!(fmt);
    }

    #[test]
    fn test_let_expr() {
        let mut parser = Parser::new("let x = 1 in x + 1");
        let expr = parser.expr().expect("parse error");
        let fmt = Format {
            indent: 0,
            value: expr,
        };
        insta::assert_debug_snapshot!(fmt);
    }

    #[test]
    fn test_const() {
        let mut parser = Parser::new("const x = 1");
        let (root, errors) = parser.parse();
        if !errors.is_empty() {
            panic!("parse error: {:?}", errors);
        }
        let fmt = Format {
            indent: 0,
            value: root,
        };
        insta::assert_debug_snapshot!(fmt);
    }

    #[test]
    fn test_let() {
        let mut parser = Parser::new("let x = 1");
        let (root, errors) = parser.parse();
        if !errors.is_empty() {
            panic!("parse error: {:?}", errors);
        }
        let fmt = Format {
            indent: 0,
            value: root,
        };
        insta::assert_debug_snapshot!(fmt);
    }

    #[test]
    fn test_fn_add() {
        let mut parser = Parser::new("fn add x y = x + y");
        let (root, errors) = parser.parse();
        if !errors.is_empty() {
            panic!("parse error: {:?}", errors);
        }
        let fmt = Format {
            indent: 0,
            value: root,
        };
        insta::assert_debug_snapshot!(fmt);
    }
}
