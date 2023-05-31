use self::ast::*;
use self::cst::Node;
use self::token::*;
use crate::{intern::InternedString, list::List, T};
use logos::{Lexer, Logos};
use num_bigint::BigInt;
use num_complex::Complex64;
use num_rational::Rational64;
use std::{
    collections::HashMap,
    fmt::Display,
    hash::Hash,
    ops::{Index, Range},
};

pub mod ast;
pub mod cst;
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
}

impl<'src> Parser<'src> {
    pub fn new(src: &'src str) -> Self {
        Self {
            src,
            logos: TokenKind::lexer(src),
            peek: None,
        }
    }

    pub fn tokenize(&mut self) -> Vec<Token> {
        self.logos
            .clone()
            .spanned()
            .map(|(t, s)| Token::from((t, s.into())))
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
            None => Token::from((T![EOF], Span::new(0, 0))),
            Some((T![comment], _)) => self.generate(),
            Some((t, s)) => Token::from((t, s.into())),
        }
    }

    fn at(&mut self, kind: TokenKind) -> bool {
        self.peek().value == kind
    }

    fn consume(&mut self, expected: TokenKind) {
        let token = self.next();
        assert_eq!(
            token.value, expected,
            "Expected to consume `{}`, but found `{:?}`",
            expected, token
        );
    }

    pub fn item(&mut self) -> Result<Item> {
        match self.peek().value {
            T![data] => Ok(Item::Data(self.data()?)),
            T![let] => match self.let_()? {
                LetKind::Decl(decl) => Ok(Item::Decl(decl)),
                LetKind::Expr(expr) => Ok(Item::Expr(expr)),
            },
            T![fn] => match self.fn_()? {
                FnKind::Decl(decl) => Ok(Item::Decl(decl)),
                FnKind::Expr(expr) => Ok(Item::Expr(expr)),
            },
            _ => Ok(Item::Expr(self.expr()?)),
        }
    }

    fn data(&mut self) -> Result<Data> {
        self.consume(T![data]);
        let name = self.ident()?;
        self.consume(T![=]);
        let mut fields = vec![];
        self.consume(T!['{']);
        while !self.at(T!['}']) {
            fields.push(self.ident()?);
            if !self.at(T!['}']) {
                self.consume(T![,]);
            }
        }
        Ok(Data { name, fields })
    }

    fn let_(&mut self) -> Result<LetKind> {
        self.consume(T![let]);
        let pattern = self.pattern()?;

        self.consume(T![=]);

        let value = self.expr()?;

        if self.at(T![in]) {
            self.consume(T![in]);
            let body = self.expr()?;
            Ok(LetKind::Expr(Expr::Let(LetExpr {
                pattern,
                value: Box::new(value),
                body: Box::new(body),
            })))
        } else {
            Ok(LetKind::Decl(Decl::Let(LetDecl {
                pattern,
                value: Box::new(value),
            })))
        }
    }

    fn let_decl(&mut self) -> Result<Decl> {
        self.consume(T![let]);
        let pattern = self.pattern()?;
        self.consume(T![=]);
        let value = self.expr()?;
        Ok(Decl::Let(LetDecl {
            pattern,
            value: Box::new(value),
        }))
    }

    fn fn_decl(&mut self) -> Result<FnDecl> {
        self.consume(T![fn]);
        let name = self.ident()?;
        let mut params = vec![];
        while !self.at(T![=]) {
            params.push(self.pattern()?);
        }
        self.consume(T![=]);
        let inner = self.expr()?;
        let value = self.curry_fn(params, inner)?;
        Ok(FnDecl {
            name: name.clone(),
            value: Box::new(Expr::Lit(Lit::Lambda(value))),
        })
    }

    fn fn_(&mut self) -> Result<FnKind> {
        let decl = self.fn_decl()?;
        if self.at(T![in]) {
            self.consume(T![in]);
            let body = self.expr()?;
            Ok(FnKind::Expr(Expr::Fn(FnExpr {
                name: decl.name,
                value: decl.value,
                body: Box::new(body),
            })))
        } else {
            Ok(FnKind::Decl(Decl::Fn(decl)))
        }
    }

    fn expr(&mut self) -> Result<Expr> {
        self.or()
    }

    fn or(&mut self) -> Result<Expr> {
        let lhs = self.and()?;
        match self.peek().value {
            T![||] => {
                self.consume(T![||]);
                let rhs = self.and()?;
                Ok(Expr::Infix {
                    op: InfixOp::Or,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                })
            }
            _ => Ok(lhs),
        }
    }

    fn and(&mut self) -> Result<Expr> {
        let lhs = self.eq()?;
        match self.peek().value {
            T![&&] => {
                self.consume(T![&&]);
                let rhs = self.eq()?;
                Ok(Expr::Infix {
                    op: InfixOp::And,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                })
            }
            _ => Ok(lhs),
        }
    }

    fn eq(&mut self) -> Result<Expr> {
        let lhs = self.cmp()?;
        match self.peek().value {
            T![=] => {
                self.consume(T![=]);
                let rhs = self.cmp()?;
                Ok(Expr::Infix {
                    lhs: Box::new(lhs),
                    op: InfixOp::Eq,
                    rhs: Box::new(rhs),
                })
            }
            T![!=] => {
                self.consume(T![!=]);
                let rhs = self.cmp()?;
                Ok(Expr::Infix {
                    lhs: Box::new(lhs),
                    op: InfixOp::Neq,
                    rhs: Box::new(rhs),
                })
            }
            _ => Ok(lhs),
        }
    }

    fn cmp(&mut self) -> Result<Expr> {
        let lhs = self.term()?;
        match self.peek().value {
            T![<] => {
                self.consume(T![<]);
                let rhs = self.term()?;
                Ok(Expr::Infix {
                    lhs: Box::new(lhs),
                    op: InfixOp::Lss,
                    rhs: Box::new(rhs),
                })
            }
            T![>] => {
                self.consume(T![>]);
                let rhs = self.term()?;
                Ok(Expr::Infix {
                    lhs: Box::new(lhs),
                    op: InfixOp::Gtr,
                    rhs: Box::new(rhs),
                })
            }
            T![<=] => {
                self.consume(T![<=]);
                let rhs = self.term()?;
                Ok(Expr::Infix {
                    lhs: Box::new(lhs),
                    op: InfixOp::Leq,
                    rhs: Box::new(rhs),
                })
            }
            T![>=] => {
                self.consume(T![>=]);
                let rhs = self.term()?;
                Ok(Expr::Infix {
                    lhs: Box::new(lhs),
                    op: InfixOp::Geq,
                    rhs: Box::new(rhs),
                })
            }
            _ => Ok(lhs),
        }
    }

    fn term(&mut self) -> Result<Expr> {
        let mut lhs = self.factor()?;
        loop {
            match self.peek().value {
                T![+] => {
                    self.consume(T![+]);
                    let rhs = self.factor()?;
                    lhs = Expr::Infix {
                        lhs: Box::new(lhs),
                        op: InfixOp::Add,
                        rhs: Box::new(rhs),
                    };
                }
                T![-] => {
                    self.consume(T![-]);
                    let rhs = self.factor()?;
                    lhs = Expr::Infix {
                        lhs: Box::new(lhs),
                        op: InfixOp::Sub,
                        rhs: Box::new(rhs),
                    };
                }
                _ => break,
            }
        }
        Ok(lhs)
    }

    fn factor(&mut self) -> Result<Expr> {
        let mut lhs = self.power()?;
        loop {
            match self.peek().value {
                T![*] => {
                    self.consume(T![*]);
                    let rhs = self.power()?;
                    lhs = Expr::Infix {
                        lhs: Box::new(lhs),
                        op: InfixOp::Mul,
                        rhs: Box::new(rhs),
                    };
                }
                T![/] => {
                    self.consume(T![/]);
                    let rhs = self.power()?;
                    lhs = Expr::Infix {
                        lhs: Box::new(lhs),
                        op: InfixOp::Div,
                        rhs: Box::new(rhs),
                    };
                }
                T![%] => {
                    self.consume(T![%]);
                    let rhs = self.power()?;
                    lhs = Expr::Infix {
                        lhs: Box::new(lhs),
                        op: InfixOp::Mod,
                        rhs: Box::new(rhs),
                    };
                }
                _ => break,
            }
        }
        Ok(lhs)
    }

    fn power(&mut self) -> Result<Expr> {
        let mut lhs = self.unary()?;
        while self.at(T![^]) {
            self.consume(T![^]);
            let rhs = self.unary()?;
            lhs = Expr::Infix {
                lhs: Box::new(lhs),
                op: InfixOp::Pow,
                rhs: Box::new(rhs),
            };
        }
        Ok(lhs)
    }

    fn unary(&mut self) -> Result<Expr> {
        match self.peek().value {
            T![!] => {
                self.consume(T![!]);
                let rhs = self.unary()?;
                Ok(Expr::Prefix {
                    op: PrefixOp::Not,
                    expr: Box::new(rhs),
                })
            }
            T![-] => {
                self.consume(T![-]);
                let rhs = self.unary()?;
                Ok(Expr::Prefix {
                    op: PrefixOp::Neg,
                    expr: Box::new(rhs),
                })
            }
            _ => self.apply(),
        }
    }

    fn apply(&mut self) -> Result<Expr> {
        let lhs = self.atom()?;
        let mut args = vec![];
        loop {
            match self.peek().value {
                T![ident]
                | T![int]
                | T![real]
                | T![str]
                | T![char]
                | T![bool]
                | T![lambda]
                | T![if]
                | T![let]
                | T!['(']
                | T!['[']
                | T!['{'] => {
                    let arg = self.atom()?;
                    args.push(arg);
                }
                _ => break,
            }
        }
        Ok(self.curry_apply(args, lhs))
    }

    fn atom(&mut self) -> Result<Expr> {
        let tok = self.peek();
        match tok.value {
            T![if] => self.if_(),
            T![let] => self.let_expr(),
            T![fn] => self.fn_expr(),
            T![int] | T![real] | T![str] | T![char] | T![bool] | T![lambda] | T!['['] | T!['{'] => {
                Ok(Expr::Lit(self.lit()?))
            }
            T![ident] => Ok(Expr::Ident(self.ident()?)),
            T!['('] => {
                self.consume(T!['(']);
                let expr = self.expr()?;
                if self.at(T![,]) {
                    self.consume(T![,]);
                    let mut items = vec![expr.clone()];
                    items.append(&mut self.tuple_items()?);
                    self.consume(T![')']);
                    Ok(Expr::Lit(Lit::Tuple(Tuple { items })))
                } else {
                    self.consume(T![')']);
                    Ok(expr)
                }
            }
            _ => Err(ParserError(format!(
                "Unexpected token in atom got `{:?}` - `{}`",
                self.peek(),
                self.text(tok)
            ))),
        }
    }

    fn let_expr(&mut self) -> Result<Expr> {
        let kind = self.let_()?;
        match kind {
            LetKind::Expr(expr) => Ok(expr),
            _ => Err(ParserError::new("Expected a let expression")),
        }
    }

    fn fn_expr(&mut self) -> Result<Expr> {
        let kind = self.fn_()?;
        match kind {
            FnKind::Expr(expr) => Ok(expr),
            _ => Err(ParserError::new("Expected a function expression")),
        }
    }

    fn if_(&mut self) -> Result<Expr> {
        self.consume(T![if]);
        let cond = self.expr()?;
        self.consume(T![then]);
        let then = self.expr()?;
        let mut elifs = vec![];
        while self.at(T![elif]) {
            self.consume(T![elif]);
            let cond = self.expr()?;
            self.consume(T![then]);
            let then = self.expr()?;
            elifs.push((cond, then));
        }
        self.consume(T![else]);
        let else_ = self.expr()?;

        let top_else = elifs
            .into_iter()
            .fold(else_, |acc, (elif_cond, elif_then)| Expr::If {
                cond: Box::new(elif_cond),
                then: Box::new(elif_then),
                else_: Box::new(acc),
            });
        Ok(Expr::If {
            cond: Box::new(cond),
            then: Box::new(then),
            else_: Box::new(top_else),
        })
    }

    fn match_(&mut self) -> Result<Expr> {
        self.consume(T![match]);
        let expr = self.expr()?;
        self.consume(T![with]);
        let mut arms = vec![];
        while !self.at(T![|]) {
            let pattern = self.full_pattern()?;
            self.consume(T![->]);
            let expr = self.expr()?;
            arms.push(MatchArm { pattern, expr });
        }
        Ok(Expr::Match {
            expr: Box::new(expr),
            arms,
        })
    }

    fn full_pattern(&mut self) -> Result<Pattern> {
        let tok = self.peek();
        match tok.value {
            T![ident] => Ok(Pattern::Ident(self.ident()?)),
            T![int] => Ok(Pattern::Int(self.int()?)),
            T![str] => Ok(Pattern::String(self.string()?)),
            T![char] => Ok(Pattern::Char(self.char()?)),
            T![bool] => Ok(Pattern::Bool(self.bool()?)),
            T!['['] => self.full_list_pattern(),
            T!['('] => self.full_tuple_pattern(),
            T!['{'] => self.full_map_pattern(),
            _ => Err(ParserError(format!(
                "Unexpected token in pattern got `{:?}` - `{}`",
                self.peek(),
                self.text(tok)
            ))),
        }
    }

    fn full_list_pattern(&mut self) -> Result<Pattern> {
        self.consume(T!['[']);
        let mut items = vec![];
        while !self.at(T![']']) {
            items.push(self.full_pattern()?);
            if !self.at(T![']']) {
                self.consume(T![,]);
            }
        }
        self.consume(T![']']);
        Ok(Pattern::List(ListPattern { items }))
    }

    fn full_tuple_pattern(&mut self) -> Result<Pattern> {
        self.consume(T!['(']);
        if self.at(T![')']) {
            self.consume(T![')']);
            return Ok(Pattern::Unit);
        }
        let mut items = vec![];
        while !self.at(T![')']) {
            items.push(self.full_pattern()?);
            if !self.at(T![')']) {
                self.consume(T![,]);
            }
        }
        self.consume(T![')']);
        Ok(Pattern::Tuple(TuplePattern { items }))
    }

    fn full_map_pattern(&mut self) -> Result<Pattern> {
        self.consume(T!['{']);
        let mut items = vec![];
        while !self.at(T!['}']) {
            let key = self.full_pattern()?;
            self.consume(T![:]);
            let value = self.full_pattern()?;
            items.push((key, value));
            if !self.at(T!['}']) {
                self.consume(T![,]);
            }
        }
        self.consume(T!['}']);
        Ok(Pattern::Map(MapPattern { items }))
    }

    fn full_record_pattern(&mut self) -> Result<Pattern> {
        self.consume(T!['{']);
        let mut items = vec![];
        while !self.at(T!['}']) {
            let key = self.ident()?;
            self.consume(T![:]);
            let value = self.full_pattern()?;
            items.push((key, value));
            if !self.at(T!['}']) {
                self.consume(T![,]);
            }
        }
        self.consume(T!['}']);
        Ok(Pattern::Record(RecordPattern { items }))
    }

    fn pattern(&mut self) -> Result<Pattern> {
        let tok = self.peek();
        match tok.value {
            T![ident] => Ok(Pattern::Ident(self.ident()?)),
            T!['['] => self.list_pattern(),
            T!['('] => self.tuple_pattern(),
            T!['{'] => self.map_pattern(),
            _ => Err(ParserError(format!(
                "Unexpected token in pattern got `{:?}` - `{}`",
                self.peek(),
                self.text(tok)
            ))),
        }
    }

    fn list_pattern(&mut self) -> Result<Pattern> {
        self.consume(T!['[']);
        let mut items = vec![];
        while !self.at(T![']']) {
            items.push(self.pattern()?);
            if !self.at(T![']']) {
                self.consume(T![,]);
            }
        }
        self.consume(T![']']);
        Ok(Pattern::List(ListPattern { items }))
    }

    fn tuple_pattern(&mut self) -> Result<Pattern> {
        self.consume(T!['(']);
        if self.at(T![')']) {
            self.consume(T![')']);
            return Ok(Pattern::Unit);
        }
        let mut items = vec![];
        while !self.at(T![')']) {
            items.push(self.pattern()?);
            if !self.at(T![')']) {
                self.consume(T![,]);
            }
        }
        self.consume(T![')']);
        Ok(Pattern::Tuple(TuplePattern { items }))
    }

    fn map_pattern(&mut self) -> Result<Pattern> {
        self.consume(T!['{']);
        let mut items = vec![];
        while !self.at(T!['}']) {
            let key = self.pattern()?;
            self.consume(T![:]);
            let value = self.pattern()?;
            items.push((key, value));
            if !self.at(T!['}']) {
                self.consume(T![,]);
            }
        }
        self.consume(T!['}']);
        Ok(Pattern::Map(MapPattern { items }))
    }

    fn lit(&mut self) -> Result<Lit> {
        match self.peek().value {
            T![int] => Ok(Lit::Int(self.int()?)),
            T![real] => Ok(Lit::Real(self.real()?)),
            T![str] => Ok(Lit::String(self.string()?)),
            T![char] => Ok(Lit::Char(self.char()?)),
            T![bool] => Ok(Lit::Bool(self.bool()?)),
            T![lambda] => Ok(Lit::Lambda(self.lambda()?)),
            T!['['] => Ok(Lit::List(self.list()?)),
            T!['('] => Ok(Lit::Tuple(self.tuple()?)),
            T!['{'] => Ok(Lit::Map(self.map()?)),
            _ => Err(ParserError(format!("Unexpected token: {:?}", self.peek()))),
        }
    }

    fn int(&mut self) -> Result<Int> {
        let token = self.next();
        let text = self.text(token);
        Ok(Int::from(text.to_string().try_into()?))
    }

    fn real(&mut self) -> Result<Real> {
        let token = self.next();
        let text = self.text(token);
        Ok(Real::from(text.to_string()))
    }

    fn string(&mut self) -> Result<InternedString> {
        let token = self.next();
        let text = self.text(token);
        Ok(InternedString::from(&text[1..(text.len() - 1)]))
    }

    fn char(&mut self) -> Result<char> {
        let token = self.next();
        let text = self.text(token);
        Ok(text
            .chars()
            .nth(1)
            .ok_or(ParserError::new("Invalid character literal"))?)
    }

    fn bool(&mut self) -> Result<bool> {
        let token = self.next();
        let text = self.text(token);
        text.parse()
            .map_err(|_| ParserError(format!("Invalid boolean literal: {}", text)))
    }

    fn lambda(&mut self) -> Result<Lambda> {
        self.consume(T![lambda]);
        let mut params = vec![];
        while !self.at(T![->]) {
            params.push(self.pattern()?);
        }

        self.consume(T![->]);

        let body = self.expr()?;

        self.curry_fn(params, body)
    }

    fn list(&mut self) -> Result<List<Expr>> {
        self.consume(T!['[']);
        let mut items = vec![];
        while !self.at(T![']']) {
            items.push(self.expr()?);
            if !self.at(T![']']) {
                self.consume(T![,]);
            }
        }
        self.consume(T![']']);
        Ok(items.into())
    }

    fn tuple(&mut self) -> Result<Tuple> {
        self.consume(T!['(']);
        let items = self.tuple_items()?;
        self.consume(T![')']);
        Ok(Tuple { items })
    }

    fn tuple_items(&mut self) -> Result<Vec<Expr>> {
        let mut items = vec![];
        while !self.at(T![')']) {
            items.push(self.expr()?);
            if !self.at(T![')']) {
                self.consume(T![,]);
            }
        }
        Ok(items)
    }

    fn map(&mut self) -> Result<Map> {
        self.consume(T!['{']);
        let mut items = HashMap::new();
        while !self.at(T!['}']) {
            let key = self.ident()?;
            self.consume(T![:]);
            let value = self.expr()?;
            items.insert(key, value);
            if !self.at(T!['}']) {
                self.consume(T![,]);
            }
        }
        self.consume(T!['}']);
        Ok(Map { items })
    }

    fn ident(&mut self) -> Result<InternedString> {
        let token = self.next();
        let text = self.text(token);
        Ok(InternedString::from(text))
    }

    fn curry_fn(&mut self, params: Vec<Pattern>, body: Expr) -> Result<Lambda> {
        let last = params.first().ok_or(ParserError(
            "Cannot create a lambda with no parameters".to_string(),
        ))?;
        let iter = params.clone().into_iter().rev().filter(|p| p != last);
        let b = iter.fold(body, |acc, p| {
            Expr::Lit(Lit::Lambda(Lambda {
                param: p,
                body: Box::new(acc),
            }))
        });
        Ok(Lambda {
            param: last.clone(),
            body: Box::new(b),
        })
    }

    fn curry_apply(&mut self, args: Vec<Expr>, func: Expr) -> Expr {
        args.into_iter()
            .fold(func, |acc, arg| Expr::Apply(Box::new(acc), Box::new(arg)))
    }
}
