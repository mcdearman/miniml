use crate::{
    ast::{Decl, Expr, InfixOp, Lit, PrefixOp, Root},
    error::{ParseResult, SyntaxError},
    lex::{Token, TokenStream},
};
use miniml_util::{
    intern::InternedString,
    span::{Span, Spannable, Spanned},
};

#[derive(Debug)]
pub struct Parser {
    tokens: TokenStream,
}

impl Parser {
    pub fn new(tokens: TokenStream) -> Self {
        Self { tokens }
    }

    pub fn parse(&mut self) -> (Spanned<Root>, Vec<Spanned<SyntaxError>>) {
        self.root()
    }

    fn root(&mut self) -> (Spanned<Root>, Vec<Spanned<SyntaxError>>) {
        let mut errors = Vec::new();
        let mut decls = Vec::new();
        let start = self.tokens.peek().span;

        while !self.tokens.at(Token::Eof) {
            log::trace!("decls: {:?}", decls);
            match self.decl() {
                Ok(d) => decls.push(d),
                Err(err) => {
                    errors.push(err);
                    while !(self.tokens.at(Token::Fn)
                        || self.tokens.at(Token::Let)
                        || self.tokens.at(Token::Const)
                        || self.tokens.at(Token::Eof))
                    {
                        self.tokens.next();
                    }
                }
            }
        }

        (
            Root { decls }.spanned(start.extend(self.tokens.peek().span)),
            errors,
        )
    }

    fn decl(&mut self) -> Result<Spanned<Decl>, Spanned<SyntaxError>> {
        log::trace!("decl: {:?}", self.tokens.peek());
        match self.tokens.peek().value {
            Token::Const => self.const_(),
            Token::Let => self.let_(),
            Token::Fn => self.fn_(),
            _ => {
                let next @ Spanned { value, span } = &self.tokens.next();
                log::trace!("unexpected token in decl: {:?}", next);
                Err(SyntaxError::UnexpectedToken(value.clone()).spanned(span.clone()))
            }
        }
    }

    fn const_(&mut self) -> Result<Spanned<Decl>, Spanned<SyntaxError>> {
        log::trace!("const: {:?}", self.tokens.peek());
        let start = self.tokens.peek().span;
        self.tokens.eat(Token::Const)?;
        let name = self.ident()?;
        self.tokens.eat(Token::Eq)?;
        let expr = self.expr()?;
        Ok(Decl::Const {
            name,
            expr: Box::new(expr),
        }
        .spanned(start.extend(self.tokens.peek().span)))
    }

    fn let_(&mut self) -> Result<Spanned<Decl>, Spanned<SyntaxError>> {
        log::trace!("let: {:?}", self.tokens.peek());
        let start = self.tokens.peek().span;
        self.tokens.eat(Token::Let)?;
        let name = self.ident()?;
        self.tokens.eat(Token::Eq)?;
        let expr = self.expr()?;
        Ok(Decl::Let {
            name,
            expr: Box::new(expr),
        }
        .spanned(start.extend(self.tokens.peek().span)))
    }

    fn fn_(&mut self) -> Result<Spanned<Decl>, Spanned<SyntaxError>> {
        let start = self.tokens.peek().span;
        self.tokens.eat(Token::Fn)?;
        let name = self.ident()?;
        let mut params = vec![];
        while !self.tokens.at(Token::Eq) {
            params.push(self.ident()?);
        }
        self.tokens.eat(Token::Eq)?;
        let body = self.expr()?;
        Ok(Decl::Fn {
            name,
            params,
            body: Box::new(body),
        }
        .spanned(start.extend(self.tokens.peek().span)))
    }

    fn expr(&mut self) -> Result<Spanned<Expr>, Spanned<SyntaxError>> {
        self.pipe()
    }

    // <pipe> ::= <stmt> (<ws>* "|>" <ws>* <pipe>)?
    fn pipe(&mut self) -> ParseResult<Expr> {
        let start = self.tokens.peek().span;
        let lhs = self.stmt()?;
        match self.tokens.peek().value {
            Token::PipeArrow => {
                log::trace!("pipe: {:?}", self.tokens.peek());
                let op_span = self.tokens.peek().span;
                self.tokens.eat(Token::PipeArrow)?;
                let rhs = self.pipe()?;
                Ok(Expr::Infix {
                    op: InfixOp::Pipe.spanned(op_span),
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                }
                .spanned(start.extend(self.tokens.peek().span)))
            }
            _ => Ok(lhs),
        }
    }

    // <stmt> ::= <or> (<ws>* ";" <ws>* <stmt>)?
    fn stmt(&mut self) -> ParseResult<Expr> {
        let start = self.tokens.peek().span;
        let lhs = self.or()?;
        match self.tokens.peek().value {
            Token::Semicolon => {
                log::trace!("stmt: {:?}", self.tokens.peek());
                self.tokens.eat(Token::Semicolon)?;
                let rhs = self.stmt()?;
                Ok(Expr::Let {
                    name: InternedString::from("_").spanned(start.extend(self.tokens.peek().span)),
                    expr: Box::new(lhs),
                    body: Box::new(rhs),
                }
                .spanned(start.extend(self.tokens.peek().span)))
            }
            _ => Ok(lhs),
        }
    }

    // <or> ::= <and> (<ws>* "or" <ws>* <or>)?
    fn or(&mut self) -> ParseResult<Expr> {
        let start = self.tokens.peek().span;
        let lhs = self.and()?;
        match self.tokens.peek().value {
            Token::Or => {
                log::trace!("or: {:?}", self.tokens.peek());
                let op_span = self.tokens.peek().span;
                self.tokens.eat(Token::Or)?;
                let rhs = self.or()?;
                Ok(Expr::Infix {
                    op: InfixOp::Or.spanned(op_span),
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                }
                .spanned(start.extend(self.tokens.peek().span)))
            }
            _ => Ok(lhs),
        }
    }

    // <and> ::= <cmp> (<ws>* "and" <ws>* <and>)?
    fn and(&mut self) -> ParseResult<Expr> {
        let start = self.tokens.peek().span;
        let lhs = self.cmp()?;
        match self.tokens.peek().value {
            Token::And => {
                log::trace!("and: {:?}", self.tokens.peek());
                let op_span = self.tokens.peek().span;
                self.tokens.eat(Token::And)?;
                let rhs = self.and()?;
                Ok(Expr::Infix {
                    op: InfixOp::And.spanned(op_span),
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                }
                .spanned(start.extend(self.tokens.peek().span)))
            }
            _ => Ok(lhs),
        }
    }

    // <cmp> ::= <term> (<ws>* ("<" | ">" | "<=" | ">=" | "=" | "!=") <ws>* <cmp>)?
    fn cmp(&mut self) -> ParseResult<Expr> {
        let start = self.tokens.peek().span;
        let lhs = self.term()?;
        match self.tokens.peek().value {
            Token::Lss => {
                log::trace!("cmp: {:?}", self.tokens.peek());
                let op_span = self.tokens.peek().span;
                self.tokens.eat(Token::Lss)?;
                let rhs = self.cmp()?;
                Ok(Expr::Infix {
                    op: InfixOp::Lss.spanned(op_span),
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                }
                .spanned(start.extend(self.tokens.peek().span)))
            }
            Token::Gtr => {
                log::trace!("cmp: {:?}", self.tokens.peek());
                let op_span = self.tokens.peek().span;
                self.tokens.eat(Token::Gtr)?;
                let rhs = self.cmp()?;
                Ok(Expr::Infix {
                    op: InfixOp::Gtr.spanned(op_span),
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                }
                .spanned(start.extend(self.tokens.peek().span)))
            }
            Token::Leq => {
                log::trace!("cmp: {:?}", self.tokens.peek());
                let op_span = self.tokens.peek().span;
                self.tokens.eat(Token::Leq)?;
                let rhs = self.cmp()?;
                Ok(Expr::Infix {
                    op: InfixOp::Leq.spanned(op_span),
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                }
                .spanned(start.extend(self.tokens.peek().span)))
            }
            Token::Geq => {
                log::trace!("cmp: {:?}", self.tokens.peek());
                let op_span = self.tokens.peek().span;
                self.tokens.eat(Token::Geq)?;
                let rhs = self.cmp()?;
                Ok(Expr::Infix {
                    op: InfixOp::Geq.spanned(op_span),
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                }
                .spanned(start.extend(self.tokens.peek().span)))
            }
            Token::Eq => {
                log::trace!("cmp: {:?}", self.tokens.peek());
                let op_span = self.tokens.peek().span;
                self.tokens.eat(Token::Eq)?;
                let rhs = self.cmp()?;
                Ok(Expr::Infix {
                    op: InfixOp::Eq.spanned(op_span),
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                }
                .spanned(start.extend(self.tokens.peek().span)))
            }
            Token::Neq => {
                log::trace!("cmp: {:?}", self.tokens.peek());
                let op_span = self.tokens.peek().span;
                self.tokens.eat(Token::Neq)?;
                let rhs = self.cmp()?;
                Ok(Expr::Infix {
                    op: InfixOp::Neq.spanned(op_span),
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                }
                .spanned(start.extend(self.tokens.peek().span)))
            }
            _ => Ok(lhs),
        }
    }

    fn term(&mut self) -> ParseResult<Expr> {
        let start = self.tokens.peek().span;
        let lhs = self.factor()?;
        match self.tokens.peek().value {
            Token::Add => {
                log::trace!("term: {:?}", self.tokens.peek());
                let op_span = self.tokens.peek().span;
                self.tokens.eat(Token::Add)?;
                let rhs = self.term()?;
                Ok(Expr::Infix {
                    op: InfixOp::Add.spanned(op_span),
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                }
                .spanned(start.extend(self.tokens.peek().span)))
            }
            Token::Sub => {
                log::trace!("term: {:?}", self.tokens.peek());
                let op_span = self.tokens.peek().span;
                self.tokens.eat(Token::Sub)?;
                let rhs = self.term()?;
                Ok(Expr::Infix {
                    op: InfixOp::Sub.spanned(op_span),
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                }
                .spanned(start.extend(self.tokens.peek().span)))
            }
            _ => Ok(lhs),
        }
    }

    fn factor(&mut self) -> Result<Spanned<Expr>, Spanned<SyntaxError>> {
        let start = self.tokens.peek().span;
        let lhs = self.power()?;
        match self.tokens.peek().value {
            Token::Mul => {
                log::trace!("factor: {:?}", self.tokens.peek());
                let op_span = self.tokens.peek().span;
                self.tokens.eat(Token::Mul)?;
                let rhs = self.factor()?;
                Ok(Expr::Infix {
                    op: InfixOp::Mul.spanned(op_span),
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                }
                .spanned(start.extend(self.tokens.peek().span)))
            }
            Token::Div => {
                log::trace!("factor: {:?}", self.tokens.peek());
                let op_span = self.tokens.peek().span;
                self.tokens.eat(Token::Div)?;
                let rhs = self.factor()?;
                Ok(Expr::Infix {
                    op: InfixOp::Div.spanned(op_span),
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                }
                .spanned(start.extend(self.tokens.peek().span)))
            }
            Token::Rem => {
                log::trace!("factor: {:?}", self.tokens.peek());
                let op_span = self.tokens.peek().span;
                self.tokens.eat(Token::Rem)?;
                let rhs = self.factor()?;
                Ok(Expr::Infix {
                    op: InfixOp::Mod.spanned(op_span),
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                }
                .spanned(start.extend(self.tokens.peek().span)))
            }
            _ => Ok(lhs),
        }
    }

    fn power(&mut self) -> Result<Spanned<Expr>, Spanned<SyntaxError>> {
        let start = self.tokens.peek().span;
        let lhs = self.unary()?;
        match self.tokens.peek().value {
            Token::Pow => {
                log::trace!("power: {:?}", self.tokens.peek());
                let op_span = self.tokens.peek().span;
                self.tokens.eat(Token::Pow)?;
                let rhs = self.power()?;
                Ok(Expr::Infix {
                    op: InfixOp::Pow.spanned(op_span),
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                }
                .spanned(start.extend(self.tokens.peek().span)))
            }
            _ => Ok(lhs),
        }
    }

    fn unary(&mut self) -> Result<Spanned<Expr>, Spanned<SyntaxError>> {
        match self.tokens.peek().value {
            Token::Sub => {
                let start = self.tokens.peek().span;
                log::trace!("unary: {:?}", self.tokens.peek());
                self.tokens.eat(Token::Sub)?;
                Ok(Expr::Prefix {
                    op: PrefixOp::Neg.spanned(start),
                    expr: Box::new(self.unary()?),
                }
                .spanned(start.extend(self.tokens.peek().span)))
            }
            _ => self.apply(),
        }
    }

    // <apply> ::= <atom> (<ws>+ <atom>)*
    fn apply(&mut self) -> ParseResult<Expr> {
        let start = self.tokens.peek().span;
        let call = self.atom()?;
        let mut args = vec![];
        loop {
            match self.tokens.peek().value {
                Token::Int(_) | Token::Real(_) | Token::String(_) | Token::Ident(_) => {
                    log::trace!("apply: {:?}", self.tokens.peek());
                    args.push(self.atom()?);
                }
                _ => break,
            }
        }
        if args.is_empty() {
            Ok(call)
        } else {
            Ok(Expr::Apply {
                fun: Box::new(call),
                args,
            }
            .spanned(start.extend(self.tokens.peek().span)))
        }
    }

    fn atom(&mut self) -> ParseResult<Expr> {
        log::trace!("atom: {:?}", self.tokens.peek());
        let start = self.tokens.peek().span;
        match self.tokens.peek().value {
            Token::Ident(name) => {
                self.tokens.next();
                Ok(Expr::Ident(name).spanned(start.extend(self.tokens.peek().span)))
            }
            Token::Int(_) | Token::Real(_) | Token::String(_) => {
                let Spanned { value, span } = self.lit()?;
                Ok(Expr::Lit(value).spanned(span))
            }
            Token::If => self.if_(),
            Token::LParen => {
                self.tokens.eat(Token::LParen)?;
                if self.tokens.at(Token::RParen) {
                    return Ok(Expr::Unit.spanned(start.extend(self.tokens.peek().span)));
                }
                let expr = self.expr()?;
                self.tokens.eat(Token::RParen)?;
                log::trace!("paren atom expr: {:?}", expr);
                log::trace!("paren atom: {:?}", self.tokens.peek());
                Ok(expr)
            }
            _ => {
                let next @ Spanned { value, span } = &self.tokens.next();
                log::trace!("unexpected token in atom: {:?}", next);
                Err(SyntaxError::UnexpectedToken(value.clone()).spanned(span.clone()))
            }
        }
    }

    //  <if> ::= "if" <ws>+ <expr> <ws>+ "then" <ws>+ <expr>
    //  ("elif" <ws>+ <expr> "then" <expr>)* <ws>+ "else" <ws>+ <expr>
    fn if_(&mut self) -> Result<Spanned<Expr>, Spanned<SyntaxError>> {
        log::trace!("if: {:?}", self.tokens.peek());
        let start = self.tokens.peek().span;
        self.tokens.eat(Token::If)?;
        let cond = self.expr()?;
        self.tokens.eat(Token::Then)?;
        let then = self.expr()?;
        let mut elifs = vec![];
        while self.tokens.at(Token::Elif) {
            self.tokens.eat(Token::Elif)?;
            let cond = self.expr()?;
            self.tokens.eat(Token::Then)?;
            let then = self.expr()?;
            elifs.push((cond, then));
        }
        self.tokens.eat(Token::Else)?;
        let else_ = self.expr()?;
        let top_else = elifs.into_iter().fold(else_, |else_, (cond, then)| {
            Expr::If {
                cond: Box::new(cond.clone()),
                then: Box::new(then.clone()),
                else_: Box::new(else_),
            }
            .spanned(cond.span.extend(then.span))
        });
        Ok(Expr::If {
            cond: Box::new(cond),
            then: Box::new(then),
            else_: Box::new(top_else),
        }
        .spanned(start.extend(self.tokens.peek().span)))
    }

    fn lit(&mut self) -> Result<Spanned<Lit>, Spanned<SyntaxError>> {
        let span = self.tokens.peek().span.clone();
        match self.tokens.peek().value {
            Token::Int(i) => {
                self.tokens.next();
                Ok(Lit::Int(i).spanned(span))
            }
            Token::Real(r) => {
                self.tokens.next();
                Ok(Lit::Real(r).spanned(span))
            }
            Token::String(s) => {
                self.tokens.next();
                Ok(Lit::String(s).spanned(span))
            }
            _ => {
                let next = self.tokens.next();
                log::trace!("unexpected token in lit: {:?}", next);
                Err(SyntaxError::UnexpectedToken(next.value.clone()).spanned(span))
            }
        }
    }

    fn ident(&mut self) -> Result<Spanned<InternedString>, Spanned<SyntaxError>> {
        let span = self.tokens.peek().span.clone();
        match self.tokens.peek().value {
            Token::Ident(name) => {
                self.tokens.next();
                Ok(name.spanned(span))
            }
            _ => {
                let Spanned { value, span } = self.tokens.next();
                Err(SyntaxError::UnexpectedToken(value).spanned(span))
            }
        }
    }
}

mod tests {
    use super::Parser;
    use crate::{error, lex::lex};

    #[test]
    fn test_pipe() {
        let src = "foo 2 |> bar 1";
        let mut parser = Parser::new(lex(src).unwrap());
        let expr = parser.expr().expect("parse error");
        insta::assert_debug_snapshot!(expr);
    }

    #[test]
    fn test_stmt() {
        let src = "println 1; 2";
        let mut parser = Parser::new(lex(src).unwrap());
        let expr = parser.expr().expect("parse error");
        insta::assert_debug_snapshot!(expr);
    }
}
