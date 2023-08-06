use crate::{
    ast::{Decl, Expr, InfixOp, Lit, PrefixOp, Root},
    error::{ParseResult, SyntaxError},
    lex::{Token, TokenStream},
};
use miniml_util::{
    intern::InternedString,
    span::{Span, Spanned},
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
        let start = self.tokens.peek().1.start;

        while !self.tokens.at(&Token::Eof) {
            log::trace!("decls: {:?}", decls);
            match self.decl() {
                Ok(d) => decls.push(d),
                Err(err) => {
                    errors.push(err);
                    while !(self.tokens.at(&Token::Fn)
                        || self.tokens.at(&Token::Let)
                        || self.tokens.at(&Token::Const)
                        || self.tokens.at(&Token::Eof))
                    {
                        self.tokens.next();
                    }
                }
            }
        }

        let end = self.tokens.peek().1.end;
        ((Root { decls }, Span::new(start, end)), errors)
    }

    fn decl(&mut self) -> Result<Spanned<Decl>, Spanned<SyntaxError>> {
        log::trace!("decl: {:?}", self.tokens.peek());
        match self.tokens.peek().0 {
            Token::Const => self.const_(),
            Token::Let => self.let_(),
            Token::Fn => self.fn_(),
            _ => {
                let next = self.tokens.next();
                log::trace!("unexpected token in decl: {:?}", next);
                Err((SyntaxError::UnexpectedToken(next.0.clone()), next.1.clone()))
            }
        }
    }

    fn const_(&mut self) -> Result<Spanned<Decl>, Spanned<SyntaxError>> {
        log::trace!("const: {:?}", self.tokens.peek());
        let start = self.tokens.peek().1;
        self.tokens.eat(&Token::Const)?;
        let name = self.ident()?;
        self.tokens.eat(&Token::Eq)?;
        let expr = self.expr()?;
        let span = start.extend(self.tokens.peek().1);
        Ok(Decl::Const {
            name,
            expr: Box::new(expr),
        }
        .spanned(span))
    }

    fn let_(&mut self) -> Result<Spanned<Decl>, Spanned<SyntaxError>> {
        log::trace!("let: {:?}", self.tokens.peek());
        let start = self.tokens.peek().1;
        self.tokens.eat(&Token::Let)?;
        let name = self.ident()?;
        self.tokens.eat(&Token::Eq)?;
        let expr = self.expr()?;
        let span = start.extend(self.tokens.peek().1);
        Ok(Decl::Let {
            name,
            expr: Box::new(expr),
        }
        .spanned(span))
    }

    fn fn_(&mut self) -> Result<Spanned<Decl>, Spanned<SyntaxError>> {
        let start = self.tokens.peek().1;
        self.tokens.eat(&Token::Fn)?;
        let name = self.ident()?;
        let mut params = vec![];
        while !self.tokens.at(&Token::Eq) {
            params.push(self.ident()?);
        }
        self.tokens.eat(&Token::Eq)?;
        let body = self.expr()?;
        let span = start.extend(self.tokens.peek().1);
        Ok(Decl::Fn {
            name,
            params,
            body: Box::new(body),
        }
        .spanned(span))
    }

    fn expr(&mut self) -> Result<Spanned<Expr>, Spanned<SyntaxError>> {
        self.cmp()
    }

    // <cmp> ::= <term> (<ws>* ("<" | ">" | "<=" | ">=" | "=" | "!=") <ws>* <cmp>)?
    fn cmp(&mut self) -> ParseResult<Expr> {
        let start = self.tokens.peek().1.start;
        let lhs = self.term()?;
        match self.tokens.peek().0 {
            Token::Lss => {
                log::trace!("cmp: {:?}", self.tokens.peek());
                self.tokens.eat(&Token::Lss)?;
                let rhs = self.cmp()?;
                Ok((
                    Expr::Infix {
                        op: (
                            InfixOp::Lss,
                            Span::new(lhs.1.end + 1, self.tokens.peek().1.start),
                        ),
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    },
                    Span::new(start, self.tokens.peek().1.end),
                ))
            }
            Token::Gtr => {
                log::trace!("cmp: {:?}", self.tokens.peek());
                self.tokens.eat(&Token::Gtr)?;
                let rhs = self.cmp()?;
                Ok((
                    Expr::Infix {
                        op: (
                            InfixOp::Gtr,
                            Span::new(lhs.1.end + 1, self.tokens.peek().1.start),
                        ),
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    },
                    Span::new(start, self.tokens.peek().1.end),
                ))
            }
            Token::Leq => {
                log::trace!("cmp: {:?}", self.tokens.peek());
                self.tokens.eat(&Token::Leq)?;
                let rhs = self.cmp()?;
                Ok((
                    Expr::Infix {
                        op: (
                            InfixOp::Leq,
                            Span::new(lhs.1.end + 1, self.tokens.peek().1.start),
                        ),
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    },
                    Span::new(start, self.tokens.peek().1.end),
                ))
            }
            Token::Geq => {
                log::trace!("cmp: {:?}", self.tokens.peek());
                self.tokens.eat(&Token::Geq)?;
                let rhs = self.cmp()?;
                Ok((
                    Expr::Infix {
                        op: (
                            InfixOp::Geq,
                            Span::new(lhs.1.end + 1, self.tokens.peek().1.start),
                        ),
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    },
                    Span::new(start, self.tokens.peek().1.end),
                ))
            }
            Token::Eq => {
                log::trace!("cmp: {:?}", self.tokens.peek());
                self.tokens.eat(&Token::Eq)?;
                let rhs = self.cmp()?;
                Ok((
                    Expr::Infix {
                        op: (
                            InfixOp::Eq,
                            Span::new(lhs.1.end + 1, self.tokens.peek().1.start),
                        ),
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    },
                    Span::new(start, self.tokens.peek().1.end),
                ))
            }
            Token::Neq => {
                log::trace!("cmp: {:?}", self.tokens.peek());
                self.tokens.eat(&Token::Neq)?;
                let rhs = self.cmp()?;
                Ok((
                    Expr::Infix {
                        op: (
                            InfixOp::Neq,
                            Span::new(lhs.1.end + 1, self.tokens.peek().1.start),
                        ),
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    },
                    Span::new(start, self.tokens.peek().1.end),
                ))
            }
            _ => Ok(lhs),
        }
    }

    fn term(&mut self) -> ParseResult<Expr> {
        let start = self.tokens.peek().1.start;
        let lhs = self.factor()?;
        match self.tokens.peek().0 {
            Token::Add => {
                log::trace!("term: {:?}", self.tokens.peek());
                self.tokens.eat(&Token::Add)?;
                let rhs = self.term()?;
                Ok((
                    Expr::Infix {
                        op: (
                            InfixOp::Add,
                            Span::new(lhs.1.end + 1, self.tokens.peek().1.start),
                        ),
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    },
                    Span::new(start, self.tokens.peek().1.end),
                ))
            }
            Token::Sub => {
                log::trace!("term: {:?}", self.tokens.peek());
                self.tokens.eat(&Token::Sub)?;
                let rhs = self.term()?;
                Ok((
                    Expr::Infix {
                        op: (
                            InfixOp::Sub,
                            Span::new(lhs.1.end + 1, self.tokens.peek().1.start),
                        ),
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    },
                    Span::new(start, self.tokens.peek().1.end),
                ))
            }
            _ => Ok(lhs),
        }
    }

    fn factor(&mut self) -> Result<Spanned<Expr>, Spanned<SyntaxError>> {
        let start = self.tokens.peek().1.start;
        let lhs = self.power()?;
        match self.tokens.peek().0 {
            Token::Mul => {
                log::trace!("factor: {:?}", self.tokens.peek());
                self.tokens.eat(&Token::Mul)?;
                let rhs = self.factor()?;
                Ok((
                    Expr::Infix {
                        op: (
                            InfixOp::Mul,
                            Span::new(lhs.1.end + 1, self.tokens.peek().1.start),
                        ),
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    },
                    Span::new(start, self.tokens.peek().1.end),
                ))
            }
            Token::Div => {
                log::trace!("factor: {:?}", self.tokens.peek());
                self.tokens.eat(&Token::Div)?;
                let rhs = self.factor()?;
                Ok((
                    Expr::Infix {
                        op: (
                            InfixOp::Div,
                            Span::new(lhs.1.end, self.tokens.peek().1.start),
                        ),
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    },
                    Span::new(start, self.tokens.peek().1.end),
                ))
            }
            Token::Rem => {
                log::trace!("factor: {:?}", self.tokens.peek());
                self.tokens.eat(&Token::Rem)?;
                let rhs = self.factor()?;
                Ok((
                    Expr::Infix {
                        op: (
                            InfixOp::Mod,
                            Span::new(lhs.1.end, self.tokens.peek().1.start),
                        ),
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    },
                    Span::new(start, self.tokens.peek().1.end),
                ))
            }
            _ => Ok(lhs),
        }
    }

    fn power(&mut self) -> Result<Spanned<Expr>, Spanned<SyntaxError>> {
        let start = self.tokens.peek().1.start;
        let lhs = self.unary()?;
        match self.tokens.peek().0 {
            Token::Pow => {
                log::trace!("power: {:?}", self.tokens.peek());
                self.tokens.eat(&Token::Pow)?;
                let rhs = self.power()?;
                Ok((
                    Expr::Infix {
                        op: (
                            InfixOp::Pow,
                            Span::new(lhs.1.end, self.tokens.peek().1.start),
                        ),
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    },
                    Span::new(start, self.tokens.peek().1.end),
                ))
            }
            _ => Ok(lhs),
        }
    }

    fn unary(&mut self) -> Result<Spanned<Expr>, Spanned<SyntaxError>> {
        let start = self.tokens.peek().1.start;
        match self.tokens.peek().0 {
            Token::Sub => {
                log::trace!("unary: {:?}", self.tokens.peek());
                self.tokens.eat(&Token::Sub)?;
                Ok((
                    Expr::Prefix {
                        op: (PrefixOp::Neg, Span::new(start, self.tokens.peek().1.start)),
                        expr: Box::new(self.unary()?),
                    },
                    Span::new(start, self.tokens.peek().1.end),
                ))
            }
            _ => self.apply(),
        }
    }

    fn apply(&mut self) -> Result<Spanned<Expr>, Spanned<SyntaxError>> {
        let start = self.tokens.peek().1.start;
        let call = self.atom()?;
        match self.tokens.peek().0 {
            Token::Int(_) | Token::Real(_) | Token::String(_) | Token::Ident(_) => {
                log::trace!("apply: {:?}", self.tokens.peek());
                let mut args = vec![];
                while !(self.tokens.at(&Token::Let)
                    || self.tokens.at(&Token::Fn)
                    || self.tokens.at(&Token::Const)
                    || self.tokens.at(&Token::Eof))
                {
                    match self.atom()? {
                        arg => args.push(arg),
                    }
                }
                Ok((
                    Expr::Apply {
                        fun: Box::new(call),
                        args,
                    },
                    Span::new(start, self.tokens.peek().1.end),
                ))
            }
            _ => Ok(call),
        }
    }

    fn atom(&mut self) -> ParseResult<Expr> {
        log::trace!("atom: {:?}", self.tokens.peek());
        let start = self.tokens.peek().1.start;
        match self.tokens.peek().0 {
            Token::Ident(name) => {
                self.tokens.next();
                Ok((
                    Expr::Ident(name),
                    Span::new(start, self.tokens.peek().1.end),
                ))
            }
            Token::Int(_) | Token::Real(_) | Token::String(_) => {
                let (lit, span) = self.lit()?;
                Ok(Expr::Lit(lit).spanned(span))
            }
            Token::If => self.if_(),
            Token::LParen => {
                self.tokens.eat(&Token::LParen)?;
                let expr = self.expr()?;
                self.tokens.eat(&Token::RParen)?;
                log::trace!("paren atom expr: {:?}", expr);
                log::trace!("paren atom: {:?}", self.tokens.peek());
                Ok(expr)
            }
            _ => {
                let next = self.tokens.next();
                log::trace!("unexpected token in atom: {:?}", next);
                Err((SyntaxError::UnexpectedToken(next.0.clone()), next.1.clone()))
            }
        }
    }

    //  <if> ::= "if" <ws>+ <expr> <ws>+ "then" <ws>+ <expr>
    //  ("elif" <ws>+ <expr> "then" <expr>)* <ws>+ "else" <ws>+ <expr>
    fn if_(&mut self) -> Result<Spanned<Expr>, Spanned<SyntaxError>> {
        log::trace!("if: {:?}", self.tokens.peek());
        let start = self.tokens.peek().1;
        self.tokens.eat(&Token::If)?;
        let cond = self.expr()?;
        self.tokens.eat(&Token::Then)?;
        let then = self.expr()?;
        let mut elifs = vec![];
        while self.tokens.at(&Token::Elif) {
            self.tokens.eat(&Token::Elif)?;
            let cond = self.expr()?;
            self.tokens.eat(&Token::Then)?;
            let then = self.expr()?;
            elifs.push((cond, then));
        }
        self.tokens.eat(&Token::Else)?;
        let else_ = self.expr()?;
        let top_else = elifs.into_iter().fold(else_, |else_, (cond, then)| {
            Expr::If {
                cond: Box::new(cond),
                then: Box::new(then.clone()),
                else_: Box::new(else_),
            }
            .spanned(Span::new(start.start, then.1.end))
        });
        let span = start.extend(self.tokens.peek().1);
        Ok((
            Expr::If {
                cond: Box::new(cond),
                then: Box::new(then),
                else_: Box::new(top_else),
            },
            span,
        ))
    }

    fn lit(&mut self) -> Result<Spanned<Lit>, Spanned<SyntaxError>> {
        let span = self.tokens.peek().1.clone();
        match self.tokens.peek().0 {
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
                Err((SyntaxError::UnexpectedToken(next.0.clone()), next.1.clone()))
            }
        }
    }

    fn ident(&mut self) -> Result<Spanned<InternedString>, Spanned<SyntaxError>> {
        let span = self.tokens.peek().1.clone();
        match self.tokens.peek().0 {
            Token::Ident(name) => {
                self.tokens.next();
                Ok(name.spanned(span))
            }
            _ => Err((
                SyntaxError::UnexpectedToken(self.tokens.peek().0.clone()),
                self.tokens.peek().1.clone(),
            )),
        }
    }
}
