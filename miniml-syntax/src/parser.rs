use crate::{
    ast::{Decl, Expr, InfixOp, Lit, PrefixOp, Root},
    error::SyntaxError,
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

    pub fn parse(&mut self) -> Result<Spanned<Root>, Vec<Spanned<SyntaxError>>> {
        self.root()
    }

    fn root(&mut self) -> Result<Spanned<Root>, Vec<Spanned<SyntaxError>>> {
        let mut errors = Vec::new();
        let mut decls = Vec::new();
        let start = self.tokens.peek().1.start;

        while !self.tokens.at(&Token::Eof) {
            match self.decl() {
                Ok(d) => decls.push(d),
                Err(err) => errors.push(err),
            }
        }

        let end = self.tokens.peek().1.end;
        if errors.is_empty() {
            Ok((Root { decls }, Span::new(start, end)))
        } else {
            Err(errors)
        }
    }

    fn decl(&mut self) -> Result<Spanned<Decl>, Spanned<SyntaxError>> {
        match self.tokens.peek().0 {
            Token::Let => self.let_(),
            Token::Fn => self.fn_(),
            _ => Err((
                SyntaxError::UnexpectedToken(self.tokens.peek().0.clone()),
                self.tokens.peek().1.clone(),
            )),
        }
    }

    fn let_(&mut self) -> Result<Spanned<Decl>, Spanned<SyntaxError>> {
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
        self.term()
    }

    fn term(&mut self) -> Result<Spanned<Expr>, Spanned<SyntaxError>> {
        let start = self.tokens.peek().1.start;
        let lhs = self.factor()?;
        match self.tokens.peek().0 {
            Token::Add => {
                self.tokens.next();
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
                self.tokens.next();
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
                self.tokens.next();
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
                self.tokens.next();
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
            _ => Ok(lhs),
        }
    }

    fn power(&mut self) -> Result<Spanned<Expr>, Spanned<SyntaxError>> {
        let start = self.tokens.peek().1.start;
        let lhs = self.unary()?;
        match self.tokens.peek().0 {
            Token::Pow => {
                self.tokens.next();
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
                self.tokens.next();
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
                let mut args = vec![];
                while !self.tokens.at(&Token::Eof) {
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

    fn atom(&mut self) -> Result<Spanned<Expr>, Spanned<SyntaxError>> {
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
            _ => Err((
                SyntaxError::UnexpectedToken(self.tokens.peek().0.clone()),
                self.tokens.peek().1.clone(),
            )),
        }
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
            _ => Err((
                SyntaxError::UnexpectedToken(self.tokens.peek().0.clone()),
                self.tokens.peek().1.clone(),
            )),
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
