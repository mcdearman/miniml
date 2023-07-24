use crate::{
    ast::{Decl, Expr, InfixOp, Item, Let, LetDecl, LetExpr, PrefixOp, Root},
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
        let mut items = Vec::new();
        let start = self.tokens.peek().1.start;

        while !self.tokens.at(&Token::Eof) {
            match self.item() {
                Ok(item) => items.push(item),
                Err(err) => errors.push(err),
            }
        }

        let end = self.tokens.peek().1.end;
        if errors.is_empty() {
            Ok((Root { items }, Span::new(start, end)))
        } else {
            Err(errors)
        }
    }

    fn item(&mut self) -> Result<Spanned<Item>, Spanned<SyntaxError>> {
        let start = self.tokens.peek().1.start;
        match self.tokens.peek().0 {
            Token::Let => match self.let_()? {
                Let::Decl(decl) => Ok((
                    Item::Decl((Decl::Let(decl), Span::new(start, self.tokens.peek().1.end))),
                    Span::new(start, self.tokens.peek().1.end),
                )),
                Let::Expr(expr) => Ok((
                    Item::Expr((Expr::Let(expr), Span::new(start, self.tokens.peek().1.end))),
                    Span::new(start, self.tokens.peek().1.end),
                )),
            },
            _ => Ok((
                Item::Expr(self.expr()?),
                Span::new(start, self.tokens.peek().1.end),
            )),
        }
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
            Token::Int(i) => Ok((Expr::Int(i), Span::new(start, self.tokens.next().1.end))),
            Token::Real(r) => Ok((Expr::Real(r), Span::new(start, self.tokens.next().1.end))),
            Token::String(s) => {
                self.tokens.next();
                Ok((Expr::String(s), Span::new(start, self.tokens.peek().1.end)))
            }
            _ => Err((
                SyntaxError::UnexpectedToken(self.tokens.peek().0.clone()),
                self.tokens.peek().1.clone(),
            )),
        }
    }

    fn let_(&mut self) -> Result<Let, Spanned<SyntaxError>> {
        let start = self.tokens.peek().1.start;
        self.tokens.eat(&Token::Let)?;
        let name = self.ident()?;
        self.tokens.eat(&Token::Eq)?;
        let expr = self.expr()?;

        if self.tokens.at(&Token::In) {
            self.tokens.eat(&Token::In)?;
            let body = self.expr()?;
            Ok(Let::Expr((
                LetExpr {
                    name,
                    expr: Box::new(expr),
                    body: Box::new(body),
                },
                Span::new(start, self.tokens.peek().1.end),
            )))
        } else {
            Ok(Let::Decl((
                LetDecl {
                    name,
                    expr: Box::new(expr),
                },
                Span::new(start, self.tokens.peek().1.end),
            )))
        }
    }

    fn ident(&mut self) -> Result<Spanned<InternedString>, Spanned<SyntaxError>> {
        let start = self.tokens.peek().1.start;
        match self.tokens.peek().0 {
            Token::Ident(name) => {
                self.tokens.next();
                Ok((name, Span::new(start, self.tokens.peek().1.end)))
            }
            _ => Err((
                SyntaxError::UnexpectedToken(self.tokens.peek().0.clone()),
                self.tokens.peek().1.clone(),
            )),
        }
    }
}
