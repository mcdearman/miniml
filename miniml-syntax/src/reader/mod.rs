use self::{
    sexpr::{Atom, Root, Sexpr, SexprKind},
    token::{Token, TokenKind},
};
use std::fmt::Display;

pub mod lexer;
pub mod sexpr;
pub mod token;

#[derive(Debug, Clone, PartialEq)]
pub enum ReaderError {
    UnexpectedEOF,
    UnexpectedToken(Token),
}

impl Display for ReaderError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ReaderError::UnexpectedEOF => write!(f, "unexpected EOF"),
            ReaderError::UnexpectedToken(token) => write!(f, "unexpected token: {}", token),
        }
    }
}

pub type ReadResult<T> = Result<T, Vec<ReaderError>>;

#[derive(Debug, Clone)]
pub struct Reader<'src> {
    src: &'src str,
    lexer: lexer::Lexer<'src>,
}

impl<'src> Reader<'src> {
    pub fn new(src: &'src str) -> Self {
        Self {
            src,
            lexer: lexer::Lexer::new(src),
        }
    }

    fn next(&mut self) -> Token {
        self.lexer.next()
    }

    fn peek(&mut self) -> Token {
        self.lexer.peek()
    }

    fn eat(&mut self, kind: TokenKind) -> ReadResult<()> {
        if self.lexer.eat(kind) {
            Ok(())
        } else {
            Err(vec![ReaderError::UnexpectedToken(self.lexer.peek())])
        }
    }

    pub fn read(&mut self) -> ReadResult<Root> {
        let mut sexprs = Vec::new();
        while *self.peek().kind() != TokenKind::Eof {
            sexprs.push(self.read_sexpr()?);
        }
        let start = sexprs.first().map(|s| s.span().start()).unwrap_or(0);
        let end = sexprs.last().map(|s| s.span().end()).unwrap_or(0);
        Ok(Root::new(sexprs, (start..end).into()))
    }

    fn read_sexpr(&mut self) -> ReadResult<Sexpr> {
        let tok = self.peek();
        match tok.kind() {
            TokenKind::LParen => self.read_list(),
            TokenKind::LBrack => self.read_vector(),
            TokenKind::LBrace => self.read_byte_vector(),
            TokenKind::Quote => self.read_quote(),
            TokenKind::Backquote => self.read_quasiquote(),
            TokenKind::Symbol(_) | TokenKind::Number(_) | TokenKind::String(_) => {
                let atom = self.read_atom();
                atom.map(|a| Sexpr::new(SexprKind::Atom(a.clone()), *a.span()))
            }
            TokenKind::Eof => Err(vec![ReaderError::UnexpectedEOF]),
            _ => Err(vec![ReaderError::UnexpectedToken(tok.clone())]),
        }
    }

    fn read_list(&mut self) -> ReadResult<Sexpr> {
        let start = self.peek().span();
        self.eat(TokenKind::LParen)?;
        let mut sexprs = Vec::new();
        while *self.peek().kind() != TokenKind::Eof  {
            if let Some(tok) = self.peek() {
                match tok.kind() {
                    TokenKind::RParen => {
                        self.eat(TokenKind::RParen)?;
                        break;
                    }
                    _ => {
                        let sexpr = self.read_sexpr()?;
                        sexprs.push(sexpr);
                    }
                }
            } else {
                return Err(vec![ReaderError::UnexpectedEOF]);
            }
        }
        let end = *self.peek().span();
        Ok(Sexpr::new(SexprKind::List(sexprs), start.extend(end)))
    }

    fn read_vector(&mut self) -> ReadResult<Sexpr> {
        todo!()
    }

    fn read_byte_vector(&mut self) -> ReadResult<Sexpr> {
        todo!()
    }

    fn read_quote(&mut self) -> ReadResult<Sexpr> {
        todo!()
    }

    fn read_quasiquote(&mut self) -> ReadResult<Sexpr> {
        todo!()
    }

    fn read_atom(&mut self) -> ReadResult<Atom> {}
}
