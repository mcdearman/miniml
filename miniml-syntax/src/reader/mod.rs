use self::{
    sexpr::{Atom, Root, Sexpr},
    token::Token,
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

    fn next(&mut self) -> Option<Token> {
        self.lexer.next()
    }

    fn peek(&mut self) -> Option<Token> {
        self.lexer.peek()
    }

    pub fn read(&mut self) -> ReadResult<Root> {
        let mut sexprs = Vec::new();
        while self.peek().is_some() {
            sexprs.push(self.read_sexpr()?);
        }
        let start = sexprs.first().map(|s| s.span().start()).unwrap_or(0);
        let end = sexprs.last().map(|s| s.span().end()).unwrap_or(0);
        Ok(Root::new(sexprs, (start..end).into()))
    }

    fn read_sexpr(&mut self) -> ReadResult<Sexpr> {
        todo!()
    }

    fn read_atom(&mut self) -> ReadResult<Atom> {
        todo!()
    }
}
