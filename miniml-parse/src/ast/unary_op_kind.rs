use miniml_lex::token::Token;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum UnaryOpKind {
    Neg,
    Not,
}

impl From<Token> for UnaryOpKind {
    fn from(t: Token) -> Self {
        match t {
            Token::Minus => Self::Neg,
            Token::Bang => Self::Not,
            _ => unreachable!(),
        }
    }
}

impl ToString for UnaryOpKind {
    fn to_string(&self) -> String {
        match self {
            Self::Neg => "-".to_string(),
            Self::Not => "!".to_string(),
        }
    }
}
