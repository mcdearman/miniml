use crate::lex::token::Token;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BinaryOpKind {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Pow,
    Eq,
    Neq,
    Lt,
    Lte,
    Gt,
    Gte,
    And,
    Or,
}

impl From<Token> for BinaryOpKind {
    fn from(t: Token) -> Self {
        match t {
            Token::Plus => Self::Add,
            Token::Minus => Self::Sub,
            Token::Star => Self::Mul,
            Token::Slash => Self::Div,
            Token::Percent => Self::Rem,
            Token::Caret => Self::Pow,
            Token::Neq => Self::Neq,
            Token::Lt => Self::Lt,
            Token::Leq => Self::Lte,
            Token::Gt => Self::Gt,
            Token::Geq => Self::Gte,
            Token::And => Self::And,
            Token::Or => Self::Or,
            _ => unreachable!(),
        }
    }
}

impl ToString for BinaryOpKind {
    fn to_string(&self) -> String {
        match self {
            Self::Add => "+".to_string(),
            Self::Sub => "-".to_string(),
            Self::Mul => "*".to_string(),
            Self::Div => "/".to_string(),
            Self::Rem => "%".to_string(),
            Self::Pow => "^".to_string(),
            Self::Eq => "==".to_string(),
            Self::Neq => "!=".to_string(),
            Self::Lt => "<".to_string(),
            Self::Lte => "<=".to_string(),
            Self::Gt => ">".to_string(),
            Self::Gte => ">=".to_string(),
            Self::And => "&&".to_string(),
            Self::Or => "||".to_string(),
        }
    }
}
