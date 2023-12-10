use std::fmt::Debug;

pub trait Parser: Debug + Clone {
    // fn parse(&self) -> Node;
}

// pub trait Lexer<T: Token>: Debug + Clone + Iterator<Item = T> {
//     fn next(&self) -> T;
// }

// pub trait Token: Debug + Clone {
//     fn get_type(&self) -> TokenType;
// }

// pub trait TokenType: Debug + Clone {}
