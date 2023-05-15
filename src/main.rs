use interpreter::{eval, Env, Lambda, Value};
use parser::{
    ast::{Expr, Item, Lit, Pattern},
    Parser,
};
use std::{cell::RefCell, rc::Rc};

mod compiler;
mod intern;
mod interpreter;
mod list;
mod parser;
mod vm;

fn main() {
    let input = "id \"Hello, World!\"";
    let ast = Parser::new(input).item().expect("failed to parse");
    let env = Rc::new(RefCell::new(Env::new()));
    env.borrow_mut().define(
        "id".into(),
        Value::Lambda(Lambda {
            env: env.clone(),
            param: Pattern::Ident("x".into()),
            body: Box::new(Expr::Lit(Lit::String("Hello, World!".into()))),
        }),
    );
    match ast {
        Item::Data(_) => todo!(),
        Item::Decl(_) => todo!(),
        Item::Expr(e) => {
            println!("{:?}", e);
            println!("{}", eval(env.clone(), &e).expect("failed to eval"));
        }
    }
}
