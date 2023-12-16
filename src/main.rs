mod compiler;
mod lexer;
mod num;
mod object;
mod span;
mod token;

fn main() {
    env_logger::init();
    // let src = "0xff";
    // // let num = num::Num::from_str(src).unwrap();
    // let num = Rational64::from_str_radix(&src[2..], 16).expect("failed to parse");
    // // let num = i64::from_str_radix(&src[2..], 16).expect("failed to parse");
    // println!("{:?}", num);

    // open examples/notebook.mn
    let src = std::fs::read_to_string("examples/notebook.mn").unwrap();
    let mut comp = compiler::Compiler::new(&src);
    comp.run();
}
