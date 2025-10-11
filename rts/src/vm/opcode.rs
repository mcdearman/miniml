#[derive(Debug, PartialEq, Eq)]
pub enum Opcode {
    Unwind,
    PushGlobal(String),
    PushInt(i64),
    Push(usize),
    Mkap,
    Slide(usize),
}
