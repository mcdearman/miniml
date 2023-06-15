use super::{ast::Map, cst::SyntaxKind};

#[derive(Debug, Clone, PartialEq)]
pub enum Event {
    Open { kind: SyntaxKind },
    Close,
    Advance,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MarkOpened {
    pub index: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MarkClosed {
    pub index: usize,
}