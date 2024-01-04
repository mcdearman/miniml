#[derive(Debug, Clone, PartialEq)]
pub struct CallFrame {
    pub ip: usize,
    pub code: Vec<u8>,
}
