#[derive(Debug, Clone, PartialEq)]
pub enum Kind {
    Star,
    Arrow(Box<Self>, Box<Self>),
}