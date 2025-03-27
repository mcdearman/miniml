/// Pretty is a trait that allows for pretty printing of a type.
pub trait Pretty {
    /// Pretty print the type.
    fn pretty(&self) -> String;
}