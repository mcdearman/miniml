#[derive(Debug, Clone, PartialEq)]
enum Constraint {
    Eq(Type, Type),
}
