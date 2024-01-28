use super::{expr::Expr, ident::Ident, pattern::Pattern};

#[derive(Debug, Clone, PartialEq)]
pub struct LetArm {
    name: Ident,
    pattern: Pattern,
    expr: Expr,
}

impl LetArm {
    pub fn new(name: Ident, pattern: Pattern, expr: Expr) -> Self {
        Self { name, pattern, expr }
    }

    pub fn name(&self) -> &Ident {
        &self.name
    }

    pub fn pattern(&self) -> &Pattern {
        &self.pattern
    }

    pub fn expr(&self) -> &Expr {
        &self.expr
    }
}
