#[derive(Clone, PartialEq, Eq)]
pub enum Type {
    Num,
    Bool,
    String,
    Var(TyVar),
    Lambda(Vec<Self>, Box<Self>),
    Unit,
}

impl Type {
    fn lower(&self, vars: &mut HashMap<TyVar, TyVar>) -> Self {
        match self.clone() {
            Self::Num => Self::Num,
            Self::Bool => Self::Bool,
            Self::String => Self::String,
            Self::Var(name) => {
                if let Some(n) = vars.get(&name) {
                    Self::Var(*n)
                } else {
                    let n = vars.len();
                    vars.insert(name, TyVar(n.into()));
                    Self::Var(TyVar(n.into()))
                }
            }
            Self::Lambda(params, body) => {
                let mut lowered_params = vec![];
                for param in params {
                    lowered_params.push(param.lower(vars));
                }
                Self::Lambda(lowered_params, Box::new(body.lower(vars)))
            }
            Self::Unit => Self::Unit,
        }
    }
}

impl Debug for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.clone() {
            Self::Num => write!(f, "Num"),
            Self::Bool => write!(f, "Bool"),
            Self::String => write!(f, "String"),
            Self::Var(n) => write!(f, "{:?}", n),
            Self::Lambda(params, body) => write!(f, "{:?} -> {:?}", params, body),
            Self::Unit => write!(f, "()"),
        }
    }
}