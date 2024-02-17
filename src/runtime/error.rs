use crate::utils::intern::InternedString;
use std::fmt::Display;

#[derive(Debug, Clone, PartialEq)]
pub enum RuntimeError {
    ParseError(Vec<InternedString>),
    ResError(Vec<RuntimeError>),
    ArityError(usize, usize),
    TypeError(InternedString),
    UnboundIdent(InternedString),
    Overflow,
    DivisionByZero,
}

impl Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RuntimeError::ParseError(errs) => {
                write!(f, "Parse error:\n")?;
                for err in errs {
                    write!(f, "{}\n", err)?;
                }
                Ok(())
            }
            RuntimeError::ResError(errs) => {
                write!(f, "Resolve error:\n")?;
                for err in errs {
                    write!(f, "{}\n", err)?;
                }
                Ok(())
            }
            RuntimeError::ArityError(expected, found) => {
                write!(
                    f,
                    "Arity error: expected {} args, found {}",
                    expected, found
                )
            }
            RuntimeError::TypeError(err) => write!(f, "Type error: {}", err),
            RuntimeError::UnboundIdent(ident) => write!(f, "Unbound identifier: {}", ident),
            RuntimeError::Overflow => write!(f, "Overflow"),
            RuntimeError::DivisionByZero => write!(f, "Division by zero"),
        }
    }
}

pub type RuntimeResult<T> = std::result::Result<T, RuntimeError>;
