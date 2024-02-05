use crate::utils::intern::InternedString;
use std::fmt::Display;

#[derive(Debug, Clone, PartialEq)]
pub enum Error {
    ParseError(Vec<InternedString>),
    ResError(Vec<Error>),
    ArityError(usize, usize),
    TypeError(InternedString),
    DivisionByZero,
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::ParseError(errs) => {
                write!(f, "Parse error:\n")?;
                for err in errs {
                    write!(f, "{}\n", err)?;
                }
                Ok(())
            }
            Error::ResError(errs) => {
                write!(f, "Resolve error:\n")?;
                for err in errs {
                    write!(f, "{}\n", err)?;
                }
                Ok(())
            }
            Error::ArityError(expected, found) => {
                write!(
                    f,
                    "Arity error: expected {} args, found {}",
                    expected, found
                )
            }
            Error::TypeError(err) => write!(f, "Type error: {}", err),
            Error::DivisionByZero => write!(f, "Division by zero"),
        }
    }
}

pub type Result<T> = std::result::Result<T, Error>;
