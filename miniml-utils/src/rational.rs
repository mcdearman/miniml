use std::{fmt::Display, str::FromStr};

pub type Rational32 = Rational<i32, i32>;
pub type Rational64 = Rational<i64, i64>;

#[derive(Debug, Copy, Clone, PartialOrd, Ord, PartialEq, Eq)]
pub struct Rational<N, D> {
    numerator: N,
    denominator: D,
}

impl<N: Display, D: Display> Display for Rational<N, D> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}/{}", self.numerator, self.denominator)
    }
}

pub enum ParseRationalError<E, F> {
    ParseNumeratorError(E),
    ParseDenominatorError(F),
    TooManyParts,
}

impl<N: FromStr, D: FromStr> FromStr for Rational<N, D> {
    type Err = ParseRationalError<N::Err, D::Err>;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut parts = s.split('/');

        if parts.clone().count() > 2 {
            return Err(ParseRationalError::TooManyParts);
        }

        let numerator = parts
            .next()
            .expect("split count is 2")
            .parse()
            .map_err(|e| ParseRationalError::ParseNumeratorError(e))?;

        let denominator = parts
            .next()
            .expect("split count is 2")
            .parse()
            .map_err(|e| ParseRationalError::ParseDenominatorError(e))?;

        Ok(Self {
            numerator,
            denominator,
        })
    }
}
