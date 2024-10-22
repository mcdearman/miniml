use dbg_pls::DebugPls;
use std::{fmt::Display, str::FromStr};

#[derive(Debug, DebugPls, Clone, Copy, PartialEq)]
pub struct Rational {
    numerator: i64,
    denominator: i64,
}

impl Display for Rational {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}/{}", self.numerator, self.denominator)
    }
}

pub struct ParseRationalError;

impl FromStr for Rational {
    type Err = ParseRationalError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut parts = s.split('/');

        let numerator = parts
            .next()
            .ok_or(ParseRationalError)?
            .parse()
            .map_err(|_| ParseRationalError)?;

        let denominator = parts
            .next()
            .ok_or(ParseRationalError)?
            .parse()
            .map_err(|_| ParseRationalError)?;

        Ok(Self {
            numerator,
            denominator,
        })
    }
}
