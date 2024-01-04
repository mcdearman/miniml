use num_rational::Rational64;
use std::error::Error;
use std::fmt::Debug;
use std::{fmt::Display, str::FromStr};

#[derive(Clone, Copy, PartialEq)]
pub struct Num(pub Rational64);

impl Display for Num {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Debug for Num {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl FromStr for Num {
    type Err = num_rational::ParseRatioError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        // let nums = s.split("/");
        // if nums.clone().count() == 2 {
        //     let numer = nums.clone().nth(0).unwrap();
        //     let denom = nums.clone().nth(1).unwrap();
        //     match 
        // } else {
        //     Ok(Self(Rational64::from_str(s)?))
        // }
        Rational64::from_str(s).map(Self)
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ParseRatioError;

impl ParseRatioError {
    pub fn desc(&self) -> &str {
        "failed to parse ratio"
    }
}

impl Display for ParseRatioError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.desc())
    }
}

impl Error for ParseRatioError {
    fn description(&self) -> &str {
        "failed to parse ratio"
    }
}
