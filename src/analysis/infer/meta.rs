use super::{
    error::{InferResult, TypeError},
    meta_context::MetaContext,
    r#type::Type,
};
use crate::utils::unique_id::UniqueId;
use std::fmt::{Debug, Display};

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Meta(UniqueId);

impl Meta {
    pub fn new(id: UniqueId) -> Self {
        Self(id)
    }

    pub fn fresh() -> Self {
        Self(UniqueId::gen())
    }

    pub fn id(&self) -> UniqueId {
        self.0
    }
}

impl Debug for Meta {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "v{}", self.0)
    }
}

#[rustfmt::skip]
const ALPHABET: &[char] = &[
    'a', 'b', 'c', 'd', 'e', 'f', 
    'g', 'h', 'i', 'j', 'k', 'l', 
    'm', 'n', 'o', 'p', 'q', 'r', 
    's', 't', 'u', 'v', 'w', 'x', 
    'y', 'z',
];

impl Display for Meta {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let id = usize::from(self.0);
        if id < ALPHABET.len() {
            write!(f, "{}", ALPHABET[id])
        } else {
            write!(
                f,
                "{}{}",
                ALPHABET[id / ALPHABET.len() - 1],
                (id + 1) % ALPHABET.len()
            )
        }
    }
}
